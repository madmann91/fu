#include "fu/lang/check.h"
#include "fu/lang/type_table.h"
#include "fu/core/alloc.h"
#include "fu/core/hash.h"
#include "fu/core/mem_pool.h"
#include "fu/core/dyn_array.h"
#include "fu/core/utils.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

TypingContext new_typing_context(TypeTable* type_table, MemPool* mem_pool, Log* log) {
    return (TypingContext) {
        .log = log,
        .type_table = type_table,
        .mem_pool = mem_pool,
        .visited_decls = new_hash_table(sizeof(AstNode*))
    };
}

void free_typing_context(TypingContext* context) {
    free_hash_table(&context->visited_decls);
}

static bool compare_decls(const void* left, const void* right) {
    return !memcmp(left, right, sizeof(AstNode*));
}

static bool push_decl(TypingContext* context, AstNode* decl) {
    return insert_in_hash_table(&context->visited_decls,
        &decl, hash_ptr(hash_init(), decl), sizeof(AstNode*), compare_decls);
}

static void pop_decl(TypingContext* context, AstNode* decl) {
    const AstNode** ptr = find_in_hash_table(&context->visited_decls,
        &decl, hash_ptr(hash_init(), decl), sizeof(AstNode*), compare_decls);
    assert(ptr && "trying to pop an unvisited declaration");
    remove_from_hash_table(&context->visited_decls, ptr, sizeof(AstNode*));
}

static const Type** check_many(
    TypingContext* context,
    AstNode* elems,
    const Type** expected_types,
    size_t count,
    const Type* (*check_one)(TypingContext*, AstNode*, const Type*))
{
    const Type** types = malloc_or_die(sizeof(Type*) * count);
    for (size_t i = 0; i < count; ++i, elems = elems->next)
        types[i] = check_one(context, elems, expected_types[i]);
    return types;
}

static const Type** infer_many(
    TypingContext* context,
    AstNode* elems, size_t count,
    const Type* (*infer_one)(TypingContext*, AstNode*))
{
    const Type** types = malloc_or_die(sizeof(Type*) * count);
    for (size_t i = 0; i < count; ++i, elems = elems->next)
        types[i] = infer_one(context, elems);
    return types;
}

static const Type* expect_type(
    TypingContext* context,
    const Type* type,
    const Type* expected_type,
    bool is_upper_bound,
    const FileLoc* file_loc)
{
    bool matches_type = is_upper_bound ? is_subtype(type, expected_type) : is_subtype(expected_type, type);
    if (!matches_type) {
        if (!expected_type->contains_error && !type->contains_error) {
            log_error(context->log, file_loc, "expected {s} type '{t}', but got type '{t}'",
                (FormatArg[]) {
                    { .s = is_upper_bound ? "at most" : "at least" },
                    { .t = expected_type },
                    { .t = type }
                });
        }
        return make_error_type(context->type_table);
    }
    return type;
}

static bool expect_type_with_tag(
    TypingContext* context,
    const Type* type,
    TypeTag tag,
    const char* msg,
    const FileLoc* file_loc)
{
    if (type->tag != tag) {
        if (!type->contains_error) {
            log_error(context->log, file_loc,
                "expected {s} type, but got '{t}'",
                (FormatArg[]) { { .s = msg }, { .t = type } });
        }
        return false;
    }
    return true;
}

static const Type* expect_assignable(TypingContext* context, AstNode* expr) {
    if (!is_assignable_expr(expr)) {
        if (!expr->type->contains_error) {
            log_error(context->log, &expr->file_loc,
                "expression cannot be written to",
                (FormatArg[]) { { .t = expr->type } });
            if (expr->tag == AST_PATH &&
                expr->path.decl_site &&
                expr->path.decl_site->tag == AST_IDENT_PATTERN &&
                expr->path.decl_site->ident_pattern.is_const)
            {
                log_note(context->log, &expr->path.decl_site->file_loc,
                    "'{s}' is declared as a constant here",
                    (FormatArg[]) { { .s = expr->path.decl_site->ident_pattern.name } });
            }
        }
        return make_error_type(context->type_table);
    }
    return expr->type;
}

static const Type* fail_expect(
    TypingContext* context,
    const char* msg,
    const Type* type,
    const FileLoc* file_loc)
{
    if (!type->contains_error) {
        log_error(context->log, file_loc,
            "expected type '{t}', but got {s}",
            (FormatArg[]) { { .t = type }, { .s = msg } });
    }
    return make_error_type(context->type_table);
}

static const Type* fail_infer(TypingContext* context, const char* msg, const FileLoc* file_loc) {
    log_error(context->log, file_loc, "cannot infer type for {s}", (FormatArg[]) { { .s = msg } });
    return make_error_type(context->type_table);
}

static const Type* fail_member_access(TypingContext* context, const char* name, const Type* type, const FileLoc* file_loc) {
    log_error(context->log, file_loc, "no member '{s}' in type '{t}'", (FormatArg[]) { { .s = name }, { .t = type } });
    return make_error_type(context->type_table);
}

static const Type* infer_decl_site(TypingContext* context, AstNode* decl_site) {
    if (!decl_site->type) {
        if (!push_decl(context, decl_site)) {
            log_error(context->log, &decl_site->file_loc, "cannot infer type for recursive declaration", NULL);
            if (decl_site->tag == AST_FUN_DECL)
                log_note(context->log, NULL, "adding a return type annotation may fix the problem", NULL);
            return decl_site->type = make_error_type(context->type_table);
        }
        infer_decl(context, decl_site);
        pop_decl(context, decl_site);
    }
    return decl_site->type;
}

static const Type* check_type_args(
    TypingContext* context,
    const Type* type,
    AstNode* type_args,
    const FileLoc* file_loc)
{
    size_t type_param_count = get_type_param_count(type);
    if (!type_args) {
        if (type_param_count != 0) {
            log_error(context->log, file_loc,
                "missing type arguments for type '{t}'", (FormatArg[]) { { .t = type } });
            return make_error_type(context->type_table);
        }
        return type;
    } else if (type_param_count == 0) {
        log_error(context->log, file_loc,
            "type arguments are not allowed on type '{t}'", (FormatArg[]) { { .t = type } });
        return make_error_type(context->type_table);
    } else if (count_ast_nodes(type_args) != type_param_count) {
        log_error(context->log, file_loc,
            "expected {0:u} type argument(s), but got {1:u}",
            (FormatArg[]) { { .u = type_param_count }, { .u = count_ast_nodes(type_args) } });
        return make_error_type(context->type_table);
    }

    const Type** args = new_dyn_array(sizeof(Type*));
    for (AstNode* type_arg = type_args; type_arg; type_arg = type_arg->next) {
        const Type* arg = infer_type(context, type_arg);
        push_on_dyn_array(args, &arg);
    }

    const Type* applied_type = make_type_app(context->type_table, type, args, type_param_count);
    free_dyn_array(args);
    return applied_type;
}

static const Type* infer_next_path_elem(TypingContext* context, AstNode* path_elem) {
    AstNode* next_elem = path_elem->next;
    const Type* inner_type = skip_type_app(path_elem->type);
    if (inner_type->tag == TYPE_STRUCT) {
        const StructField* field = find_struct_field(inner_type, next_elem->path_elem.name);
        if (!field) goto fail;
        next_elem->path_elem.index = field - inner_type->struct_type.fields;
        next_elem->path_elem.is_type = false;
        next_elem->type = field->type;
    } else if (inner_type->tag == TYPE_ENUM) {
        const EnumOption* option = find_enum_option(inner_type, next_elem->path_elem.name);
        if (!option) goto fail;
        next_elem->path_elem.index = option - inner_type->enum_type.options;
        next_elem->path_elem.is_type = is_struct_like_option(option);
        next_elem->type = option->param_type;
    } else if (inner_type->tag == TYPE_SIGNATURE) {
        const SignatureMember* member = find_signature_member(inner_type, next_elem->path_elem.name);
        if (!member) goto fail;
        next_elem->path_elem.index = member - inner_type->signature.members;
        next_elem->path_elem.is_type = member->is_type;
        next_elem->type = member->type;
    }

    return check_type_args(context,
        next_elem->type,
        next_elem->path_elem.type_args,
        &next_elem->file_loc);

fail:
    return fail_member_access(context, next_elem->path_elem.name, inner_type, &next_elem->file_loc);
}

static const Type* infer_path(TypingContext* context, AstNode* path, bool is_type_expected) {
    assert(path->path.elems);
    if (!path->path.decl_site) {
        // This error should have been reported by the name binding algorithm
        return path->type = make_error_type(context->type_table);
    }

    AstNode* path_elem = path->path.elems;
    path_elem->type = infer_decl_site(context, path->path.decl_site);
    path_elem->path_elem.is_type =
        path->path.decl_site->tag == AST_STRUCT_DECL ||
        path->path.decl_site->tag == AST_ENUM_DECL ||
        path->path.decl_site->tag == AST_TYPE_DECL ||
        path->path.decl_site->tag == AST_TYPE_PARAM ||
        path->path.decl_site->tag == AST_SIG_DECL;
    path_elem->type = check_type_args(context,
        path_elem->type,
        path_elem->path_elem.type_args,
        &path_elem->file_loc);

    while (path_elem->next) {
        infer_next_path_elem(context, path_elem);
        path_elem = path_elem->next;
    }

    if (path_elem->path_elem.is_type != is_type_expected) {
        log_error(context->log, &path->file_loc,
            is_type_expected
                ? "expected type, but got value named '{s}'"
                : "expected value, but got type named '{s}'",
            (FormatArg[]) { { .s = path_elem->path_elem.name } });
        return path->type = make_error_type(context->type_table);
    }
    return path->type = path_elem->type;
}

static const Type* infer_tuple(
    TypingContext* context,
    AstNode* tuple,
    const Type* (*infer_arg)(TypingContext*, AstNode*))
{
    size_t arg_count = count_ast_nodes(tuple->tuple_expr.args);
    const Type** arg_types = infer_many(context, tuple->tuple_expr.args, arg_count, infer_arg);
    tuple->type = make_tuple_type(context->type_table, arg_types, arg_count);
    free(arg_types);
    return tuple->type;
}

static const Type* check_tuple(
    TypingContext* context,
    AstNode* tuple,
    const Type* expected_type,
    const Type* (*check_arg)(TypingContext*, AstNode*, const Type*))
{
    assert(expected_type->tag == TYPE_TUPLE);

    size_t arg_count = count_ast_nodes(tuple->tuple_expr.args);
    if (expected_type->tuple_type.arg_count != arg_count) {
        log_error(context->log, &tuple->file_loc,
            "expected tuple with {u} argument(s), but got {u}",
            (FormatArg[]) { { .u = expected_type->tuple_type.arg_count }, { .u = arg_count } });
        return make_error_type(context->type_table);
    }

    const Type** arg_types = check_many(context,
        tuple->tuple_expr.args,
        expected_type->tuple_type.args,
        arg_count, check_arg);
    tuple->type = make_tuple_type(context->type_table, arg_types, arg_count);
    free(arg_types);
    return tuple->type;
}

static const Type* infer_type_with_noret(TypingContext* context, AstNode* type, bool accept_noret) {
    switch (type->tag) {
#define f(name, ...) case AST_TYPE_##name: return type->type = make_prim_type(context->type_table, TYPE_##name);
        PRIM_TYPE_LIST(f)
#undef f
        case AST_PATH:
            return type->type = infer_path(context, type, true);
        case AST_NORET_TYPE:
            if (!accept_noret)
                log_error(context->log, &type->file_loc, "type '!' can only be used as a return type", NULL);
            return type->type = make_noret_type(context->type_table);
        case AST_TUPLE_TYPE:
            return infer_tuple(context, type, infer_type);
        case AST_FUN_TYPE:
            return type->type = make_fun_type(context->type_table,
                infer_type(context, type->fun_type.dom_type),
                infer_type_with_noret(context, type->fun_type.codom_type, true));
        case AST_SIG_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
            return infer_decl(context, type);
        default:
            assert(false && "invalid type");
            return type->type = make_error_type(context->type_table);
    }
}

const Type* infer_type(TypingContext* context, AstNode* type) {
    return infer_type_with_noret(context, type, false);
}

static const Type* check_cond(TypingContext* context, AstNode* cond) {
    return check_expr(context, cond, make_prim_type(context->type_table, TYPE_BOOL));
}

static const Type* check_if_expr(TypingContext* context, AstNode* if_expr, const Type* expected_type) {
    check_cond(context, if_expr->if_expr.cond);
    const Type* then_type = check_expr(context, if_expr->if_expr.then_expr, expected_type);
    if (if_expr->if_expr.else_expr) {
        const Type* else_type = check_expr(context, if_expr->if_expr.else_expr, expected_type);
        if (is_subtype(then_type, else_type))
            return if_expr->type = else_type;
        return if_expr->type = expect_type(
            context, else_type, then_type, true, &if_expr->if_expr.else_expr->file_loc);
    }
    return if_expr->type = then_type;
}

static const Type* check_fun_expr(
    TypingContext* context,
    AstNode* fun_expr,
    const Type* dom_type,
    const Type* codom_type)
{
    dom_type = check_pattern(context, fun_expr->fun_expr.param, dom_type);
    if (fun_expr->fun_expr.ret_type) {
        codom_type = expect_type(context,
            infer_type(context, fun_expr->fun_expr.ret_type),
            codom_type, true, &fun_expr->fun_expr.ret_type->file_loc);
    }
    const Type* body_type = check_expr(context, fun_expr->fun_expr.body, codom_type);
    if (!fun_expr->fun_expr.ret_type)
        codom_type = body_type;
    return fun_expr->type = make_fun_type(context->type_table, dom_type, codom_type);
}

static const Type* check_call(
    TypingContext* context,
    AstNode* call_or_op,
    AstNode* arg,
    const Type* callee_type,
    const Type* expected_type)
{
    if (!expect_type_with_tag(context, callee_type, TYPE_FUN, "function", &call_or_op->file_loc))
        return make_error_type(context->type_table);
    assert(callee_type->tag == TYPE_FUN);
    check_expr(context, arg, callee_type->fun_type.dom);
    return expect_type(context,
        callee_type->fun_type.codom, expected_type, true, &call_or_op->file_loc);
}

static const Type* check_call_expr(TypingContext* context, AstNode* call_expr, const Type* expected_type) {
    const Type* callee_type = infer_expr(context, call_expr->call_expr.callee);
    return call_expr->type = check_call(context, call_expr, call_expr->call_expr.arg, callee_type, expected_type);
}

static const Type* check_int_literal(TypingContext* context, AstNode* literal, const Type* expected_type) {
    if (expected_type->tag == TYPE_UNKNOWN)
        return literal->type = make_prim_type(context->type_table, TYPE_I64);
    if (!is_int_or_float_type(expected_type->tag))
        return literal->type = fail_expect(context, "integer literal", expected_type, &literal->file_loc);
    if (is_int_type(expected_type->tag) &&
        ilog2(literal->int_literal.val) > get_prim_type_bitwidth(expected_type->tag))
    {
        log_error(context->log, &literal->file_loc,
            "integer literal does not fit in type '{t}'",
            (FormatArg[]) { { .t = expected_type } });
        return literal->type = make_error_type(context->type_table);
    }
    return literal->type = expected_type;
}

static const Type* check_float_literal(TypingContext* context, AstNode* literal, const Type* expected_type) {
    if (expected_type->tag == TYPE_UNKNOWN)
        return literal->type = make_prim_type(context->type_table, TYPE_F64);
    if (!is_float_type(expected_type->tag))
        return literal->type = fail_expect(context, "floating-point literal", expected_type, &literal->file_loc);
    return literal->type = expected_type;
}

static const Type* check_binary_expr(TypingContext* context, AstNode* binary_expr, const Type* expected_type) {
    infer_expr(context, binary_expr->binary_expr.left);
    infer_expr(context, binary_expr->binary_expr.right);
    if (is_assign_expr(binary_expr->tag))
        expect_assignable(context, binary_expr->binary_expr.left);
    // TODO
    return NULL;
}

static const Type* infer_logic_expr(TypingContext* context, AstNode* logic_expr) {
    const Type* bool_type = make_prim_type(context->type_table, TYPE_BOOL);
    check_expr(context, logic_expr->binary_expr.left, bool_type);
    check_expr(context, logic_expr->binary_expr.right, bool_type);
    return bool_type;
}

static const Type* check_struct_expr(TypingContext* context, AstNode* struct_expr, const Type* expected_type) {
    const Type* type_app = infer_type(context, struct_expr->struct_expr.left);
    const Type* struct_type = skip_type_app(type_app);
    if (!expect_type_with_tag(context, struct_type, TYPE_STRUCT, "structure", &struct_expr->file_loc))
        return struct_expr->type = make_error_type(context->type_table);

    bool* seen = calloc_or_die(struct_type->struct_type.field_count, sizeof(bool));
    for (AstNode* field_expr = struct_expr->struct_expr.fields; field_expr; field_expr = field_expr->next) {
        const StructField* field = find_struct_field(struct_type, field_expr->field_expr.name);
        if (!field) {
            fail_member_access(context, field_expr->field_expr.name, struct_type, &field_expr->file_loc);
            goto fail;
        }

        // Check that this field is not initialized twice
        size_t field_index = field - struct_type->struct_type.fields;
        if (seen[field_index]) {
            log_error(context->log, &field_expr->file_loc,
                "field '{s}' is specified more than once",
                (FormatArg[]) { { .s = field_expr->field_expr.name } });
            goto fail;
        }
        seen[field_index] = true;
        field_expr->field_expr.index = field_index;

        // Check that the value of the field matches its type
        const Type* field_type = field->type;
        if (type_app) {
            field_type = replace_types(context->type_table,
                field->type,
                struct_type->struct_type.type_params,
                type_app->type_app.args,
                struct_type->struct_type.type_param_count);
        }
        check_expr(context, field_expr->field_expr.val, field_type);
    }

    // Check that all fields have an initializer
    for (size_t i = 0; i < struct_type->struct_type.field_count; ++i) {
        if (!seen[i] && !struct_type->struct_type.fields[i].has_default) {
            log_error(context->log, &struct_expr->file_loc,
                "missing initializer for field '{s}'",
                (FormatArg[]) { { .s = struct_type->struct_type.fields[i].name } });
        }
    }

    const Type* result_type = struct_type;
    if (struct_type->struct_type.parent_enum) {
        // If the struct expression constructs an enumeration type,
        // return that instead of the structure type.
        if (type_app->tag == TYPE_APP) {
            result_type = make_type_app(
                context->type_table,
                struct_type->struct_type.parent_enum,
                type_app->type_app.args,
                type_app->type_app.arg_count);
        } else
            result_type = struct_type->struct_type.parent_enum;
    }
    struct_expr->type = expect_type(context, result_type, expected_type, true, &struct_expr->file_loc);
    goto cleanup;

fail:
    struct_expr->type = make_error_type(context->type_table);
cleanup:
    free(seen);
    return struct_expr->type;
}

const Type* check_expr(TypingContext* context, AstNode* expr, const Type* expected_type) {
    switch (expr->tag) {
        case AST_PATH:
            return expr->type = expect_type(context,
                infer_path(context, expr, false), expected_type, true, &expr->file_loc);
        case AST_INT_LITERAL:
            return check_int_literal(context, expr, expected_type);
        case AST_FLOAT_LITERAL:
            return check_float_literal(context, expr, expected_type);
        case AST_BOOL_LITERAL:
            return expr->type = expect_type(context,
                make_prim_type(context->type_table, TYPE_BOOL), expected_type, true, &expr->file_loc);
        case AST_CHAR_LITERAL:
            return expr->type = expect_type(context,
                make_prim_type(context->type_table, TYPE_U8), expected_type, true, &expr->file_loc);
        case AST_STR_LITERAL:
            return expr->type = expect_type(context,
                make_array_type(context->type_table, make_prim_type(context->type_table, TYPE_U8)),
                expected_type, true, &expr->file_loc);
        case AST_TYPED_EXPR:
            return expr->type = expect_type(context,
                check_expr(context, expr->typed_expr.left, infer_type(context, expr->typed_expr.type)),
                expected_type, true, &expr->file_loc);
        case AST_TUPLE_EXPR:
            if (expected_type->tag == TYPE_UNKNOWN)
                return infer_tuple(context, expr, infer_expr);
            if (expected_type->tag == TYPE_TUPLE)
                return check_tuple(context, expr, expected_type, check_expr);
            return expr->type = fail_expect(context, "tuple expression", expected_type, &expr->file_loc);
        case AST_IF_EXPR:
            return check_if_expr(context, expr, expected_type);
        case AST_CALL_EXPR:
            return check_call_expr(context, expr, expected_type);
        case AST_BLOCK_EXPR: {
            const Type* last_type = NULL;
            for (AstNode* stmt = expr->block_expr.stmts; stmt; stmt = stmt->next) {
                last_type = stmt->next || expr->block_expr.ends_with_semicolon
                    ? infer_stmt(context, stmt) : check_stmt(context, stmt, expected_type);
            }
            if (expr->block_expr.ends_with_semicolon || !expr->block_expr.stmts)
                last_type = make_unit_type(context->type_table);
            return expr->type = expect_type(context, last_type, expected_type, true, &expr->file_loc);
        }
        case AST_FUN_EXPR: {
            if (expected_type->tag == TYPE_UNKNOWN)
            return check_fun_expr(context, expr,
                make_unknown_type(context->type_table),
                make_unknown_type(context->type_table));
            if (expected_type->tag != TYPE_FUN)
                return expr->type = fail_expect(context, "function expression", expected_type, &expr->file_loc);
            return check_fun_expr(context, expr,
                expected_type->fun_type.dom,
                expected_type->fun_type.codom);
        }
        case AST_BREAK_EXPR:
        case AST_CONTINUE_EXPR: {
            const Type* type = make_fun_type(context->type_table,
                make_unit_type(context->type_table),
                make_noret_type(context->type_table));
            return expr->type = expect_type(context, type, expected_type, true, &expr->file_loc);
        }
        case AST_STRUCT_EXPR:
            return check_struct_expr(context, expr, expected_type);
#define f(name, ...) case AST_##name##_EXPR:
#define g(name, ...) case AST_##name##_ASSIGN_EXPR:
        AST_ASSIGN_EXPR_LIST(g)
        case AST_ASSIGN_EXPR:
            // fallthrough
        AST_ARITH_EXPR_LIST(f)
        AST_BIT_EXPR_LIST(f)
        AST_SHIFT_EXPR_LIST(f)
        AST_CMP_EXPR_LIST(f)
            return check_binary_expr(context, expr, make_unknown_type(context->type_table));
        AST_LOGIC_EXPR_LIST(f)
            return expr->type = expect_type(context, infer_logic_expr(context, expr), expected_type, true, &expr->file_loc);
#undef f
#undef g
        default:
            assert(false && "invalid expression");
            return expr->type = make_error_type(context->type_table);
    }
}

const Type* infer_expr(TypingContext* context, AstNode* expr) {
    return check_expr(context, expr, make_unknown_type(context->type_table));
}

const Type* check_pattern(TypingContext* context, AstNode* pattern, const Type* expected_type) {
    switch (pattern->tag) {
        case AST_INT_LITERAL:
            return check_int_literal(context, pattern, expected_type);
        case AST_FLOAT_LITERAL:
            return check_float_literal(context, pattern, expected_type);
        case AST_BOOL_LITERAL:
            return pattern->type = expect_type(context,
                make_prim_type(context->type_table, TYPE_BOOL), expected_type, false, &pattern->file_loc);
        case AST_CHAR_LITERAL:
            return pattern->type = expect_type(context,
                make_prim_type(context->type_table, TYPE_U8), expected_type, false, &pattern->file_loc);
        case AST_STR_LITERAL:
            return pattern->type = expect_type(context,
                make_array_type(context->type_table, make_prim_type(context->type_table, TYPE_U8)),
                expected_type, false, &pattern->file_loc);
        case AST_TUPLE_PATTERN:
            if (expected_type->tag == TYPE_UNKNOWN)
                return pattern->type = infer_tuple(context, pattern, infer_pattern);
            if (expected_type->tag == TYPE_TUPLE)
                return check_tuple(context, pattern, expected_type, check_pattern);
            return fail_expect(context, "tuple pattern", expected_type, &pattern->file_loc);
        case AST_TYPED_PATTERN:
            pattern->type = check_pattern(context, pattern->typed_pattern.left, infer_type(context, pattern->typed_pattern.type));
            return pattern->type = expect_type(context, pattern->type, expected_type, false, &pattern->file_loc);
        case AST_IDENT_PATTERN:
            if (expected_type->contains_unknown)
                return pattern->type = fail_infer(context, "pattern", &pattern->file_loc);
            return pattern->type = expected_type;
        case AST_PATH:
            return infer_path(context, pattern, true);
        default:
            assert(false && "invalid pattern");
            return make_error_type(context->type_table);
    }
}

const Type* infer_pattern(TypingContext* context, AstNode* pattern) {
    return check_pattern(context, pattern, make_unknown_type(context->type_table));
}

const Type* check_stmt(TypingContext* context, AstNode* stmt, const Type* expected_type) {
    switch (stmt->tag) {
        case AST_VAR_DECL:
        case AST_CONST_DECL:
        case AST_TYPE_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
        case AST_FUN_DECL:
            infer_decl(context, stmt);
            return expect_type(context, make_unit_type(context->type_table), expected_type, true, &stmt->file_loc);
        case AST_WHILE_LOOP:
            check_cond(context, stmt->while_loop.cond);
            check_expr(context, stmt->while_loop.body, make_unit_type(context->type_table));
            return stmt->type = expect_type(context, make_unit_type(context->type_table), expected_type, true, &stmt->file_loc);
        default:
            return check_expr(context, stmt, expected_type);
    }
}

const Type* infer_stmt(TypingContext* context, AstNode* stmt) {
    return check_stmt(context, stmt, make_unknown_type(context->type_table));
}

static const Type* check_pattern_and_expr(
    TypingContext* context,
    AstNode* pattern,
    AstNode* expr,
    const Type* expected_type)
{
    if (pattern->tag == AST_TUPLE_PATTERN && expr->tag == AST_TUPLE_EXPR &&
        (expected_type->tag == TYPE_UNKNOWN || expected_type->tag == TYPE_TUPLE))
    {
        size_t arg_count = count_ast_nodes(pattern->tuple_pattern.args);
        if (arg_count == count_ast_nodes(expr->tuple_expr.args) &&
            (expected_type->tag != TYPE_TUPLE || expected_type->tuple_type.arg_count == arg_count))
        {
            AstNode* expr_arg = expr->tuple_expr.args;
            AstNode* pattern_arg = pattern->tuple_pattern.args;
            const Type** arg_types = malloc_or_die(sizeof(Type*) * arg_count);
            for (size_t i = 0; i < arg_count;
                ++i, expr_arg = expr_arg->next, pattern_arg = pattern_arg->next)
            {
                const Type* arg_type = expected_type->tag == TYPE_TUPLE
                    ? expected_type->tuple_type.args[i]
                    : make_unknown_type(context->type_table);
                arg_types[i] = check_pattern_and_expr(context, pattern_arg, expr_arg, arg_type);
            }
            const Type* result = make_tuple_type(context->type_table, arg_types, arg_count);
            free(arg_types);
            return result;
        }
    } else if (pattern->tag == AST_TYPED_PATTERN)
        return check_expr(context, expr, check_pattern(context, pattern, expected_type));

    return check_pattern(context, pattern, check_expr(context, expr, expected_type));
}

static const Type* infer_const_or_var_decl(TypingContext* context, AstNode* decl) {
    return check_pattern_and_expr(context,
        decl->var_decl.pattern,
        decl->var_decl.init,
        make_unknown_type(context->type_table));
}

static const Type* infer_type_param(TypingContext* context, AstNode* type_param) {
    return type_param->type = make_type_var(
        context->type_table,
        type_param->type_param.name, type_param->type_param.kind);
}

static void infer_type_params(TypingContext* context, AstNode* type_params, const Type** type_vars) {
    for (; type_params; type_params = type_params->next) {
        infer_type_param(context, type_params);
        push_on_dyn_array(type_vars, &type_params->type);
    }
}

static StructField infer_field_decl(TypingContext* context, AstNode* field_decl) {
    const Type* field_type = infer_type(context, field_decl->field_decl.type);
    if (field_decl->field_decl.val)
        check_expr(context, field_decl->field_decl.val, field_type);
    return (StructField) {
        .name = field_decl->field_decl.name,
        .type = field_type,
        .has_default = field_decl->field_decl.val
    };
}

static const Type* infer_struct_decl(TypingContext* context, AstNode* struct_decl) {
    Type* struct_type = make_struct_type(context->type_table, struct_decl->struct_decl.name);
    infer_type_params(context, struct_decl->struct_decl.type_params, struct_type->struct_type.type_params);
    struct_decl->type = struct_type;
    for (AstNode* field_decl = struct_decl->struct_decl.fields; field_decl; field_decl = field_decl->next) {
        StructField field = infer_field_decl(context, field_decl);
        push_on_dyn_array(struct_type->struct_type.fields, &field);
    }
    return freeze_struct_type(context->type_table, struct_type);
}

static const Type* infer_option_decl(TypingContext* context, AstNode* option_decl, Type* enum_type) {
    if (!option_decl->option_decl.param_type)
        return option_decl->type = make_unit_type(context->type_table);
    if (!option_decl->option_decl.is_struct_like)
        return option_decl->type = infer_type(context, option_decl->option_decl.param_type);

    Type* struct_type = make_struct_type(context->type_table, option_decl->option_decl.name);
    struct_type->struct_type.parent_enum = enum_type;
    for (AstNode* field_decl = option_decl->option_decl.param_type; field_decl; field_decl = field_decl->next) {
        StructField field = infer_field_decl(context, field_decl);
        push_on_dyn_array(struct_type->struct_type.fields, &field);
    }
    return option_decl->type = freeze_struct_type(context->type_table, struct_type);
}

static const Type* infer_enum_decl(TypingContext* context, AstNode* enum_decl) {
    Type* enum_type = make_enum_type(context->type_table, enum_decl->enum_decl.name);
    infer_type_params(context, enum_decl->enum_decl.type_params, enum_type->enum_type.type_params);

    enum_decl->type = enum_type;
    for (AstNode* option_decl = enum_decl->enum_decl.options; option_decl; option_decl = option_decl->next) {
        push_on_dyn_array(enum_type->enum_type.options, &(EnumOption) {
            .name = option_decl->option_decl.name,
            .param_type = infer_option_decl(context, option_decl, enum_type)
        });
    }
    return freeze_enum_type(context->type_table, enum_type);
}

static const Type* infer_type_decl(TypingContext* context, AstNode* type_decl) {
    const Type** type_params = new_dyn_array(sizeof(Type*));
    infer_type_params(context, type_decl->type_decl.type_params, type_params);
    const Type* aliased_type = infer_type(context, type_decl->type_decl.aliased_type);
    type_decl->type = make_type_alias(
        context->type_table,
        type_decl->type_decl.name,
        type_params, get_dyn_array_size(type_params),
        aliased_type);
    free_dyn_array(type_params);
    return type_decl->type;
}

static const Type* infer_fun_decl(TypingContext* context, AstNode* fun_decl) {
    const Type** type_params = new_dyn_array(sizeof(Type*));
    infer_type_params(context, fun_decl->fun_decl.type_params, type_params);
    const Type* dom_type = infer_pattern(context, fun_decl->fun_decl.param);
    if (fun_decl->fun_decl.ret_type) {
        const Type* codom_type = infer_type(context, fun_decl->fun_decl.ret_type);
        // Set the function type before checking the body in case
        // the function is recursive (or uses `return`).
        fun_decl->type = make_poly_fun_type(context->type_table,
            type_params, get_dyn_array_size(type_params),
            dom_type, codom_type);
        check_expr(context, fun_decl->fun_decl.body, codom_type);
    } else {
        const Type* codom_type = infer_expr(context, fun_decl->fun_decl.body);
        fun_decl->type = make_poly_fun_type(context->type_table,
            type_params, get_dyn_array_size(type_params),
            dom_type, codom_type);
    }
    free_dyn_array(type_params);
    return fun_decl->type;
}

static const Type* infer_mod_decl(TypingContext* context, AstNode* mod_decl) {
    SignatureMember* members = new_dyn_array(sizeof(SignatureMember));
    const Type** type_params = new_dyn_array(sizeof(Type*));

    infer_type_params(context, mod_decl->mod_decl.type_params, type_params);
    for (AstNode* decl = mod_decl->mod_decl.members; decl; decl = decl->next) {
        infer_decl(context, decl);
        if (decl->external_type) {
            bool is_type =
                decl->tag == AST_SIG_DECL ||
                decl->tag == AST_STRUCT_DECL ||
                decl->tag == AST_ENUM_DECL ||
                decl->tag == AST_TYPE_DECL;
            bool is_opaque = decl->tag == AST_TYPE_DECL && !decl->type_decl.aliased_type;
            push_on_dyn_array(members, &(SignatureMember) {
                .name = get_decl_name(decl),
                .type = is_opaque ? NULL : decl->type,
                .is_type = is_type,
            });
        }
    }

    mod_decl->type = make_signature_type(context->type_table,
        type_params, get_dyn_array_size(type_params),
        members, get_dyn_array_size(members));
    free_dyn_array(type_params);
    free_dyn_array(members);
    return mod_decl->type;
}

const Type* infer_decl(TypingContext* context, AstNode* decl) {
    switch (decl->tag) {
        case AST_FUN_DECL:
            return infer_fun_decl(context, decl);
        case AST_STRUCT_DECL:
            return infer_struct_decl(context, decl);
        case AST_ENUM_DECL:
            return infer_enum_decl(context, decl);
        case AST_TYPE_DECL:
            return infer_type_decl(context, decl);
        case AST_MOD_DECL:
            return infer_mod_decl(context, decl);
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            return infer_const_or_var_decl(context, decl);
        default:
            assert(false && "invalid declaration");
            return make_error_type(context->type_table);
    }
}

void infer_program(TypingContext* context, AstNode* program) {
    infer_mod_decl(context, program);
}
