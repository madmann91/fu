#include "fu/lang/check.h"
#include "fu/lang/type_table.h"
#include "fu/core/alloc.h"
#include "fu/core/hash.h"
#include "fu/core/mem_pool.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#define DEFAULT_DECL_CAPACITY 8

TypingContext new_typing_context(TypeTable* type_table, MemPool* mem_pool, Log* log) {
    return (TypingContext) {
        .log = log,
        .type_table = type_table,
        .mem_pool = mem_pool,
        .visited_decls = new_hash_table(DEFAULT_DECL_CAPACITY, sizeof(AstNode*))
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

static bool is_assignable(AstNode* expr) {
    assert(expr->type && "expression must have been type-checked first");
    // TODO
    return true;
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
    AstNode* elems,
    size_t count,
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
    if (!matches_type && !expected_type->contains_error && !type->contains_error) {
        log_error(context->log, file_loc, "expected {s} type '{t}', but got type '{t}'",
            (FormatArg[]) {
                { .s = is_upper_bound ? "at most" : "at least" },
                { .t = expected_type },
                { .t = type }
            });
        return make_error_type(context->type_table);
    }
    return type;
}

static const Type* expect_assignable(TypingContext* context, AstNode* expr) {
    if (!is_assignable(expr) && !expr->type->contains_error) {
        log_error(context->log, &expr->file_loc,
            "expression with type '{t}' cannot be written to",
            (FormatArg[]) { { .t = expr->type } });
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

static const Type* infer_path(TypingContext* context, AstNode* path) {
    // TODO: Infer the rest of the path
    return infer_decl_site(context, path->path.decl_site);
}

static const Type* infer_tuple(
    TypingContext* context,
    AstNode* tuple,
    const Type* (*infer_arg)(TypingContext*, AstNode*))
{
    size_t arg_count = get_ast_list_length(tuple->tuple_expr.args);
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

    size_t arg_count = get_ast_list_length(tuple->tuple_expr.args);
    if (expected_type->tuple_type.arg_count != arg_count)
    {
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
#define f(name, ...) case AST_TYPE_##name: return make_prim_type(context->type_table, TYPE_##name);
        AST_PRIM_TYPE_LIST(f)
#undef f
        case AST_NORET_TYPE:
            if (!accept_noret)
                log_error(context->log, &type->file_loc, "type '!' can only be used as a return type", NULL);
            return make_noret_type(context->type_table);
        case AST_TUPLE_TYPE:
            return infer_tuple(context, type, infer_type);
        case AST_FUN_TYPE:
            return make_fun_type(context->type_table,
                infer_type(context, type->fun_type.dom_type),
                infer_type_with_noret(context, type->fun_type.codom_type, true));
        default:
            assert(false && "invalid type");
            return make_error_type(context->type_table);
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
    const char* msg,
    const Type* callee_type,
    const Type* expected_type)
{
    if (callee_type->tag != TYPE_FUN) {
        log_error(context->log, &call_or_op->file_loc, "expected function type in {s}, but got type '{t}'",
            (FormatArg[]) { { .s = msg }, { .t = callee_type } });
        return make_error_type(context->type_table);
    }
    assert(callee_type->tag == TYPE_FUN);
    check_expr(context, arg, callee_type->fun_type.dom);
    return expect_type(context,
        callee_type->fun_type.codom, expected_type, true, &call_or_op->file_loc);
}

static const Type* check_call_expr(TypingContext* context, AstNode* call_expr, const Type* expected_type) {
    const Type* callee_type = infer_expr(context, call_expr->call_expr.callee);
    return call_expr->type = check_call(context,
        call_expr, call_expr->call_expr.arg, "function call", callee_type, expected_type);
}

static const Type* check_int_literal(TypingContext* context, AstNode* literal, const Type* expected_type) {
    if (expected_type->tag == TYPE_UNKNOWN)
        return literal->type = make_prim_type(context->type_table, TYPE_I64);
    if (!is_int_or_float_type(expected_type->tag))
        return literal->type = fail_expect(context, "integer literal", expected_type, &literal->file_loc);
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

const Type* check_expr(TypingContext* context, AstNode* expr, const Type* expected_type) {
    switch (expr->tag) {
        case AST_PATH:
            return expr->type = expect_type(context,
                infer_path(context, expr), expected_type, true, &expr->file_loc);
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
            return expect_type(context, type, expected_type, true, &expr->file_loc);
       }
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
            return expect_type(context, infer_logic_expr(context, expr), expected_type, true, &expr->file_loc);
#undef f
#undef g
        default:
            assert(false && "invalid expression");
            return make_error_type(context->type_table);
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
        case AST_PATH:
            if (!pattern->path.decl_site) {
                if (expected_type->contains_unknown)
                    return pattern->type = fail_infer(context, "pattern", &pattern->file_loc);
                return pattern->type = expected_type;
            }
            return pattern->type = infer_path(context, pattern);
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
        size_t arg_count = get_ast_list_length(pattern->tuple_pattern.args);
        if (arg_count == get_ast_list_length(expr->tuple_expr.args) &&
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

static const Type* infer_struct_decl(TypingContext* context, AstNode* struct_decl) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* infer_enum_decl(TypingContext* context, AstNode* enum_decl) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* infer_type_decl(TypingContext* context, AstNode* type_decl) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* infer_fun_decl(TypingContext* context, AstNode* fun_decl) {
    const Type* dom_type = infer_pattern(context, fun_decl->fun_decl.param);
    if (fun_decl->fun_decl.ret_type) {
        const Type* codom_type = infer_type(context, fun_decl->fun_decl.ret_type);
        // Set the function type now in case the function is recursive
        fun_decl->type = make_fun_type(context->type_table, dom_type, codom_type);
        check_expr(context, fun_decl->fun_decl.body, codom_type);
        return fun_decl->type;
    }
    const Type* codom_type = infer_expr(context, fun_decl->fun_decl.body);
    return fun_decl->type = make_fun_type(context->type_table, dom_type, codom_type);
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
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            return infer_const_or_var_decl(context, decl);
        default:
            assert(false && "invalid declaration");
            return make_error_type(context->type_table);
    }
}

void infer_program(TypingContext* context, AstNode* program) {
    for (AstNode* decl = program->program.decls; decl; decl = decl->next)
        infer_decl(context, decl);
}
