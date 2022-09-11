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

#define DEFAULT_INT_TYPE_TAG   TYPE_I32
#define DEFAULT_FLOAT_TYPE_TAG TYPE_F32

struct SignatureVars { DynArray vars; };

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
    // Push a declaration in the context, so that recursion can
    // be stopped when we encounter it again.
    return insert_in_hash_table(&context->visited_decls,
        &decl, hash_ptr(hash_init(), decl), sizeof(AstNode*), compare_decls);
}

static void pop_decl(TypingContext* context, AstNode* decl) {
    const AstNode** ptr = find_in_hash_table(&context->visited_decls,
        &decl, hash_ptr(hash_init(), decl), sizeof(AstNode*), compare_decls);
    assert(ptr && "trying to pop an unvisited declaration");
    remove_from_hash_table(&context->visited_decls, ptr, sizeof(AstNode*));
}

static bool must_add_to_parent_sig_or_mod(const AstNode* ast_node) {
    return
        is_public_decl(ast_node) ||
        (ast_node->parent_scope && ast_node->parent_scope->tag == AST_SIG_DECL);
}

static const Type* add_to_parent_sig_or_mod(
    TypeTable* type_table,
    AstNode* ast_node,
    const Type* type_or_kind,
    const char* name,
    bool is_kind)
{
    assert(
        ast_node->parent_scope->tag == AST_MOD_DECL ||
        ast_node->parent_scope->tag == AST_SIG_DECL);
    SignatureVars* vars = ast_node->parent_scope->tag == AST_MOD_DECL
        ? ast_node->parent_scope->mod_decl.vars
        : ast_node->parent_scope->sig_decl.vars;
    assert(vars);
    const Type* var = is_kind
        ? make_var_type_with_kind(type_table, name, type_or_kind)
        : make_var_type_with_value(type_table, name, type_or_kind);
    push_on_dyn_array(&vars->vars, &var);
    return var;
}

static inline const Type* add_decl_to_parent_sig_or_mod(
    TypeTable* type_table,
    AstNode* ast_node,
    const Type* type)
{
    return add_to_parent_sig_or_mod(
        type_table, ast_node, type,
        get_decl_name(ast_node),
        is_value_decl(ast_node->tag));
}

static inline void add_idents_to_parent_mod(TypeTable* type_table, AstNode* pattern) {
    switch (pattern->tag) {
        case AST_IDENT_PATTERN:
            add_to_parent_sig_or_mod(type_table, pattern, pattern->type, pattern->ident_pattern.name, true);
            break;
        case AST_FIELD_PATTERN:
            add_idents_to_parent_mod(type_table, pattern->field_pattern.val);
            break;
        case AST_STRUCT_PATTERN:
            for (AstNode* field = pattern->struct_pattern.fields; field; field = field->next)
                add_idents_to_parent_mod(type_table, field);
            break;
        case AST_CTOR_PATTERN:
            add_idents_to_parent_mod(type_table, pattern->ctor_pattern.arg);
            break;
        case AST_TUPLE_PATTERN:
            for (AstNode* arg = pattern->tuple_pattern.args; arg; arg = arg->next)
                add_idents_to_parent_mod(type_table, arg);
            break;
        case AST_TYPED_PATTERN:
            add_idents_to_parent_mod(type_table, pattern->typed_pattern.left);
            break;
        case AST_ARRAY_PATTERN:
            for (AstNode* elem = pattern->array_pattern.elems; elem; elem = elem->next)
                add_idents_to_parent_mod(type_table, elem);
            break;
        default:
            assert(false && "invalid pattern");
            break;
    }
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

static void show_resolved_types(TypingContext* context, const Type** types, size_t count) {
    for (size_t i = 0, j = 0; i < count; ++i) {
        const Type* resolved_type = resolve_type(types[i]);
        if (types[i] != resolved_type) {
            if (j++ == 0)
                log_note(context->log, NULL, "where", NULL);
            log_note(context->log, NULL, " '{t}' is '{t}'",
                (FormatArg[]) { { .t = types[i] }, { .t = resolved_type } });
        }
    }
}

static const Type* expect_type(
    TypingContext* context,
    const Type* type,
    const Type* expected_type,
    bool is_upper_bound,
    const FileLoc* file_loc)
{
    bool matches_type = is_sub_type(context->type_table,
        is_upper_bound ? type : expected_type,
        is_upper_bound ? expected_type : type);
    if (!matches_type) {
        if (!expected_type->contains_error && !type->contains_error) {
            log_error(context->log, file_loc, "expected {s} type '{t}', but got type '{t}'",
                (FormatArg[]) {
                    { .s = is_upper_bound ? "at most" : "at least" },
                    { .t = expected_type },
                    { .t = type }
                });
            show_resolved_types(context, (const Type*[]) { type, expected_type }, 2);
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
    if (resolve_type(type)->tag != tag) {
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

static const Type* report_type_mismatch(
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

static const Type* report_cannot_infer(TypingContext* context, const char* msg, const FileLoc* file_loc) {
    log_error(context->log, file_loc, "cannot infer type for {s}", (FormatArg[]) { { .s = msg } });
    return make_error_type(context->type_table);
}

static const Type* report_missing_member(TypingContext* context, const char* name, const Type* type, const FileLoc* file_loc) {
    if (!type->contains_error)
        log_error(context->log, file_loc, "no member '{s}' in '{t}'", (FormatArg[]) { { .s = name }, { .t = type } });
    return make_error_type(context->type_table);
}

static const Type* report_type_expected(TypingContext* context, const Type* type, const FileLoc* file_loc) {
    if (!type->contains_error)
        log_error(context->log, file_loc, "expected type, but got value with type '{t}'", (FormatArg[]) { { .t = type } });
    return make_error_type(context->type_table);
}

static const Type* report_value_expected(TypingContext* context, const Type* type, const FileLoc* file_loc) {
    if (!type->contains_error)
        log_error(context->log, file_loc, "expected value, but got type '{t}'", (FormatArg[]) { { .t = type } });
    return make_error_type(context->type_table);
}

static const Type* report_redeclared_inherited_member(
    TypingContext* context,
    const char* member,
    const Type* parent_type,
    const FileLoc* file_loc)
{
    log_error(context->log, file_loc,
        "member '{s}' is already inherited from type '{t}'",
        (FormatArg[]) { { .s = member }, { .t = parent_type } });
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
    size_t type_param_count = type->kind->tag == TYPE_SIGNATURE
        ? type->kind->signature.type_param_count : get_type_param_count(type);
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

    DynArray args = new_dyn_array(sizeof(Type*));
    for (AstNode* type_arg = type_args; type_arg; type_arg = type_arg->next) {
        const Type* arg = infer_type(context, type_arg);
        push_on_dyn_array(&args, &arg);
    }

    const Type* applied_type = make_app_type(context->type_table, type, args.elems, type_param_count);
    free_dyn_array(&args);
    return applied_type;
}

static const Type* make_enum_ctor_type(TypeTable* type_table, const Type* enum_type, const Type* param_type) {
    return !is_unit_type(param_type) ? make_fun_type(type_table, param_type, enum_type) : enum_type;
}

static const Type* infer_next_path_elem(TypingContext* context, AstNode* prev_elem) {
    assert(prev_elem->type && prev_elem->next);

    bool is_type_expected = false;
    AstNode* next_elem = prev_elem->next;
    const Type* applied_type = get_applied_type(prev_elem->type);
    if (applied_type->tag == TYPE_STRUCT) {
        const StructField* field = find_struct_field(applied_type, next_elem->path_elem.name);
        if (!field)
            goto missing_member;

        next_elem->path_elem.index = field - applied_type->struct_.fields;
        next_elem->path_elem.is_type = false;
        next_elem->type = get_struct_field_type(
            context->type_table, prev_elem->type, next_elem->path_elem.index);
    } else if (applied_type->tag == TYPE_ENUM) {
        const EnumOption* option = find_enum_option(applied_type, next_elem->path_elem.name);
        if (!option)
            goto missing_member;

        bool is_struct_like = is_struct_like_option(option);
        next_elem->path_elem.index = option - applied_type->enum_.options;
        next_elem->path_elem.is_type = is_struct_like;
        const Type* param_type = get_enum_option_param_type(
            context->type_table, prev_elem->type, next_elem->path_elem.index);
        next_elem->type = is_struct_like
            ? param_type : make_enum_ctor_type(context->type_table, prev_elem->type, param_type);
        is_type_expected = true;
    } else if (prev_elem->type->kind->tag == TYPE_SIGNATURE) {
        const Type* signature = prev_elem->type->kind;
        const Type** var = find_signature_var(signature, next_elem->path_elem.name);
        if (!var)
            goto missing_member;

        next_elem->path_elem.index = var - signature->signature.vars;
        if (is_kind_level_type((*var)->kind)) {
            next_elem->path_elem.is_type = true;
            next_elem->type = make_proj_type(
                context->type_table, prev_elem->type, next_elem->path_elem.index);
        } else {
            next_elem->path_elem.is_type = false;
            next_elem->type = (*var)->kind;
        }
    }
    else
        goto missing_member;

    // Make sure we do not perform accesses on a type. For instance, given the type
    // `struct Foo { ... }`, an expression of the form `Foo.x` should be rejected.
    if (is_type_expected != prev_elem->path_elem.is_type) {
        return next_elem->type = is_type_expected
            ? report_type_expected(context, prev_elem->type, &prev_elem->file_loc)
            : report_value_expected(context, prev_elem->type, &prev_elem->file_loc);
    }

    return check_type_args(context,
        next_elem->type,
        next_elem->path_elem.type_args,
        &next_elem->file_loc);

missing_member:
    return next_elem->type = report_missing_member(
        context, next_elem->path_elem.name, prev_elem->type, &next_elem->file_loc);
}

static const Type* make_tuple_like_struct_ctor(TypeTable* type_table, const Type* type) {
    const Type* struct_type = get_applied_type(type);
    assert(is_tuple_like_struct_type(struct_type));

    // Using the tuple-like name as a value results produces a constructor that
    // types as a function (only for tuple-like structures with parameters).
    if (struct_type->struct_.field_count > 0) {
        const Type** arg_types = malloc_or_die(sizeof(Type*) * struct_type->struct_.field_count);
        for (size_t i = 0, n = struct_type->struct_.field_count; i < n; ++i)
            arg_types[i] = get_struct_field_type(type_table, type, i);
        const Type* param_type = make_tuple_type(type_table, arg_types, struct_type->struct_.field_count);
        free(arg_types);
        return make_fun_type(type_table, param_type, type);
    }
    return type;
}

static const Type* infer_path_elems(TypingContext* context, AstNode* path_elem, bool is_type_expected) {
    while (path_elem->next) {
        infer_next_path_elem(context, path_elem);
        path_elem = path_elem->next;
    }

    // A tuple-like structure like `Foo` can either be a type or a value,
    // depending on where it appears. We deal with this ambiguity here.
    if (!is_type_expected && path_elem->path_elem.is_type &&
        is_tuple_like_struct_type(get_applied_type(path_elem->type)))
    {
        path_elem->type = make_tuple_like_struct_ctor(context->type_table, path_elem->type);
        path_elem->path_elem.is_type = false;
    }

    // Make sure we do not use a type as a value, and vice-versa
    if (path_elem->path_elem.is_type != is_type_expected) {
        return is_type_expected
            ? report_type_expected(context, path_elem->type, &path_elem->file_loc)
            : report_value_expected(context, path_elem->type, &path_elem->file_loc);
    }
    return path_elem->type;
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
        path->path.decl_site->tag == AST_SIG_DECL ||
        (path->path.decl_site->tag == AST_TYPE_PARAM && is_kind_level_type(path_elem->type->kind));
    path_elem->type = check_type_args(context,
        path_elem->type,
        path_elem->path_elem.type_args,
        &path_elem->file_loc);

    return path->type = infer_path_elems(context, path_elem, is_type_expected);
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
    if (expected_type->tuple.arg_count != arg_count) {
        log_error(context->log, &tuple->file_loc,
            "expected tuple with {u} argument(s), but got {u}",
            (FormatArg[]) { { .u = expected_type->tuple.arg_count }, { .u = arg_count } });
        return make_error_type(context->type_table);
    }

    const Type** arg_types = check_many(context,
        tuple->tuple_expr.args,
        expected_type->tuple.args,
        arg_count, check_arg);
    tuple->type = make_tuple_type(context->type_table, arg_types, arg_count);
    free(arg_types);
    return tuple->type;
}

const Type* infer_kind(TypingContext* context, AstNode* kind) {
    switch (kind->tag) {
        default:
            assert(false && "invalid kind");
            // fallthrough
        case AST_KIND_STAR:
            return make_star_kind(context->type_table);
        case AST_KIND_ARROW: {
            size_t type_param_count = count_ast_nodes(kind->arrow_kind.dom_kinds);
            const Type** type_params = infer_many(context, kind->arrow_kind.dom_kinds, type_param_count, infer_kind);
            const Type* body = infer_kind(context, kind->arrow_kind.codom_kind);
            const Type* arrow = make_arrow_kind(context->type_table, type_params, type_param_count, body);
            free(type_params);
            return arrow;
        }
        case AST_PATH:
            return infer_path(context, kind, true);
    }
}

static const Type* check_where_clause(TypingContext* context, AstNode* where_clause, const Type* signature) {
    const Type** var_ptr = find_signature_var(signature, where_clause->where_clause.name);
    if (!var_ptr)
    {
        report_missing_member(context,
            where_clause->where_clause.name, signature, &where_clause->file_loc);
        return make_error_type(context->type_table);
    }
    Type* var = (Type*)*var_ptr;
    if (var->var.value)
    {
        log_error(context->log, &where_clause->file_loc,
            "cannot rebind '{s}' to another type",
            (FormatArg[]) { { .s = var->var.name } });
        log_note(context->log, NULL, "'{s}' already bound to '{t}'",
            (FormatArg[]) { { .s = var->var.name }, { .t = var->var.value } });
        return make_error_type(context->type_table);
    }
    ((Type*)var)->var.value = infer_type(context, where_clause->where_clause.type);
    return var;
}

static const Type* infer_where_type(TypingContext* context, AstNode* where_type) {
    const Type* source_signature = infer_path(context, where_type->where_type.path, true);
    if (!expect_type_with_tag(context, source_signature, TYPE_SIGNATURE, "signature", &where_type->where_type.path->file_loc))
        return make_error_type(context->type_table);

    // Create a new signature type to hold the modified signature with new bindings from the clauses
    Type* signature = copy_signature_type(context->type_table, source_signature);
    for (AstNode* where_clause = where_type->where_type.clauses; where_clause; where_clause = where_clause->next)
        check_where_clause(context, where_clause, signature);
    return signature;
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
        case AST_ARRAY_TYPE:
            return type->type = make_unsized_array_type(
                context->type_table, infer_type(context, type->array_type.elem_type));
        case AST_PTR_TYPE:
            return make_ptr_type(context->type_table,
                type->ptr_type.is_const,
                infer_type(context, type->ptr_type.pointed_type));
        case AST_WHERE_TYPE:
            return infer_where_type(context, type);
        case AST_SIG_DECL:
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
        if (is_sub_type(context->type_table, then_type, else_type))
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
    const FileLoc* callee_loc =
        call_or_op->tag == AST_CALL_EXPR ? &call_or_op->call_expr.callee->file_loc : &call_or_op->file_loc;
    if (!expect_type_with_tag(context, callee_type, TYPE_FUN, "function", callee_loc))
        return make_error_type(context->type_table);
    assert(callee_type->tag == TYPE_FUN);
    check_expr(context, arg, callee_type->fun.dom);
    return expect_type(context,
        callee_type->fun.codom, expected_type, true, &call_or_op->file_loc);
}

static const Type* check_call_expr(TypingContext* context, AstNode* call_expr, const Type* expected_type) {
    const Type* callee_type = infer_expr(context, call_expr->call_expr.callee);
    return call_expr->type = check_call(context, call_expr, call_expr->call_expr.arg, callee_type, expected_type);
}

static const Type* check_int_literal(TypingContext* context, AstNode* literal, const Type* expected_type) {
    const Type* resolved_type = resolve_type(expected_type);
    if (resolved_type->tag == TYPE_UNKNOWN)
        return literal->type = make_prim_type(context->type_table, DEFAULT_INT_TYPE_TAG);
    if (!is_int_or_float_type(resolved_type->tag))
        return literal->type = report_type_mismatch(context, "integer literal", expected_type, &literal->file_loc);
    if (is_int_type(resolved_type->tag) &&
        ilog2(literal->int_literal.val) > get_prim_type_bitwidth(resolved_type->tag))
    {
        log_error(context->log, &literal->file_loc,
            "integer literal does not fit in type '{t}'",
            (FormatArg[]) { { .t = resolved_type } });
        return literal->type = make_error_type(context->type_table);
    }
    return literal->type = expected_type;
}

static const Type* check_float_literal(TypingContext* context, AstNode* literal, const Type* expected_type) {
    const Type* resolved_type = resolve_type(expected_type);
    if (resolved_type->tag == TYPE_UNKNOWN)
        return literal->type = make_prim_type(context->type_table, DEFAULT_FLOAT_TYPE_TAG);
    if (!is_float_type(resolved_type->tag))
        return literal->type = report_type_mismatch(context, "floating-point literal", expected_type, &literal->file_loc);
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
    const Type* type = infer_type(context, struct_expr->struct_expr.left);
    const Type* struct_type = get_applied_type(type);
    if (!expect_type_with_tag(context, struct_type, TYPE_STRUCT, "structure", &struct_expr->file_loc))
        return struct_expr->type = make_error_type(context->type_table);

    bool* seen = calloc_or_die(struct_type->struct_.field_count, sizeof(bool));
    for (AstNode* field_expr = struct_expr->struct_expr.fields; field_expr; field_expr = field_expr->next) {
        const StructField* field = find_struct_field(struct_type, field_expr->field_expr.name);
        if (!field) {
            report_missing_member(context, field_expr->field_expr.name, struct_type, &field_expr->file_loc);
            goto fail;
        }

        // Check that this field is not initialized twice
        size_t field_index = field - struct_type->struct_.fields;
        if (seen[field_index]) {
            log_error(context->log, &field_expr->file_loc,
                "field '{s}' is specified more than once",
                (FormatArg[]) { { .s = field_expr->field_expr.name } });
            goto fail;
        }
        seen[field_index] = true;
        field_expr->field_expr.index = field_index;

        // Check that the value of the field matches its type
        const Type* field_type = get_struct_field_type(context->type_table, type, field_index);
        check_expr(context, field_expr->field_expr.val, field_type);
    }

    // Check that all fields have an initializer
    for (size_t i = 0; i < struct_type->struct_.field_count; ++i) {
        if (!seen[i] && !struct_type->struct_.fields[i].has_default) {
            log_error(context->log, &struct_expr->file_loc,
                "missing initializer for field '{s}'",
                (FormatArg[]) { { .s = struct_type->struct_.fields[i].name } });
        }
    }

    const Type* result_type = type;
    if (struct_type->struct_.parent_enum) {
        // If the struct expression constructs an enumeration type,
        // return that instead of the structure type.
        if (type->tag == TYPE_APP) {
            result_type = make_app_type(
                context->type_table,
                struct_type->struct_.parent_enum,
                type->app.args,
                type->app.arg_count);
        } else
            result_type = struct_type->struct_.parent_enum;
    }
    struct_expr->type = expect_type(context, result_type, expected_type, true, &struct_expr->file_loc);
    goto cleanup;

fail:
    struct_expr->type = make_error_type(context->type_table);
cleanup:
    free(seen);
    return struct_expr->type;
}

static const Type* check_member_expr(TypingContext* context, AstNode* member_expr, const Type* expected_type) {
    const FileLoc* left_loc = &member_expr->member_expr.left->file_loc;
    const Type* left_type = infer_expr(context, member_expr->member_expr.left);
    const Type* applied_type = get_applied_type(left_type);

    const Type* member_type = NULL;
    if (member_expr->member_expr.elems_or_index->tag == AST_INT_LITERAL) {
        size_t index = member_expr->member_expr.elems_or_index->int_literal.val;
        if (is_tuple_like_struct_type(applied_type)) {
            member_type = get_struct_field_type(context->type_table, left_type, index);
        } else if (expect_type_with_tag(context, left_type, TYPE_TUPLE, "tuple or tuple-like structure", left_loc))
            member_type = applied_type->tuple.args[index];
    } else if (expect_type_with_tag(context, applied_type, TYPE_STRUCT, "structure", left_loc)) {
        assert(member_expr->member_expr.elems_or_index->tag == AST_PATH_ELEM);
        AstNode first_elem = {
            .tag = AST_PATH_ELEM,
            .type = member_expr->member_expr.left->type,
            .next = member_expr->member_expr.elems_or_index,
            .path_elem.is_type = false
        };
        member_type = infer_path_elems(context, &first_elem, false);
    }

    if (!member_type)
        return member_expr->type = make_error_type(context->type_table); // Error was already reported above

    return member_expr->type = expect_type(
        context, member_type, expected_type, true, &member_expr->file_loc);
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
                make_unsized_array_type(context->type_table, make_prim_type(context->type_table, TYPE_U8)),
                expected_type, true, &expr->file_loc);
        case AST_TYPED_EXPR:
            return expr->type = expect_type(context,
                check_expr(context, expr->typed_expr.left, infer_type(context, expr->typed_expr.type)),
                expected_type, true, &expr->file_loc);
        case AST_TUPLE_EXPR: {
            if (expected_type->tag == TYPE_UNKNOWN)
                return infer_tuple(context, expr, infer_expr);
            const Type* resolved_type = resolve_type(expected_type);
            if (resolved_type->tag == TYPE_TUPLE)
                return check_tuple(context, expr, resolved_type, check_expr);
            return expr->type = report_type_mismatch(context, "tuple expression", expected_type, &expr->file_loc);
        }
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
                return expr->type = report_type_mismatch(context, "function expression", expected_type, &expr->file_loc);
            return check_fun_expr(context, expr, expected_type->fun.dom, expected_type->fun.codom);
        }
        case AST_BREAK_EXPR:
        case AST_CONTINUE_EXPR: {
            const Type* type = make_fun_type(context->type_table,
                make_unit_type(context->type_table),
                make_noret_type(context->type_table));
            return expr->type = expect_type(context, type, expected_type, true, &expr->file_loc);
        }
        case AST_MEMBER_EXPR:
            return check_member_expr(context, expr, expected_type);
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
                make_unsized_array_type(context->type_table, make_prim_type(context->type_table, TYPE_U8)),
                expected_type, false, &pattern->file_loc);
        case AST_TUPLE_PATTERN:
            if (expected_type->tag == TYPE_UNKNOWN)
                return pattern->type = infer_tuple(context, pattern, infer_pattern);
            if (expected_type->tag == TYPE_TUPLE)
                return check_tuple(context, pattern, expected_type, check_pattern);
            return report_type_mismatch(context, "tuple pattern", expected_type, &pattern->file_loc);
        case AST_TYPED_PATTERN:
            pattern->type = check_pattern(context, pattern->typed_pattern.left, infer_type(context, pattern->typed_pattern.type));
            return pattern->type = expect_type(context, pattern->type, expected_type, false, &pattern->file_loc);
        case AST_IDENT_PATTERN:
            if (expected_type->contains_unknown)
                return pattern->type = report_cannot_infer(context, "pattern", &pattern->file_loc);
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
            (expected_type->tag != TYPE_TUPLE || expected_type->tuple.arg_count == arg_count))
        {
            AstNode* expr_arg = expr->tuple_expr.args;
            AstNode* pattern_arg = pattern->tuple_pattern.args;
            const Type** arg_types = malloc_or_die(sizeof(Type*) * arg_count);
            for (size_t i = 0; i < arg_count;
                ++i, expr_arg = expr_arg->next, pattern_arg = pattern_arg->next)
            {
                const Type* arg_type = expected_type->tag == TYPE_TUPLE
                    ? expected_type->tuple.args[i]
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
    const Type* type =  check_pattern_and_expr(context,
        decl->var_decl.pattern,
        decl->var_decl.init,
        make_unknown_type(context->type_table));
    if (must_add_to_parent_sig_or_mod(decl))
        add_idents_to_parent_mod(context->type_table, decl->var_decl.pattern);
    return type;
}

static const Type* infer_type_param(TypingContext* context, AstNode* type_param) {
    const Type* kind = type_param->type_param.kind
        ? infer_kind(context, type_param->type_param.kind)
        : make_star_kind(context->type_table);
    return type_param->type = make_var_type_with_kind(
        context->type_table, type_param->type_param.name, kind);
}

static const Kind* infer_type_params(
    TypingContext* context,
    AstNode* type_params,
    DynArray* type_vars,
    const Kind* body)
{
    for (; type_params; type_params = type_params->next) {
        infer_type_param(context, type_params);
        push_on_dyn_array(type_vars, &type_params->type);
    }
    if (!body)
        return NULL;
    return make_type_ctor_kind(context->type_table, type_vars->elems, type_vars->size, body);
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
    DynArray type_params = new_dyn_array(sizeof(Type*));
    DynArray struct_fields = new_dyn_array(sizeof(StructField));

    struct_type->struct_.is_tuple_like = struct_decl->struct_decl.is_tuple_like;

    struct_type->kind = infer_type_params(context,
        struct_decl->struct_decl.type_params, &type_params, make_star_kind(context->type_table));

    struct_type->struct_.type_params = type_params.elems;
    struct_type->struct_.type_param_count = type_params.size;

    // Add inherited fields to the structure
    const Type* super_struct = NULL;
    if (struct_decl->struct_decl.super_type) {
        const Type* super_type = infer_type(context, struct_decl->struct_decl.super_type);
        super_struct = get_applied_type(super_type);
        if (expect_type_with_tag(
            context, super_struct, TYPE_STRUCT, "structure", &struct_decl->struct_decl.super_type->file_loc))
        {
            struct_type->struct_.super_type = super_type;
            for (size_t i = 0; i < super_struct->struct_.field_count; ++i) {
                StructField field = super_struct->struct_.fields[i];
                field.type = get_struct_field_type(context->type_table, super_type, i);
                field.is_inherited = true;
                push_on_dyn_array(&struct_fields, &field);
            }
        }
        else
            super_struct = NULL;
    }

    struct_decl->type = must_add_to_parent_sig_or_mod(struct_decl)
        ? add_decl_to_parent_sig_or_mod(context->type_table, struct_decl, struct_type)
        : struct_type;

    for (AstNode* field_decl = struct_decl->struct_decl.fields; field_decl; field_decl = field_decl->next) {
        if (struct_decl->struct_decl.is_tuple_like) {
            push_on_dyn_array(&struct_fields, &(StructField) {
                .name = "",
                .type = infer_type(context, field_decl)
            });
        } else if (super_struct && find_struct_field(super_struct, field_decl->field_decl.name)) {
            report_redeclared_inherited_member(context,
                field_decl->field_decl.name,
                struct_type->struct_.super_type,
                &field_decl->file_loc);
        } else {
            StructField field = infer_field_decl(context, field_decl);
            push_on_dyn_array(&struct_fields, &field);
        }
    }

    struct_type->struct_.fields = struct_fields.elems;
    struct_type->struct_.field_count = struct_fields.size;
    seal_struct_type(context->type_table, struct_type);
    free_dyn_array(&struct_fields);
    free_dyn_array(&type_params);
    return struct_decl->type;
}

static const Type* infer_option_decl(
    TypingContext* context,
    AstNode* option_decl,
    Type* enum_type)
{
    if (!option_decl->option_decl.param_type)
        return option_decl->type = make_unit_type(context->type_table);
    if (!option_decl->option_decl.is_struct_like)
        return option_decl->type = infer_type(context, option_decl->option_decl.param_type);

    // If the option is struct-like, we need to create an adequate structure type to represent it
    Type* struct_type = make_struct_type(context->type_table, option_decl->option_decl.name);
    struct_type->struct_.parent_enum = enum_type;
    struct_type->kind = make_type_ctor_kind(context->type_table,
        enum_type->enum_.type_params,
        enum_type->enum_.type_param_count,
        make_star_kind(context->type_table));

    DynArray struct_fields = new_dyn_array(sizeof(StructField));
    for (AstNode* field_decl = option_decl->option_decl.param_type; field_decl; field_decl = field_decl->next) {
        StructField field = infer_field_decl(context, field_decl);
        push_on_dyn_array(&struct_fields, &field);
    }
    struct_type->struct_.type_params = enum_type->enum_.type_params;
    struct_type->struct_.type_param_count = enum_type->enum_.type_param_count;
    struct_type->struct_.fields = struct_fields.elems;
    struct_type->struct_.field_count = struct_fields.size;
    const Type* option_type = seal_struct_type(context->type_table, struct_type);
    free_dyn_array(&struct_fields);

    // We may need to wrap the structure type into a type application if the enumeration is polymorphic
    option_type = make_app_type(context->type_table, option_type,
        enum_type->enum_.type_params,
        enum_type->enum_.type_param_count);
    return option_decl->type = option_type;
}

static const Type* infer_enum_decl(TypingContext* context, AstNode* enum_decl) {
    Type* enum_type = make_enum_type(context->type_table, enum_decl->enum_decl.name);
    DynArray type_params = new_dyn_array(sizeof(Type*));
    DynArray enum_options = new_dyn_array(sizeof(EnumOption));

    enum_type->kind = infer_type_params(context,
        enum_decl->enum_decl.type_params, &type_params, make_star_kind(context->type_table));

    enum_type->enum_.type_params = type_params.elems;
    enum_type->enum_.type_param_count = type_params.size;

    // Add inherited options to the enumeration
    const Type* sub_enum = NULL;
    if (enum_decl->enum_decl.sub_type) {
        const Type* sub_type = infer_type(context, enum_decl->enum_decl.sub_type);
        sub_enum = get_applied_type(sub_type);
        if (expect_type_with_tag(
            context, sub_enum, TYPE_ENUM, "enumeration", &enum_decl->enum_decl.sub_type->file_loc))
        {
            enum_type->enum_.sub_type = sub_type;
            for (size_t i = 0; i < sub_enum->enum_.option_count; ++i) {
                EnumOption option = sub_enum->enum_.options[i];
                option.param_type = get_enum_option_param_type(context->type_table, sub_type, i);
                option.is_inherited = true;
                push_on_dyn_array(&enum_options, &option);
            }
        }
        else
            sub_enum = NULL;
    }

    enum_decl->type = must_add_to_parent_sig_or_mod(enum_decl)
        ? add_decl_to_parent_sig_or_mod(context->type_table, enum_decl, enum_type)
        : enum_type;

    for (AstNode* option_decl = enum_decl->enum_decl.options; option_decl; option_decl = option_decl->next) {
        if (sub_enum && find_enum_option(sub_enum, option_decl->option_decl.name)) {
            report_redeclared_inherited_member(context,
                option_decl->option_decl.name,
                enum_type->enum_.sub_type,
                &option_decl->file_loc);
        } else {
            push_on_dyn_array(&enum_options, &(EnumOption) {
                .name = option_decl->option_decl.name,
                .param_type = infer_option_decl(context, option_decl, enum_type)
            });
        }
    }

    enum_type->enum_.options = enum_options.elems;
    enum_type->enum_.option_count = enum_options.size;
    seal_enum_type(context->type_table, enum_type);
    free_dyn_array(&enum_options);
    free_dyn_array(&type_params);
    return enum_decl->type;
}

static const Type* infer_type_decl(TypingContext* context, AstNode* type_decl) {
    DynArray type_params = new_dyn_array(sizeof(Type*));
    const Kind* type_kind = infer_type_params(context,
        type_decl->type_decl.type_params, &type_params, make_star_kind(context->type_table));

    if (!type_decl->type_decl.aliased_type) {
        // Types without an actual binding should not exist outside of signatures.
        if (type_decl->parent_scope && type_decl->parent_scope->tag == AST_SIG_DECL) {
            return type_decl->type = add_to_parent_sig_or_mod(
                context->type_table, type_decl, type_kind, type_decl->type_decl.name, true);
        } else {
            log_error(context->log, &type_decl->file_loc, "missing binding for type alias", NULL);
            return type_decl->type = make_error_type(context->type_table);
        }
    } else {
        const Type* aliased_type = infer_type(context, type_decl->type_decl.aliased_type);
        const Type* alias_type = make_alias_type(
            context->type_table,
            type_decl->type_decl.name,
            type_params.elems,
            type_params.size,
            aliased_type);
        free_dyn_array(&type_params);

        return type_decl->type = must_add_to_parent_sig_or_mod(type_decl)
            ? add_decl_to_parent_sig_or_mod(context->type_table, type_decl, alias_type)
            : alias_type;
    }
}

static const Type* infer_fun_decl(TypingContext* context, AstNode* fun_decl) {
    DynArray type_params = new_dyn_array(sizeof(Type*));
    infer_type_params(context, fun_decl->fun_decl.type_params, &type_params, NULL);
    const Type* dom_type = infer_pattern(context, fun_decl->fun_decl.param);
    if (fun_decl->fun_decl.ret_type) {
        const Type* codom_type = infer_type(context, fun_decl->fun_decl.ret_type);
        // Set the function type before checking the body in case
        // the function is recursive (or uses `return`).
        fun_decl->type = make_poly_fun_type(context->type_table,
            type_params.elems, type_params.size, dom_type, codom_type);
        if (fun_decl->fun_decl.body)
            check_expr(context, fun_decl->fun_decl.body, codom_type);
    } else if (fun_decl->fun_decl.body) {
        const Type* codom_type = infer_expr(context, fun_decl->fun_decl.body);
        fun_decl->type = make_poly_fun_type(context->type_table,
            type_params.elems, type_params.size, dom_type, codom_type);
    } else
        report_cannot_infer(context, "function declaration", &fun_decl->file_loc);
    free_dyn_array(&type_params);
    if (must_add_to_parent_sig_or_mod(fun_decl))
        add_decl_to_parent_sig_or_mod(context->type_table, fun_decl, fun_decl->type);
    return fun_decl->type;
}

static const Type* infer_mod_decl(TypingContext* context, AstNode* mod_decl) {
    Type* signature = make_signature_type(context->type_table);
    DynArray type_params = new_dyn_array(sizeof(Type*));

    signature->kind = infer_type_params(context,
        mod_decl->mod_decl.type_params, &type_params, make_star_kind(context->type_table));

    signature->signature.type_params = type_params.elems;
    signature->signature.type_param_count = type_params.size;

    SignatureVars vars = { .vars = new_dyn_array(sizeof(Type*)) };
    mod_decl->mod_decl.vars = &vars;
    for (AstNode* decl = mod_decl->mod_decl.members; decl; decl = decl->next)
        infer_decl(context, decl);

    // Hide the value (i.e. the actual type) of opaque types
    for (AstNode* decl = mod_decl->mod_decl.members; decl; decl = decl->next) {
        if (is_public_decl(decl) && is_opaque_decl(decl)) {
            assert(decl->type->tag == TYPE_VAR);
            ((Type*)decl->type)->var.value = NULL;
        }
    }

    signature->signature.vars = vars.vars.elems;
    signature->signature.var_count = vars.vars.size;
    seal_signature_type(context->type_table, signature);
    free_dyn_array(&type_params);
    free_dyn_array(&vars.vars);

    if (mod_decl->mod_decl.signature) {
        const Type* assigned_signature = infer_type(context, mod_decl->mod_decl.signature);
        if (expect_type_with_tag(context,
            assigned_signature, TYPE_SIGNATURE, "signature", &mod_decl->mod_decl.signature->file_loc))
        {
            // TODO: Check that the computed signature is a sub-signature of the assigned one.
            signature = (Type*)resolve_type(assigned_signature);
        }
    }

    // TODO: Check that private symbols do not leak.

    mod_decl->mod_decl.vars = NULL;
    mod_decl->type = make_mod_type(context->type_table, mod_decl->mod_decl.name, signature);
    if (must_add_to_parent_sig_or_mod(mod_decl))
        add_decl_to_parent_sig_or_mod(context->type_table, mod_decl, mod_decl->type);
    return mod_decl->type;
}

const Type* infer_sig_decl(TypingContext* context, AstNode* sig_decl) {
    Type* signature = make_signature_type(context->type_table);
    DynArray type_params = new_dyn_array(sizeof(Type*));

    signature->kind = infer_type_params(context,
        sig_decl->sig_decl.type_params, &type_params, make_star_kind(context->type_table));

    signature->signature.type_params = type_params.elems;
    signature->signature.type_param_count = type_params.size;

    SignatureVars vars = { .vars = new_dyn_array(sizeof(Type*)) };
    sig_decl->sig_decl.vars = &vars;
    for (AstNode* decl = sig_decl->sig_decl.members; decl; decl = decl->next)
        infer_decl(context, decl);

    signature->signature.vars = vars.vars.elems;
    signature->signature.var_count = vars.vars.size;
    seal_signature_type(context->type_table, signature);
    free_dyn_array(&type_params);
    free_dyn_array(&vars.vars);

    sig_decl->sig_decl.vars = NULL;
    return sig_decl->type = must_add_to_parent_sig_or_mod(sig_decl)
        ? add_decl_to_parent_sig_or_mod(context->type_table, sig_decl, signature)
        : signature;
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
        case AST_SIG_DECL:
            return infer_sig_decl(context, decl);
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
