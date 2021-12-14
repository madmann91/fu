#include "lang/check.h"
#include "lang/env.h"
#include "lang/type.h"
#include "lang/ast.h"
#include "core/alloc.h"

#include <assert.h>
#include <stdlib.h>

struct checker {
    struct log* log;
    struct type_table* type_table;
};

static const struct type* infer_type(struct checker*, struct ast*);
static const struct type* infer_pattern(struct checker*, struct ast*);
static const struct type* check_pattern(struct checker*, struct ast*, const struct type*);
static const struct type* infer_expr(struct checker*, struct ast*);
static const struct type* check_expr(struct checker*, struct ast*, const struct type*);
static const struct type* infer_stmt(struct checker*, struct ast*);
static const struct type* check_stmt(struct checker*, struct ast*, const struct type*);
static void infer_decl(struct checker*, struct ast*);

static const struct type* report_cannot_infer(struct checker* checker, const char* msg, const struct file_loc* loc) {
    log_error(checker->log, loc, "cannot infer type for {s}", (union format_arg[]) { { .s = msg } });
    return make_error_type(checker->type_table);
}

static const struct type* expect_type_or_error(
    struct checker* checker,
    const struct type* type,
    const struct type* expected_type,
    const struct file_loc* loc)
{
    if (!is_subtype(type, expected_type)) {
        if (!type->contains_error && !expected_type->contains_error) {
            log_error(checker->log, loc,
                "expected type '{t}', but got '{t}'",
                (union format_arg[]) { { .t = expected_type }, { .t = type } });
        }
        return make_error_type(checker->type_table);
    }
    return type;
}

static const struct type* report_unexpected_type(
    struct checker* checker,
    const char* msg,
    const struct type* expected_type,
    const struct file_loc* loc)
{
    if (!expected_type->contains_error) {
        log_error(checker->log, loc,
            "expected type '{t}', but got {s}",
            (union format_arg[]) { { .t = expected_type }, { .s = msg } });
    }
    return make_error_type(checker->type_table);
}

static const struct type** check_many(
    struct checker* checker,
    struct ast* ast,
    const struct type** expected_types, size_t count,
    const struct type* (*check_one)(struct checker*, struct ast*, const struct type*))
{
    const struct type** types = malloc_or_die(sizeof(struct type*) * count);
    for (size_t i = 0; i < count; ++i, ast = ast->next)
        types[i] = check_one(checker, ast, expected_types[i]);
    return types;
}

static const struct type** infer_many(
    struct checker* checker,
    struct ast* ast,
    size_t count,
    const struct type* (*infer_one)(struct checker*, struct ast*))
{
    const struct type** types = malloc_or_die(sizeof(struct type*) * count);
    for (size_t i = 0; i < count; ++i, ast = ast->next)
        types[i] = infer_one(checker, ast);
    return types;
}

static const struct type* check_tuple(
    struct checker* checker,
    const char* msg,
    struct ast* ast,
    const struct type* expected_type,
    const struct type* (*check_arg)(struct checker*, struct ast*, const struct type*))
{
    size_t arg_count = get_ast_list_length(ast->tuple_type.args);
    if (expected_type->tag != TYPE_TUPLE)
        return report_unexpected_type(checker, msg, expected_type, &ast->loc);
    const struct type** arg_types = check_many(checker, ast->tuple_type.args, expected_type->tuple_type.args, arg_count, check_arg);
    const struct type* tuple_type = make_tuple_type(checker->type_table, arg_types, arg_count);
    free(arg_types);
    return tuple_type;
}

static const struct type* infer_tuple(
    struct checker* checker,
    struct ast* ast,
    const struct type* (*infer_arg)(struct checker*, struct ast*))
{
    size_t arg_count = get_ast_list_length(ast->tuple_type.args);
    const struct type** arg_types = infer_many(checker, ast->tuple_type.args, arg_count, infer_arg);
    const struct type* tuple_type = make_tuple_type(checker->type_table, arg_types, arg_count);
    free(arg_types);
    return tuple_type;
}

static const struct type* infer_type(struct checker* checker, struct ast* type) {
    switch (type->tag) {
        case AST_PRIM_TYPE:
            return type->type = make_prim_type(checker->type_table, type->prim_type.tag);
        case AST_TUPLE_TYPE:
            return type->type = infer_tuple(checker, type, infer_type);
        default:
            assert(false && "unsupported type");
            // fallthrough
        case AST_ERROR:
            return type->type = make_error_type(checker->type_table);
    }
}

static const struct type* check_type(struct checker* checker, struct ast* type, const struct type* expected_type) {
    return expect_type_or_error(checker, infer_type(checker, type), expected_type, &type->loc);
}

static const struct type* infer_pattern(struct checker* checker, struct ast* pattern) {
    switch (pattern->tag) {
        case AST_IDENT:
            return pattern->type = report_cannot_infer(checker, "identifier", &pattern->loc);
        case AST_TYPE_ANNOT:
            return pattern->type = check_pattern(checker, pattern->type_annot.left, infer_type(checker, pattern->type_annot.type));
        case AST_TUPLE_PATTERN:
            return pattern->type = infer_tuple(checker, pattern, infer_pattern);
        default:
            assert(false && "unsupported pattern");
            // fallthrough
        case AST_ERROR:
            return pattern->type = make_error_type(checker->type_table);
    }
}

static void infer_pattern_with_expr(struct checker* checker, struct ast* pattern, struct ast* expr) {
    if (pattern->tag == AST_TUPLE_PATTERN && expr->tag == AST_TUPLE_EXPR) {
        struct ast* pattern_arg = pattern->tuple_pattern.args;
        struct ast* expr_arg = expr->tuple_expr.args;
        size_t arg_count = get_ast_list_length(pattern_arg);
        const struct type** arg_types = malloc_or_die(sizeof(struct type*) * arg_count * 2);
        for (size_t i = 0; pattern_arg && expr_arg; i++, pattern_arg = pattern_arg->next, expr_arg = expr_arg->next) {
            infer_pattern_with_expr(checker, pattern_arg, expr_arg);
            arg_types[i] = pattern_arg->type;
            arg_types[arg_count + i] = expr_arg->type;
        }
        if (!pattern_arg && !expr_arg) {
            pattern->type = make_tuple_type(checker->type_table, arg_types, arg_count);
            expr->type    = make_tuple_type(checker->type_table, arg_types + arg_count, arg_count);
        } else {
            log_error(checker->log, pattern_arg ? &pattern_arg->loc : &expr_arg->loc, "too many tuple arguments", NULL);
            pattern->type = expr->type = make_error_type(checker->type_table);
        }
        free(arg_types);
    } else if (pattern->tag == AST_ARRAY_PATTERN && expr->tag == AST_ARRAY_EXPR) {
        struct ast* pattern_elem = pattern->array_pattern.elems;
        struct ast* expr_elem = expr->array_expr.elems;
        if (!expr_elem || !pattern_elem)
            pattern->type = expr->type = report_cannot_infer(checker, "array", &expr->loc);
        else {
            infer_pattern_with_expr(checker, pattern_elem, expr_elem);
            const struct type* pattern_elem_type = pattern_elem->type;
            const struct type* expr_elem_type = expr_elem->type;
            size_t elem_count = 0;
            for (; pattern_elem && expr_elem; elem_count++, pattern_elem = pattern_elem->next, expr_elem = expr_elem->next) {
                if (elem_count == 0) continue;
                check_pattern(checker, pattern_elem, pattern_elem_type);
                check_expr(checker, expr_elem, expr_elem_type);
            }
            if (!pattern_elem && !expr_elem) {
                log_error(checker->log, pattern_elem ? &pattern_elem->loc : &expr_elem->loc, "too many array elements", NULL);
                pattern->type = expr->type = make_error_type(checker->type_table);
            } else {
                pattern->type = make_array_type(checker->type_table, pattern_elem_type, elem_count);
                expr->type    = make_array_type(checker->type_table, expr_elem_type, elem_count);
            }
        }
    } else
        check_pattern(checker, pattern, infer_expr(checker, expr));
}

static const struct type* check_pattern(struct checker* checker, struct ast* pattern, const struct type* expected_type) {
    switch (pattern->tag) {
        case AST_IDENT:
            return pattern->type = expected_type;
        case AST_TUPLE_PATTERN:
            return pattern->type = check_tuple(checker, "tuple pattern", pattern, expected_type, check_pattern);
        default:
            return pattern->type = expect_type_or_error(checker, expected_type, infer_pattern(checker, pattern), &pattern->loc);
    }
}

static const struct type* infer_expr(struct checker* checker, struct ast* expr) {
    switch (expr->tag) {
        case AST_BLOCK_EXPR: {
            const struct type* last_type = make_unit_type(checker->type_table);
            for (struct ast* stmt = expr->block_expr.stmts; stmt; stmt = stmt->next)
                last_type = infer_stmt(checker, stmt);
            if (expr->block_expr.ends_with_semicolon)
                last_type = make_unit_type(checker->type_table);
            return expr->type = last_type;
        }
        default:
            assert(false && "unsupported expression");
            // fallthrough
        case AST_ERROR:
            return expr->type = make_error_type(checker->type_table);
    }
}

static const struct type* check_expr(struct checker* checker, struct ast* expr, const struct type* expected_type) {
    switch (expr->tag) {
        case AST_BLOCK_EXPR: {
            const struct type* last_type = make_unit_type(checker->type_table);
            for (struct ast* stmt = expr->block_expr.stmts; stmt; stmt = stmt->next) {
                last_type = stmt->next || expr->block_expr.ends_with_semicolon
                    ? infer_stmt(checker, stmt) : check_stmt(checker, stmt, expected_type);
            }
            if (expr->block_expr.ends_with_semicolon)
                last_type = make_unit_type(checker->type_table);
            return expr->type = expect_type_or_error(checker, last_type, expected_type, &expr->loc);
        }
        default:
            return expr->type = expect_type_or_error(checker, infer_expr(checker, expr), expected_type, &expr->loc);
    }
}

static const struct type* infer_stmt(struct checker* checker, struct ast* stmt) {
    switch (stmt->tag) {
        case AST_FUN_DECL:
        case AST_VAR_DECL:
        case AST_CONST_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
            infer_decl(checker, stmt);
            return make_unit_type(checker->type_table);
        default:
            assert(false && "unsupported statement");
            // fallthrough
        case AST_ERROR:
            return stmt->type = make_error_type(checker->type_table);
    }
}

static const struct type* check_stmt(struct checker* checker, struct ast* stmt, const struct type* expected_type) {
    return expect_type_or_error(checker, infer_stmt(checker, stmt), expected_type, &stmt->loc);
}

static void infer_fun_decl(struct checker* checker, struct ast* fun_decl) {
    const struct type* codom_type = NULL;
    if (fun_decl->fun_decl.ret_type)
        codom_type = infer_type(checker, fun_decl->fun_decl.ret_type);
    const struct type* dom_type = infer_pattern(checker, fun_decl->fun_decl.param);

    // Set the function type before entering the function body if possible,
    // in order to be able to type `return`.
    if (codom_type)
        fun_decl->type = make_fun_type(checker->type_table, dom_type, codom_type);

    const struct type* body_type = codom_type
        ? check_expr(checker, fun_decl->fun_decl.body, codom_type)
        : infer_expr(checker, fun_decl->fun_decl.body);

    if (!fun_decl->type)
        fun_decl->type = make_fun_type(checker->type_table, dom_type, body_type);
}

static void infer_struct_decl(struct checker* checker, struct ast* struct_decl) {
}

static void infer_enum_decl(struct checker* checker, struct ast* enum_decl) {
}

static void infer_const_or_var_decl(struct checker* checker, struct ast* decl) {
    if (decl->var_decl.init)
        infer_pattern_with_expr(checker, decl->var_decl.pattern, decl->var_decl.init);
    infer_pattern(checker, decl->var_decl.pattern);
} 

static void infer_decl(struct checker* checker, struct ast* decl) {
    switch (decl->tag) {
        case AST_FUN_DECL:    infer_fun_decl(checker, decl);    break;
        case AST_STRUCT_DECL: infer_struct_decl(checker, decl); break;
        case AST_ENUM_DECL:   infer_enum_decl(checker, decl);   break;
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            infer_const_or_var_decl(checker, decl); 
            break;
        default:
            assert(false && "unsupported declaration");
            return;
    }
}

static void check_program(struct checker* checker, struct ast* first_decl) {
    for (struct ast* decl = first_decl; decl; decl = decl->next)
        infer_decl(checker, decl);
}

void check_ast(struct ast* ast, struct type_table* type_table, struct log* log) {
    struct checker checker = {
        .log = log,
        .type_table = type_table,
    };
    check_program(&checker, ast);
}
