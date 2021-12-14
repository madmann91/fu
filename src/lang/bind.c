#include "lang/bind.h"
#include "lang/env.h"
#include "lang/ast.h"

#include <assert.h>

struct binder {
    struct log* log;
    struct env* env;
};

static void bind_decl(struct binder*, struct ast*);
static void bind_type(struct binder*, struct ast*);
static void bind_pattern(struct binder*, struct ast*);
static void bind_expr(struct binder*, struct ast*);
static void bind_stmt(struct binder*, struct ast*);

static void add_symbol_or_error(struct binder* binder, const char* name, struct ast* ast, const struct file_loc* loc) {
    if (!add_symbol(binder->env, name, ast)) {
        struct ast* other = find_symbol(binder->env, name);
        log_error(binder->log, loc, "redefinition of symbol '{s}'", (union format_arg[]) { { .s = name } });
        log_note(binder->log, &other->loc, "previous definition was here", NULL);
    }
}

static struct ast* find_symbol_or_error(struct binder* binder, const char* name, const struct file_loc* loc) {
    struct ast* symbol = find_symbol(binder->env, name);
    if (symbol)
        return symbol;
    log_error(binder->log, loc, "unknown identifier '{s}'", (union format_arg[]) { { .s = name } });
    return NULL;
}

static void insert_decl_in_env(struct binder* binder, struct ast* decl) {
    const char* name = NULL;
    switch (decl->tag) {
        case AST_FUN_DECL:    name = decl->fun_decl.name;    break;
        case AST_STRUCT_DECL: name = decl->struct_decl.name; break;
        case AST_ENUM_DECL:   name = decl->enum_decl.name;   break;
        default: return;
    }
    add_symbol_or_error(binder, name, decl, &decl->loc);
}

static void bind_many(struct binder* binder, struct ast* ast, void (*bind_one)(struct binder*, struct ast*)) {
    for (; ast; ast = ast->next)
        bind_one(binder, ast);
}

static void bind_path_elem(struct binder* binder, struct ast* path_elem) {
    find_symbol_or_error(binder, path_elem->path_elem.ident->ident.name, &path_elem->path_elem.ident->loc);
    bind_many(binder, path_elem->path_elem.type_args, bind_type);
}

static void bind_path(struct binder* binder, struct ast* path) {
    // Only bind the first element on the path
    bind_path_elem(binder, path->path.elems);
}

static void bind_type(struct binder* binder, struct ast* type) {
    switch (type->tag) {
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
            insert_decl_in_env(binder, type);
            bind_decl(binder, type);
            break;
        case AST_PATH:
            bind_path(binder, type);
            break;
        case AST_PRIM_TYPE:
            break;
        case AST_TUPLE_TYPE:
            bind_many(binder, type->tuple_type.args, bind_type);
            break;
        default:
            assert(false && "unsupported type");
            break;
    }
}

static void bind_pattern(struct binder* binder, struct ast* pattern) {
    switch (pattern->tag) {
        case AST_LITERAL:
            break;
        case AST_TUPLE_PATTERN:
            bind_many(binder, pattern->tuple_pattern.args, bind_pattern);
            break;
        case AST_CALL_PATTERN:
            bind_path(binder, pattern->call_pattern.callee);
            bind_pattern(binder, pattern->call_pattern.arg);
            break;
        case AST_TYPE_ANNOT:
            bind_pattern(binder, pattern->type_annot.left);
            bind_type(binder, pattern->type_annot.type);
            break;
        case AST_IDENT:
            add_symbol_or_error(binder, pattern->ident.name, pattern, &pattern->loc);
            break;
        default:
            assert(false && "unsupported pattern");
            break;
    }
}

static void bind_stmt(struct binder* binder, struct ast* stmt) {
    switch (stmt->tag) {
        case AST_FUN_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
        case AST_CONST_DECL:
        case AST_VAR_DECL:
            bind_decl(binder, stmt);
            break;
        default:
            bind_expr(binder, stmt);
            break;
    }
}

static void bind_match_case(struct binder* binder, struct ast* match_case) {
    bind_pattern(binder, match_case->match_case.pattern);
    bind_expr(binder, match_case->match_case.case_val);
}

static void bind_expr(struct binder* binder, struct ast* expr) {
    switch (expr->tag) {
        case AST_LITERAL:
            break;
        case AST_BLOCK_EXPR:
            push_env(&binder->env);
            // Insert functions, structures, enums and all declarations that belong
            // to the block before binding the contents of each statement.
            for (struct ast* stmt = expr->block_expr.stmts; stmt; stmt = stmt->next)
                insert_decl_in_env(binder, stmt);
            bind_many(binder, expr->block_expr.stmts, bind_stmt);
            pop_env(&binder->env);
            break;
        case AST_PATH:
            bind_path(binder, expr);
            break;
        case AST_IF_EXPR:
            bind_expr(binder, expr->if_expr.cond);
            bind_expr(binder, expr->if_expr.branch_true);
            if (expr->if_expr.branch_false)
                bind_expr(binder, expr->if_expr.branch_false);
            break;
        case AST_MATCH_EXPR:
            bind_expr(binder, expr->match_expr.match_val);
            bind_many(binder, expr->match_expr.cases, bind_match_case);
            break;
        case AST_TUPLE_EXPR:
            bind_many(binder, expr->tuple_expr.args, bind_expr);
            break;
        case AST_TYPE_ANNOT:
            bind_expr(binder, expr->type_annot.left);
            bind_type(binder, expr->type_annot.type);
            break;
        case AST_IDENT:
            expr->ident.decl = find_symbol_or_error(binder, expr->ident.name, &expr->loc);
            break;
        default:
            assert(false && "unsupported expr");
            break;
    }
}

static void bind_field_decl(struct binder* binder, struct ast* field_decl) {
    bind_type(binder, field_decl->field_decl.type);
}

static void bind_option_decl(struct binder* binder, struct ast* option_decl) {
    if (option_decl->option_decl.param)
        bind_type(binder, option_decl->option_decl.param);
}

static void bind_type_param(struct binder* binder, struct ast* type_param) {
    add_symbol_or_error(binder, type_param->ident.name, type_param, &type_param->loc);
}

static void bind_decl(struct binder* binder, struct ast* decl) {
    switch (decl->tag) {
        case AST_FUN_DECL:
            if (decl->fun_decl.ret_type)
                bind_type(binder, decl->fun_decl.ret_type);
            bind_pattern(binder, decl->fun_decl.param);
            bind_expr(binder, decl->fun_decl.body);
            break;
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            bind_pattern(binder, decl->var_decl.pattern);
            if (decl->var_decl.init)
                bind_expr(binder, decl->var_decl.init);
            break;
        case AST_STRUCT_DECL:
            bind_many(binder, decl->struct_decl.fields, bind_field_decl);
            bind_many(binder, decl->struct_decl.type_params, bind_type_param);
            break;
        case AST_ENUM_DECL:
            bind_many(binder, decl->enum_decl.options, bind_option_decl);
            bind_many(binder, decl->enum_decl.type_params, bind_type_param);
            break;
        default:
            assert(false && "unsupported declaration");
            return;
    }
}

static void bind_program(struct binder* binder, struct ast* first_decl) {
    for (struct ast* decl = first_decl; decl; decl = decl->next)
        insert_decl_in_env(binder, decl);
    for (struct ast* decl = first_decl; decl; decl = decl->next)
        bind_decl(binder, decl);
}

void bind_ast(struct ast* ast, struct log* log) {
    struct binder binder = {
        .log = log,
        .env = NULL
    };
    push_env(&binder.env);
    bind_program(&binder, ast);
    free_env(binder.env);
}
