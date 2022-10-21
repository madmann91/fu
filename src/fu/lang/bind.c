#include "fu/lang/bind.h"
#include "fu/lang/ast.h"
#include "fu/core/hash_table.h"
#include "fu/core/hash.h"
#include "fu/core/alloc.h"
#include "fu/core/log.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
    const char* name;
    AstNode* decl_site;
} Symbol;

struct Scope {
    HashTable symbols;
    AstNode* ast_node;
    struct Scope* prev, *next;
};

static bool compare_symbols(const void* left, const void* right) {
    return !strcmp(((Symbol*)left)->name, ((Symbol*)right)->name);
}

static Scope* new_scope(Scope* prev) {
    Scope* scope = malloc_or_die(sizeof(Scope));
    scope->prev = prev;
    scope->next = NULL;
    scope->symbols = new_hash_table(sizeof(Symbol));
    if (prev)
        prev->next = scope;
    return scope;
}

Env new_env(Log* log) {
    return (Env) { .log = log, .first_scope = new_scope(NULL) };
}

void free_env(Env* env) {
    Scope* scope = env->first_scope;
    while (scope) {
        Scope* next = scope->next;
        free_hash_table(&scope->symbols);
        free(scope);
        scope = next;
    }
}

static void insert_symbol(Env* env, const char* name, AstNode* decl_site) {
    assert(env->cur_scope);
    // Variables or patterns that begin with '_' are anonymous and cannot be referred to.
    if (name[0] == '_')
        return;
    Symbol symbol = { .name = name, .decl_site = decl_site };
    uint32_t hash = hash_str(hash_init(), name);
    bool was_inserted = insert_in_hash_table(&env->cur_scope->symbols,
        &symbol, hash, sizeof(Symbol), compare_symbols);
    if (!was_inserted) {
        log_error(env->log, &decl_site->file_loc, "redefinition of symbol '{s}'",
            (FormatArg[]) { { .s = name } });
        const Symbol* prev_symbol = find_in_hash_table(&env->cur_scope->symbols,
            &symbol, hash, sizeof(Symbol), compare_symbols);
        assert(prev_symbol);
        log_note(env->log, &prev_symbol->decl_site->file_loc, "previously declared here", NULL);
    }
}

static size_t levenshtein_distance(const char* left, const char* right, size_t min_dist) {
    if (!*left)  return strlen(right);
    if (!*right) return strlen(left);

    if (left[0] == right[0])
        return levenshtein_distance(left + 1, right + 1, min_dist);

    if (min_dist == 0)
        return 1;

    size_t a = levenshtein_distance(left + 1, right, min_dist - 1);
    size_t b = levenshtein_distance(left, right + 1, min_dist - 1);
    size_t c = levenshtein_distance(left + 1, right + 1, min_dist - 1);
    size_t min = a;
    min = min < b ? min : b;
    min = min < c ? min : c;
    return 1 + min;
}

static void suggest_similar_symbol(Env* env, const char* name) {
    size_t min_dist = 2;

    // Do not suggest similar symbols for identifiers that are too short
    if (strlen(name) <= min_dist)
        return;

    const char* similar_name = NULL;

    for (Scope* scope = env->cur_scope; scope; scope = scope->prev) {
        Symbol* symbols = scope->symbols.elems;
        for (size_t i = 0; i < scope->symbols.capacity; i++) {
            if (!is_bucket_occupied(&scope->symbols, i))
                continue;
            size_t dist = levenshtein_distance(name, symbols[i].name, min_dist);
            if (dist < min_dist) {
                min_dist = dist;
                similar_name = symbols[i].name;
            }
        }
    }

    if (similar_name)
        log_note(env->log, NULL, "did you mean '{s}'?", (FormatArg[]) { { .s = similar_name } });
}

static AstNode* find_symbol(Env* env, const char* name, const FileLoc* file_loc) {
    uint32_t hash = hash_str(hash_init(), name);
    for (Scope* scope = env->cur_scope; scope; scope = scope->prev) {
        Symbol* symbol = find_in_hash_table(&scope->symbols,
            &(Symbol) { .name = name },
            hash, sizeof(Symbol),
            compare_symbols);
        if (symbol)
            return symbol->decl_site;
    }
    log_error(env->log, file_loc, "unknown identifier '{s}'", (FormatArg[]) { { .s = name } });
    suggest_similar_symbol(env, name);
    return NULL;
}

static inline AstNode* find_enclosing_scope(
    Env* env,
    const char* keyword,
    const char* context,
    AstNodeTag fst_tag,
    AstNodeTag snd_tag,
    const FileLoc* file_loc)
{
    for (Scope* scope = env->cur_scope; scope; scope = scope->prev) {
        if (scope->ast_node->tag == fst_tag || scope->ast_node->tag == snd_tag)
            return scope->ast_node;
    }
    log_error(env->log, file_loc, "use of '{$}{s}{$}' outside of a {s}", (FormatArg[]) {
        { .style = keyword_style },
        { .s = keyword },
        { .style = reset_style },
        { .s = context },
    });
    return NULL;
}

static inline AstNode* find_enclosing_loop(Env* env, const char* keyword, const FileLoc* file_loc) {
    return find_enclosing_scope(env, keyword, "loop", AST_WHILE_LOOP, AST_FOR_LOOP, file_loc);
}

static inline AstNode* find_enclosing_fun(Env* env, const FileLoc* file_loc) {
    return find_enclosing_scope(env, "return", "function", AST_FUN_EXPR, AST_FUN_DECL, file_loc);
}

static inline void push_scope(Env* env, AstNode* ast_node) {
    if (!env->cur_scope)
        env->cur_scope = env->first_scope;
    else if (env->cur_scope->next)
        env->cur_scope = env->cur_scope->next;
    else
        env->cur_scope = new_scope(env->cur_scope);
    env->cur_scope->ast_node = ast_node;
    clear_hash_table(&env->cur_scope->symbols);
}

static inline void pop_scope(Env* env) {
    assert(env->cur_scope);
    env->cur_scope = env->cur_scope->prev;
}

static inline void bind_many(Env* env, AstNode* elems, void (*bind_one)(Env*, AstNode*)) {
    for (; elems; elems = elems->next)
        bind_one(env, elems);
}

static void insert_decl_in_env(Env* env, AstNode* decl) {
    const char* name = get_decl_name(decl);
    if (name)
        insert_symbol(env, name, decl);
}

static void insert_many_decls_in_env(Env* env, AstNode* decls) {
    for (AstNode* decl = decls; decl; decl = decl->next)
        insert_decl_in_env(env, decl);
}

static void bind_while_loop(Env* env, AstNode* while_loop) {
    bind_expr(env, while_loop->while_loop.cond);
    push_scope(env, while_loop);
    bind_expr(env, while_loop->while_loop.body);
    pop_scope(env);
}

static void bind_for_loop(Env* env, AstNode* for_loop) {
    bind_const_pattern(env, for_loop->for_loop.pattern);
    bind_expr(env, for_loop->for_loop.range);
    push_scope(env, for_loop);
    bind_expr(env, for_loop->for_loop.body);
    pop_scope(env);
}

void bind_stmt(Env* env, AstNode* stmt) {
    stmt->parent_scope = env->cur_scope->ast_node;
    switch (stmt->tag) {
        case AST_FUN_DECL:
        case AST_VAR_DECL:
        case AST_CONST_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
        case AST_MOD_DECL:
        case AST_SIG_DECL:
        case AST_USING_DECL:
        case AST_TYPE_DECL:
            bind_decl(env, stmt);
            break;
        case AST_WHILE_LOOP:
            bind_while_loop(env, stmt);
            break;
        case AST_FOR_LOOP:
            bind_for_loop(env, stmt);
            break;
        default:
            bind_expr(env, stmt);
            break;
    }
}

static void bind_path(Env* env, AstNode* path) {
    // Only bind the base of the path
    // (the other elements cannot be bound because types are not yet known).
    AstNode* base = path->path.elems;
    path->path.decl_site = find_symbol(env, base->path_elem.name, &base->file_loc);

    for (AstNode* elem = path->path.elems; elem; elem = elem->next)
        bind_many(env, elem->path_elem.type_args, bind_type);
}

static void bind_pattern(Env* env, AstNode* pattern, bool is_const) {
    void (*bind_sub_pattern)(Env*, AstNode*) = is_const ? bind_const_pattern : bind_non_const_pattern;
    pattern->parent_scope = env->cur_scope->ast_node;
    switch (pattern->tag) {
        case AST_PATH:
            bind_path(env, pattern);
            break;
        case AST_BOOL_LITERAL:
        case AST_INT_LITERAL:
        case AST_CHAR_LITERAL:
        case AST_STR_LITERAL:
            return;
        case AST_IDENT_PATTERN:
            insert_symbol(env, pattern->ident_pattern.name, pattern);
            pattern->ident_pattern.is_const = is_const;
            break;
        case AST_TUPLE_PATTERN:
            bind_many(env, pattern->tuple_pattern.args, bind_sub_pattern);
            break;
        case AST_FIELD_PATTERN:
            bind_pattern(env, pattern->field_pattern.val, is_const);
            break;
        case AST_STRUCT_PATTERN:
            bind_path(env, pattern->struct_pattern.left);
            bind_many(env, pattern->struct_pattern.fields, bind_sub_pattern);
            break;
        case AST_CTOR_PATTERN:
            bind_path(env, pattern->ctor_pattern.path);
            bind_pattern(env, pattern->ctor_pattern.arg, is_const);
            break;
        case AST_TYPED_PATTERN:
            bind_pattern(env, pattern->typed_pattern.left, is_const);
            bind_type(env, pattern->typed_pattern.type);
            break;
        case AST_ARRAY_PATTERN:
            bind_many(env, pattern->array_pattern.elems, bind_sub_pattern);
            break;
        default:
            assert(false && "invalid pattern");
            break;
    }
}

void bind_const_pattern(Env* env, AstNode* pattern) {
    bind_pattern(env, pattern, true);
}

void bind_non_const_pattern(Env* env, AstNode* pattern) {
    bind_pattern(env, pattern, false);
}

static void bind_type_param(Env* env, AstNode* type_param) {
    if (type_param->type_param.kind)
        bind_kind(env, type_param->type_param.kind);
    insert_symbol(env, type_param->type_param.name, type_param);
}

static void bind_type_params(Env* env, AstNode* type_params) {
    bind_many(env, type_params, bind_type_param);
}

static void bind_members(Env* env, AstNode* decl, AstNode* type_params, AstNode* super_or_sub_type, AstNode* members) {
    push_scope(env, decl);
    bind_type_params(env, type_params);
    if (super_or_sub_type)
        bind_type(env, super_or_sub_type);
    insert_many_decls_in_env(env, members);
    bind_many(env, members, bind_decl);
    pop_scope(env);
}

void bind_decl(Env* env, AstNode* decl) {
    // Note: The current scope might be NULL upon entering the top-level module.
    decl->parent_scope = env->cur_scope ? env->cur_scope->ast_node : NULL;
    switch (decl->tag) {
        case AST_FIELD_DECL:
            bind_type(env, decl->field_decl.type);
            break;
        case AST_OPTION_DECL:
            if (decl->option_decl.param_type) {
                if (decl->option_decl.is_struct_like)
                    bind_many(env, decl->option_decl.param_type, bind_decl);
                else
                    bind_type(env, decl->option_decl.param_type);
            }
            break;
        case AST_ENUM_DECL:
            bind_members(env, decl,
                decl->enum_decl.type_params,
                decl->enum_decl.sub_type,
                decl->enum_decl.options);
            break;
        case AST_STRUCT_DECL:
            if (decl->struct_decl.is_tuple_like) {
                push_scope(env, decl);
                bind_type_params(env, decl->struct_decl.type_params);
                bind_many(env, decl->struct_decl.fields, bind_type);
                pop_scope(env);
            } else {
                bind_members(env, decl,
                    decl->struct_decl.type_params,
                    decl->struct_decl.super_type,
                    decl->struct_decl.fields);
            }
            break;
        case AST_SIG_DECL:
            bind_members(env, decl, decl->sig_decl.type_params, NULL, decl->sig_decl.members);
            break;
        case AST_MOD_DECL:
            if (decl->mod_decl.signature)
                bind_type(env, decl->mod_decl.signature);
            if (decl->mod_decl.aliased_mod)
                bind_type(env, decl->mod_decl.aliased_mod);
            bind_members(env, decl, decl->mod_decl.type_params, NULL, decl->mod_decl.members);
            break;
        case AST_TYPE_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->type_decl.type_params);
            if (decl->type_decl.aliased_type)
                bind_type(env, decl->type_decl.aliased_type);
            pop_scope(env);
            break;
        case AST_FUN_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->fun_decl.type_params);
            bind_const_pattern(env, decl->fun_decl.param);
            if (decl->fun_decl.ret_type)
                bind_type(env, decl->fun_decl.ret_type);
            if (decl->fun_decl.body)
                bind_expr(env, decl->fun_decl.body);
            bind_many(env, decl->fun_decl.used_sigs, bind_type);
            pop_scope(env);
            break;
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            if (decl->var_decl.init)
                bind_expr(env, decl->var_decl.init);
            bind_pattern(env, decl->var_decl.pattern, decl->tag == AST_CONST_DECL);
            break;
        case AST_USING_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->using_decl.type_params);
            bind_type(env, decl->using_decl.used_mod);
            pop_scope(env);
            break;
        case AST_VAL_DECL:
            break;
        default:
            assert(false && "invalid declaration");
            break;
    }
}

static void bind_match_case(Env* env, AstNode* match_case) {
    push_scope(env, match_case);
    bind_const_pattern(env, match_case->match_case.pattern);
    bind_expr(env, match_case->match_case.val);
    pop_scope(env);
}

void bind_expr(Env* env, AstNode* expr) {
    expr->parent_scope = env->cur_scope->ast_node;
    switch (expr->tag) {
        case AST_PATH:
            bind_path(env, expr);
            break;
        case AST_BOOL_LITERAL:
        case AST_INT_LITERAL:
        case AST_FLOAT_LITERAL:
        case AST_CHAR_LITERAL:
        case AST_STR_LITERAL:
            break;
        case AST_BREAK_EXPR:
        case AST_CONTINUE_EXPR: {
            const char* keyword = expr->tag == AST_BREAK_EXPR ? "break" : "continue";
            expr->break_expr.loop = find_enclosing_loop(env, keyword, &expr->file_loc);
            break;
        }
        case AST_RETURN_EXPR:
            expr->return_expr.fun = find_enclosing_fun(env, &expr->file_loc);
            break;
        case AST_TUPLE_EXPR:
            bind_many(env, expr->tuple_expr.args, bind_expr);
            break;
        case AST_FIELD_EXPR:
            bind_expr(env, expr->field_expr.val);
            break;
        case AST_STRUCT_EXPR:
        case AST_UPDATE_EXPR:
            bind_expr(env, expr->struct_expr.left);
            bind_many(env, expr->struct_expr.fields, bind_expr);
            break;
        case AST_CALL_EXPR:
            bind_expr(env, expr->call_expr.callee);
            bind_expr(env, expr->call_expr.arg);
            break;
        case AST_TYPED_EXPR:
            bind_expr(env, expr->typed_expr.left);
            bind_type(env, expr->typed_expr.type);
            break;
        case AST_ARRAY_EXPR:
            bind_many(env, expr->array_expr.elems, bind_expr);
            break;
#define f(name, ...) case AST_##name##_EXPR:
    AST_BINARY_EXPR_LIST(f)
#undef f
        case AST_ASSIGN_EXPR:
#define f(name, ...) case AST_##name##_ASSIGN_EXPR:
    AST_ASSIGN_EXPR_LIST(f)
#undef f
            bind_expr(env, expr->binary_expr.left);
            bind_expr(env, expr->binary_expr.right);
            break;
#define f(name, ...) case AST_##name##_EXPR:
    AST_UNARY_EXPR_LIST(f)
#undef f
            bind_expr(env, expr->unary_expr.operand);
            break;
        case AST_IF_EXPR:
            bind_expr(env, expr->if_expr.cond);
            bind_expr(env, expr->if_expr.then_expr);
            if (expr->if_expr.else_expr)
                bind_expr(env, expr->if_expr.else_expr);
            break;
        case AST_MATCH_EXPR:
            bind_expr(env, expr->match_expr.arg);
            bind_many(env, expr->match_expr.cases, bind_match_case);
            break;
        case AST_BLOCK_EXPR:
            push_scope(env, expr);
            insert_many_decls_in_env(env, expr->block_expr.stmts);
            bind_many(env, expr->block_expr.stmts, bind_stmt);
            pop_scope(env);
            break;
        case AST_MEMBER_EXPR:
            bind_expr(env, expr->member_expr.left);
            break;
        case AST_FUN_EXPR:
            push_scope(env, expr);
            bind_const_pattern(env, expr->fun_expr.param);
            bind_expr(env, expr->fun_expr.body);
            if (expr->fun_expr.ret_type)
                bind_type(env, expr->fun_expr.ret_type);
            pop_scope(env);
            break;
        default:
            assert(false && "invalid expression");
            break;
    }
}

void bind_kind(Env* env, AstNode* kind) {
    kind->parent_scope = env->cur_scope->ast_node;
    switch (kind->tag) {
        case AST_KIND_STAR:
            break;
        case AST_KIND_ARROW:
            bind_many(env, kind->arrow_kind.dom_kinds, bind_kind);
            bind_kind(env, kind->arrow_kind.codom_kind);
            break;
        case AST_PATH:
            bind_path(env, kind);
            break;
        default:
            assert(false && "invalid kind");
            break;
    }
}

static void bind_where_clause(Env* env, AstNode* where_clause) {
    bind_type(env, where_clause->where_clause.type);
}

void bind_type(Env* env, AstNode* type) {
    type->parent_scope = env->cur_scope->ast_node;
    switch (type->tag) {
        case AST_NORET_TYPE:
#define f(name, ...) case AST_TYPE_##name:
        PRIM_TYPE_LIST(f)
#undef f
            break;
        case AST_PATH:
            bind_path(env, type);
            break;
        case AST_TUPLE_TYPE:
            bind_many(env, type->tuple_type.args, bind_type);
            break;
        case AST_ARRAY_TYPE:
            bind_type(env, type->array_type.elem_type);
            break;
        case AST_FUN_TYPE:
            bind_type(env, type->fun_type.dom_type);
            bind_type(env, type->fun_type.codom_type);
            break;
        case AST_PTR_TYPE:
            bind_type(env, type->ptr_type.pointed_type);
            break;
        case AST_WHERE_TYPE:
            bind_type(env, type->where_type.path);
            bind_many(env, type->where_type.clauses, bind_where_clause);
            break;
        case AST_SIG_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
            insert_decl_in_env(env, type);
            bind_decl(env, type);
            break;
        default:
            assert(false && "invalid type");
            break;
    }
}

void bind_program(Env* env, AstNode* program) {
    bind_decl(env, program);
}
