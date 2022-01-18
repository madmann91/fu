#include "fu/lang/bind.h"
#include "fu/lang/ast.h"
#include "fu/core/hash_table.h"
#include "fu/core/hash.h"
#include "fu/core/alloc.h"
#include "fu/core/log.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define DEFAULT_SCOPE_CAPACITY 8

typedef struct {
    const char* name;
    AstNode* decl_site;
} Symbol;

struct Scope {
    HashTable symbols;
    AstNode* decl_site;
    struct Scope* prev, *next;
};

static bool compare_symbols(const void* left, const void* right) {
    return !strcmp(((Symbol*)left)->name, ((Symbol*)right)->name);
}

static Scope* new_scope(Scope* prev) {
    Scope* scope = malloc_or_die(sizeof(Scope));
    scope->prev = prev;
    scope->next = NULL;
    scope->symbols = new_hash_table(DEFAULT_SCOPE_CAPACITY, sizeof(Symbol));
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
        if (scope->decl_site->tag == fst_tag || scope->decl_site->tag == snd_tag)
            return scope->decl_site;
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

static inline void push_scope(Env* env, AstNode* decl_site) {
    if (!env->cur_scope)
        env->cur_scope = env->first_scope;
    else if (env->cur_scope->next)
        env->cur_scope = env->cur_scope->next;
    else
        env->cur_scope = new_scope(env->cur_scope);
    env->cur_scope->decl_site = decl_site;
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
    const char* name = NULL;
    switch (decl->tag) {
        case AST_STRUCT_DECL: name = decl->struct_decl.name; break;
        case AST_ENUM_DECL:   name = decl->enum_decl.name; break;
        case AST_FUN_DECL:    name = decl->fun_decl.name; break;
        case AST_TYPE_DECL:   name = decl->type_decl.name; break;
        default:
            return;
    }
    insert_symbol(env, name, decl);
}

static void bind_while_loop(Env* env, AstNode* while_loop) {
    bind_expr(env, while_loop->while_loop.cond);
    push_scope(env, while_loop);
    bind_expr(env, while_loop->while_loop.body);
    pop_scope(env);
}

static void bind_for_loop(Env* env, AstNode* for_loop) {
    bind_pattern(env, for_loop->for_loop.pattern);
    bind_expr(env, for_loop->for_loop.range);
    push_scope(env, for_loop);
    bind_expr(env, for_loop->for_loop.body);
    pop_scope(env);
}

void bind_stmt(Env* env, AstNode* stmt) {
    switch (stmt->tag) {
        case AST_FUN_DECL:
        case AST_VAR_DECL:
        case AST_CONST_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
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

static void bind_type_param(Env* env, AstNode* type_param) {
    insert_symbol(env, type_param->type_param.name, type_param);
}

static void bind_type_params(Env* env, AstNode* type_params) {
    bind_many(env, type_params, bind_type_param);
}

void bind_decl(Env* env, AstNode* decl) {
    switch (decl->tag) {
        case AST_FIELD_DECL:
            bind_type(env, decl->field_decl.type);
            break;
        case AST_STRUCT_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->struct_decl.type_params);
            bind_many(env, decl->struct_decl.fields, bind_decl);
            pop_scope(env);
            break;
        case AST_OPTION_DECL:
            if (decl->option_decl.param_type)
                bind_type(env, decl->option_decl.param_type);
            break;
        case AST_ENUM_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->enum_decl.type_params);
            bind_many(env, decl->enum_decl.options, bind_decl);
            pop_scope(env);
            break;
        case AST_TYPE_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->type_decl.type_params);
            bind_type(env, decl->type_decl.aliased_type);
            pop_scope(env);
            break;
        case AST_FUN_DECL:
            push_scope(env, decl);
            bind_type_params(env, decl->fun_decl.type_params);
            bind_pattern(env, decl->fun_decl.param);
            if (decl->fun_decl.ret_type)
                bind_type(env, decl->fun_decl.ret_type);
            bind_expr(env, decl->fun_decl.body);
            pop_scope(env);
            break;
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            bind_pattern(env, decl->var_decl.pattern);
            if (decl->var_decl.init)
                bind_expr(env, decl->var_decl.init);
            break;
        default:
            assert(false && "invalid declaration");
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

void bind_pattern(Env* env, AstNode* pattern) {
    switch (pattern->tag) {
        case AST_PATH:
            if (!pattern->path.elems->next && !pattern->path.elems->path_elem.type_args)
                insert_symbol(env, pattern->path.elems->path_elem.name, pattern);
            else
                bind_path(env, pattern);
            break;
        case AST_BOOL_LITERAL:
        case AST_INT_LITERAL:
        case AST_CHAR_LITERAL:
        case AST_STR_LITERAL:
            return;
        case AST_TUPLE_PATTERN:
            bind_many(env, pattern->tuple_pattern.args, bind_pattern);
            break;
        case AST_FIELD_PATTERN:
            bind_pattern(env, pattern->field_pattern.val);
            break;
        case AST_STRUCT_PATTERN:
            bind_path(env, pattern->struct_pattern.left);
            bind_many(env, pattern->struct_pattern.fields, bind_pattern);
            break;
        case AST_CTOR_PATTERN:
            bind_path(env, pattern->ctor_pattern.path);
            bind_pattern(env, pattern->ctor_pattern.arg);
            break;
        case AST_TYPED_PATTERN:
            bind_pattern(env, pattern->typed_pattern.left);
            bind_type(env, pattern->typed_pattern.type);
            break;
        case AST_ARRAY_PATTERN:
            bind_many(env, pattern->array_pattern.elems, bind_pattern);
            break;
        default:
            assert(false && "invalid pattern");
            break;
    }
}

static void bind_match_case(Env* env, AstNode* match_case) {
    push_scope(env, match_case);
    bind_pattern(env, match_case->match_case.pattern);
    bind_expr(env, match_case->match_case.val);
    pop_scope(env);
}

void bind_expr(Env* env, AstNode* expr) {
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
            for (AstNode* decl = expr->block_expr.stmts; decl; decl = decl->next)
                insert_decl_in_env(env, decl);
            bind_many(env, expr->block_expr.stmts, bind_stmt);
            pop_scope(env);
            break;
        case AST_MEMBER_EXPR:
            bind_expr(env, expr->member_expr.left);
            break;
        case AST_FUN_EXPR:
            push_scope(env, expr);
            bind_pattern(env, expr->fun_expr.param);
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

void bind_type(Env* env, AstNode* type) {
    switch (type->tag) {
#define f(name, ...) case AST_TYPE_##name:
        AST_PRIM_TYPE_LIST(f)
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
        default:
            assert(false && "invalid type");
            break;
    }
}

void bind_program(Env* env, AstNode* program) {
    push_scope(env, program);
    for (AstNode* decl = program->program.decls; decl; decl = decl->next)
        insert_decl_in_env(env, decl);
    bind_many(env, program->program.decls, bind_decl);
    pop_scope(env);
}
