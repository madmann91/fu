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
    AstNode* ast_node;
} Symbol;

struct Scope {
    HashTable symbols;
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

static void insert_symbol(Env* env, const char* name, AstNode* ast_node) {
    assert(env->cur_scope);
    bool was_inserted = insert_in_hash_table(&env->cur_scope->symbols,
        &(Symbol) { .name = name, .ast_node = ast_node },
        hash_str(hash_init(), name), sizeof(Symbol),
        compare_symbols);
    if (!was_inserted)
        log_error(env->log, &ast_node->file_loc, "redefinition of symbol '{s}'", (FormatArg[]) { { .s = name } });
}

static AstNode* find_symbol(Env* env, const char* name) {
    uint32_t hash = hash_str(hash_init(), name);
    for (Scope* scope = env->cur_scope; scope; scope = scope->prev) {
        Symbol* symbol = find_in_hash_table(&scope->symbols,
            &(Symbol) { .name = name },
            hash, sizeof(Symbol),
            compare_symbols);
        if (symbol)
            return symbol->ast_node;
    }
    return NULL;
}

static void push_scope(Env* env) {
    if (!env->cur_scope)
        env->cur_scope = env->first_scope;
    else if (env->cur_scope->next)
        env->cur_scope = env->cur_scope->next;
    else
        env->cur_scope = new_scope(env->cur_scope);
    clear_hash_table(&env->cur_scope->symbols);
}

static void pop_scope(Env* env) {
    assert(env->cur_scope);
    env->cur_scope = env->cur_scope->prev;
}

// TODO
void bind_stmt(Env*, AstNode*);

void bind_decl(Env* env, AstNode* decl) {
}

// TODO
void bind_pattern(Env*, AstNode*);
void bind_expr(Env*, AstNode*);
void bind_type(Env*, AstNode*);

static void insert_decl_in_env(Env* env, AstNode* decl) {
    const char* name = NULL;
    switch (decl->tag) {
        case AST_STRUCT_DECL: name = decl->struct_decl.name; break;
        case AST_ENUM_DECL:   name = decl->struct_decl.name; break;
        case AST_FUN_DECL:    name = decl->fun_decl.name; break;
        case AST_TYPE_DECL:   name = decl->type_decl.name; break;
        default:
            return;
    }
    insert_symbol(env, name, decl);
}

void bind_program(Env* env, AstNode* program) {
    push_scope(env);
    for (AstNode* decl = program->program.decls; decl; decl = decl->next)
        insert_decl_in_env(env, decl);
    for (AstNode* decl = program->program.decls; decl; decl = decl->next)
        bind_decl(env, decl);
    pop_scope(env);
}
