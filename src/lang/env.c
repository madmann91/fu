#include "lang/env.h"
#include "core/alloc.h"
#include "core/hash_table.h"
#include "core/hash.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define DEFAULT_ENV_CAPACITY 7

struct symbol {
    const char* name;
    struct ast* ast;
};

struct env {
    struct hash_table symbols;
    struct env* prev;
    struct env* next;
};

static struct env* new_env(struct env* prev, struct env* next) {
    struct env* env = malloc_or_die(sizeof(struct env));
    env->symbols = new_hash_table(DEFAULT_ENV_CAPACITY, sizeof(struct symbol));
    env->prev = prev;
    env->next = next;
    return env;
}

void free_env(struct env* env) {
    if (!env) return;
    while (env->prev) env = env->prev;
    while (env) {
        struct env* next = env->next;
        free_hash_table(&env->symbols);
        free(env);
        env = next;
    }
}

void push_env(struct env** env_ptr) {
    if (!*env_ptr)
        *env_ptr = new_env(NULL, NULL);
    else {
        if (!(*env_ptr)->next)
            (*env_ptr)->next = new_env(*env_ptr, NULL);
        *env_ptr = (*env_ptr)->next;
    }
    clear_hash_table(&(*env_ptr)->symbols);
}

void pop_env(struct env** env_ptr) {
    assert(*env_ptr && (*env_ptr)->prev);
    *env_ptr = (*env_ptr)->prev;
}

static bool compare_symbols(const void* left, const void* right) {
    return !strcmp(((struct symbol*)left)->name, ((struct symbol*)right)->name);
}

bool add_symbol(struct env* env, const char* name, struct ast* ast) {
    struct symbol symbol = { .name = name, .ast = ast };
    uint32_t hash = hash_string(hash_init(), name);
    return insert_in_hash_table(&env->symbols, &symbol, hash, sizeof(struct symbol), compare_symbols);
}

struct ast* find_symbol(struct env* env, const char* name) {
    while (env) {
        uint32_t hash = hash_string(hash_init(), name);
        struct symbol* symbol = find_in_hash_table(&env->symbols,
            &(struct symbol) { .name = name },
            hash, sizeof(struct symbol), compare_symbols);
        if (symbol)
            return symbol->ast;
        env = env->prev;
    }
    return NULL;
}
