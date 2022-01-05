#include "fu/core/str_pool.h"
#include "fu/core/mem_pool.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"

#include <string.h>

#define DEFAULT_STRING_POOL_CAPACITY 1024

StrPool new_str_pool(MemPool* mem_pool) {
    return (StrPool) {
        .mem_pool = mem_pool,
        .hash_table = new_hash_table(DEFAULT_STRING_POOL_CAPACITY, sizeof(char*))
    };
}

void free_str_pool(StrPool* str_pool) {
    free_hash_table(&str_pool->hash_table);
}

static bool compare_strs(const void* left, const void* right) {
    return !strcmp(*(char**)left, *(char**)right);
}

const char* make_str(StrPool* str_pool, const char* str) {
    if (!str)
        return NULL;
    uint32_t hash = hash_str(hash_init(), str);
    char** str_ptr =
        find_in_hash_table(&str_pool->hash_table, &str, hash, sizeof(char*), compare_strs);
    if (str_ptr)
        return *str_ptr;
    size_t len = strlen(str);
    char* new_str = alloc_from_mem_pool(str_pool->mem_pool, len + 1);
    memcpy(new_str, str, len);
    new_str[len] = 0;
    must_succeed(insert_in_hash_table(&str_pool->hash_table, &new_str, hash, sizeof(char*), compare_strs));
    return new_str;
}
