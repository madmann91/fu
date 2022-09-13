#include "fu/core/str_pool.h"
#include "fu/core/mem_pool.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"

#include <string.h>
#include <assert.h>

StrPool new_str_pool(MemPool* mem_pool) {
    return (StrPool) {
        .mem_pool = mem_pool,
        .hash_table = new_hash_table(sizeof(char*))
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
    if (!insert_in_hash_table(&str_pool->hash_table, &new_str, hash, sizeof(char*), compare_strs))
        assert(false && "cannot insert string in string pool");
    return new_str;
}
