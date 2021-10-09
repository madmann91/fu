#include "core/string_pool.h"
#include "core/hash.h"
#include "core/utils.h"

#include <string.h>

#define DEFAULT_STRING_POOL_CAPACITY 1024

struct string_pool new_string_pool()
{
    return (struct string_pool) {
        .mem_pool = new_mem_pool(),
        .hash_table = new_hash_table(DEFAULT_STRING_POOL_CAPACITY, sizeof(char*))
    };
}

void free_string_pool(struct string_pool* string_pool)
{
    free_mem_pool(&string_pool->mem_pool);
    free_hash_table(&string_pool->hash_table);
}

static bool compare_strings(const void* left, const void* right) {
    return !strcmp(*(char**)left, *(char**)right);
}

const char* make_string(struct string_pool* string_pool, const char* str) {
    if (!str)
        return NULL;
    uint32_t hash = hash_string(hash_init(), str);
    char** str_ptr =
        find_in_hash_table(&string_pool->hash_table, &str, hash, sizeof(char*), compare_strings);
    if (str_ptr)
        return *str_ptr;
    size_t len = strlen(str);
    char* new_str = alloc_from_mem_pool(&string_pool->mem_pool, len + 1);
    memcpy(new_str, str, len);
    new_str[len] = 0;
    must_succeed(insert_in_hash_table(&string_pool->hash_table, &new_str, hash, sizeof(char*), compare_strings));
    return new_str;
}
