#ifndef FU_CORE_STRING_POOL_H
#define FU_CORE_STRING_POOL_H

#include "core/hash_table.h"
#include "core/mem_pool.h"

struct string_pool {
    struct mem_pool mem_pool;
    struct hash_table hash_table;
};

struct string_pool new_string_pool();
void free_string_pool(struct string_pool*);
const char* make_unique_string(struct string_pool*, const char*);

#endif
