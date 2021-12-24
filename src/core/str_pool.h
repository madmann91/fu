#ifndef FU_CORE_STR_POOL_H
#define FU_CORE_STR_POOL_H

#include "core/hash_table.h"
#include "core/mem_pool.h"

typedef struct str_pool {
    MemPool mem_pool;
    HashTable hash_table;
} StrPool;

StrPool new_str_pool();
void free_str_pool(StrPool*);

const char* make_str(StrPool*, const char*);

#endif
