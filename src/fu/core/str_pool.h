#ifndef FU_CORE_STR_POOL_H
#define FU_CORE_STR_POOL_H

#include "fu/core/hash_table.h"

/*
 * The string pool is a hashed container for strings. Each string contained in the pool is stored
 * uniquely, which allows fast comparison (strings can then be compared by their address).
 */

typedef struct MemPool MemPool;

typedef struct {
    MemPool* mem_pool;
    HashTable hash_table;
} StrPool;

StrPool new_str_pool(MemPool*);
void free_str_pool(StrPool*);

const char* make_str(StrPool*, const char*);

#endif
