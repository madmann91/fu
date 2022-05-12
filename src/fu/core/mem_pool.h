#ifndef FU_CORE_MEM_POOL_H
#define FU_CORE_MEM_POOL_H

#include <stddef.h>

/*
 * The memory pool is a block-based allocator that allocates blocks of memory of fixed size
 * to hold the allocated data. The blocks are reclaimed when the memory pool is destroyed.
 */

struct MemBlock;

typedef struct MemPool {
    struct MemBlock* first;
    struct MemBlock* cur;
} MemPool;

MemPool new_mem_pool(void);
void* alloc_from_mem_pool(MemPool*, size_t);
void reset_mem_pool(MemPool*);
void free_mem_pool(MemPool*);

#endif
