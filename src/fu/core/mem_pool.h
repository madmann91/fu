#ifndef FU_CORE_MEM_POOL_H
#define FU_CORE_MEM_POOL_H

#include <stdalign.h>
#include <stddef.h>

struct MemBlock;

typedef struct {
    struct MemBlock* first;
    struct MemBlock* cur;
} MemPool;

MemPool new_mem_pool(void);
void* alloc_from_mem_pool(MemPool*, size_t);
void reset_mem_pool(MemPool*);
void free_mem_pool(MemPool*);

#endif
