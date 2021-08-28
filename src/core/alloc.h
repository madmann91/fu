#ifndef FU_CORE_ALLOC_H
#define FU_CORE_ALLOC_H

#include <stddef.h>
#include <stdalign.h>

struct mem_block;

struct mem_pool {
    struct mem_block* first;
    struct mem_block* cur;
};

void* malloc_or_die(size_t);
void* calloc_or_die(size_t, size_t);
void* realloc_or_die(void*, size_t);

void* alloc_from_mem_pool(struct mem_pool*, size_t);
void reset_mem_pool(struct mem_pool*);
void free_mem_pool(struct mem_pool*);

#endif
