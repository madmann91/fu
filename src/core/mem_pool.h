#ifndef FU_CORE_MEM_POOL_H
#define FU_CORE_MEM_POOL_H

#include <stdalign.h>
#include <stddef.h>

struct mem_block;

struct mem_pool {
    struct mem_block* first;
    struct mem_block* cur;
};

struct mem_pool new_mem_pool(void);
void* alloc_from_mem_pool(struct mem_pool*, size_t);
void reset_mem_pool(struct mem_pool*);
void free_mem_pool(struct mem_pool*);

char* copy_string_with_mem_pool(struct mem_pool*, const char*);
void* copy_bytes_with_mem_pool(struct mem_pool*, size_t size, const void* data, size_t data_size);

#endif
