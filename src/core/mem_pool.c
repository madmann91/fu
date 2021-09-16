#include "core/mem_pool.h"
#include "core/alloc.h"

#include <assert.h>
#include <stdlib.h>

struct mem_block {
    size_t size;
    size_t capacity;
    struct mem_block* next;
    alignas(max_align_t) char data[];
};

#define MIN_MEM_BLOCK_CAPACITY 1024

struct mem_pool new_mem_pool(void) {
    return (struct mem_pool) { NULL, NULL };
}

static size_t remaining_mem(struct mem_block* block) {
    assert(block->capacity >= block->size);
    return block->capacity - block->size;
}

static struct mem_block* alloc_mem_block(struct mem_block* prev, size_t capacity) {
    if (capacity < MIN_MEM_BLOCK_CAPACITY) capacity = MIN_MEM_BLOCK_CAPACITY;
    struct mem_block* block = malloc_or_die(sizeof(struct mem_block) + capacity);
    block->capacity = capacity;
    block->size = 0;
    block->next = NULL;
    if (prev)
        prev->next = block;
    return block;
}

static size_t align_to(size_t size, size_t align) {
    size_t offset = size % align;
    return offset != 0 ? size + align - offset : size;
}

void* alloc_from_mem_pool(struct mem_pool* mem_pool, size_t size) {
    size = align_to(size, sizeof(max_align_t));
    if (!mem_pool->cur) {
        mem_pool->first = mem_pool->cur = alloc_mem_block(NULL, size);
    } else {
        // Try to re-use the next memory pools if they are appropriately sized
        while (remaining_mem(mem_pool->cur) < size) {
            if (!mem_pool->cur->next) {
                mem_pool->cur = alloc_mem_block(mem_pool->cur, size);
                break;
            }
            mem_pool->cur = mem_pool->cur->next;
            assert(mem_pool->cur->size == 0 && "next memory pool block must have been reset");
        }
    }
    assert(remaining_mem(mem_pool->cur) >= size);
    void* ptr = ((char*)mem_pool->cur->data) + mem_pool->cur->size;
    mem_pool->cur->size += size;
    return ptr;
}

void reset_mem_pool(struct mem_pool* mem_pool) {
    struct mem_block* block = mem_pool->first;
    while (block) {
        block->size = 0;
        block = block->next;
    }
    mem_pool->cur = mem_pool->first;
}

void free_mem_pool(struct mem_pool* mem_pool) {
    struct mem_block* block = mem_pool->first;
    while (block) {
        struct mem_block* next = block->next;
        free(block);
        block = next;
    }
    mem_pool->first = mem_pool->cur = NULL;
}
