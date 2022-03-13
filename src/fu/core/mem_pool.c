#include "fu/core/mem_pool.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdalign.h>

typedef struct MemBlock {
    size_t size;
    size_t capacity;
    struct MemBlock* next;
    alignas(max_align_t) char data[];
} MemBlock;

#define MIN_MEM_BLOCK_CAPACITY 1024

MemPool new_mem_pool(void) {
    return (MemPool) { NULL, NULL };
}

static size_t remaining_mem(MemBlock* block) {
    assert(block->capacity >= block->size);
    return block->capacity - block->size;
}

static MemBlock* alloc_mem_block(MemBlock* prev, size_t capacity) {
    if (capacity < MIN_MEM_BLOCK_CAPACITY) capacity = MIN_MEM_BLOCK_CAPACITY;
    MemBlock* block = malloc_or_die(sizeof(MemBlock) + capacity);
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

void* alloc_from_mem_pool(MemPool* mem_pool, size_t size) {
    if (size == 0)
        return NULL;
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

void reset_mem_pool(MemPool* mem_pool) {
    MemBlock* block = mem_pool->first;
    while (block) {
        block->size = 0;
        block = block->next;
    }
    mem_pool->cur = mem_pool->first;
}

void free_mem_pool(MemPool* mem_pool) {
    MemBlock* block = mem_pool->first;
    while (block) {
        MemBlock* next = block->next;
        free(block);
        block = next;
    }
    mem_pool->first = mem_pool->cur = NULL;
}
