#include "core/hash_table.h"
#include "core/primes.h"
#include "core/alloc.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define OCCUPIED_MASK UINT32_C(0x80000000)
#define MAX_LOAD_FACTOR 70//%

static inline size_t increment_wrap(size_t capacity, size_t index) {
    return index + 1 >= capacity ? 0 : index + 1;
}

static inline void* elem_at(void* elems, size_t elem_size, size_t index) {
    return ((char*)elems) + elem_size * index;
}

static inline bool needs_rehash(const struct hash_table* hash_table) {
    return hash_table->size * 100 >= hash_table->capacity * MAX_LOAD_FACTOR;
}

struct hash_table new_hash_table(size_t capacity, size_t elem_size) {
    capacity = next_prime(capacity);
    void* elems = malloc_or_die(capacity * elem_size);
    uint32_t* hashes = calloc_or_die(capacity, sizeof(uint32_t));
    return (struct hash_table) {
        .elems    = elems,
        .hashes   = hashes,
        .capacity = capacity
    };
}

void free_hash_table(struct hash_table* hash_table) {
    free(hash_table->elems);
    free(hash_table->hashes);
    hash_table->capacity = hash_table->size = 0;
}

bool is_bucket_occupied(uint32_t hash) {
	return hash & OCCUPIED_MASK;
}

static inline void rehash_table(struct hash_table* hash_table, size_t elem_size) {
    size_t new_capacity = next_prime(hash_table->capacity);
    if (new_capacity <= hash_table->capacity)
        new_capacity = hash_table->capacity * 2 + 1;
    void* new_elems = malloc_or_die(new_capacity * elem_size);
    uint32_t* new_hashes = calloc_or_die(new_capacity, elem_size);
    for (size_t i = 0, n = hash_table->capacity; i < n; ++i) {
        uint32_t hash = hash_table->hashes[i];
        if (!is_bucket_occupied(hash))
            continue;
        size_t index = mod_prime(hash, new_capacity);
        while (is_bucket_occupied(new_hashes[index]))
            index = increment_wrap(new_capacity, index);

        memcpy(
            elem_at(new_elems, elem_size, index),
            elem_at(hash_table->elems, elem_size, i),
            elem_size);
        new_hashes[index] = hash;
    }
    free(hash_table->hashes);
    free(hash_table->elems);
    hash_table->hashes = new_hashes;
    hash_table->elems = new_elems;
    hash_table->capacity = new_capacity;
}

bool insert_in_hash_table(
    struct hash_table* hash_table,
    const void* elem,
    uint32_t hash,
    size_t elem_size,
    compare_fn compare)
{
	hash |= OCCUPIED_MASK;
    size_t index = mod_prime(hash, hash_table->capacity);
    while (is_bucket_occupied(hash_table->hashes[index])) {
        if (hash_table->hashes[index] == hash &&
            compare(elem_at(hash_table->elems, elem_size, index), elem))
            return false;
        index = increment_wrap(hash_table->capacity, index);
    }
    memcpy(elem_at(hash_table->elems, elem_size, index), elem, elem_size);
    hash_table->hashes[index] = hash;
    hash_table->size++;
    if (needs_rehash(hash_table))
        rehash_table(hash_table, elem_size);
    return true;
}

void* find_in_hash_table(
    const struct hash_table* hash_table,
    const void* elem,
    uint32_t hash,
    size_t elem_size,
    compare_fn compare)
{
    hash |= OCCUPIED_MASK;
    size_t index = mod_prime(hash, hash_table->capacity);
    while (is_bucket_occupied(hash_table->hashes[index])) {
        void* target_elem = elem_at(hash_table->elems, elem_size, index);
        if (hash_table->hashes[index] == hash && compare(target_elem, elem))
            return target_elem;
        index = increment_wrap(hash_table->capacity, index);
    }
    return NULL;
}

static inline size_t distance_in_bytes(const void* from, const void* to) {
    return (char*)to - (char*)from;
}

void remove_from_hash_table(struct hash_table* hash_table, void* elem, size_t elem_size) {
    assert(elem >= hash_table->elems);
    assert(elem < elem_at(hash_table->elems, elem_size, hash_table->capacity));
    size_t index = distance_in_bytes(hash_table->elems, elem) / elem_size;
    assert(is_bucket_occupied(hash_table->hashes[index]));
    size_t next_index = increment_wrap(hash_table->capacity, index);
    while (is_bucket_occupied(hash_table->hashes[next_index])) {
        uint32_t next_hash = hash_table->hashes[next_index];
        size_t desired_index = mod_prime(next_hash, hash_table->capacity);
        // If the next element is part of the collision chain, move it
        if (desired_index <= index || desired_index > next_index) {
            void* next_elem = elem_at(hash_table->elems, elem_size, next_index);
            memcpy(elem, next_elem, elem_size);
            hash_table->hashes[index] = next_hash;
            elem = next_elem;
            index = next_index;
        }
        next_index = increment_wrap(hash_table->capacity, next_index);
    }
    hash_table->hashes[index] = 0;
    hash_table->size--;
}

void clear_hash_table(struct hash_table* hash_table) {
    hash_table->size = 0;
    memset(hash_table->hashes, 0, sizeof(uint32_t) * hash_table->capacity);
}
