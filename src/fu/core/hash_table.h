#ifndef FU_CORE_HASH_TABLE_H
#define FU_CORE_HASH_TABLE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#include "fu/core/hash.h"
#include "fu/core/utils.h"

/*
 * This table only uses the lower bits of the hash value.
 * The highest bit is used to encode buckets that are used.
 * Hashes are stored in the hash map to speed up comparisons:
 * The hash value is compared with the bucket's hash value first,
 * and the comparison function is only used if they compare equal.
 * The collision resolution strategy is linear probing.
 */

typedef struct {
    size_t capacity;
    size_t size;
    HashCode* hashes;
    void* elems;
} HashTable;

HashTable new_hash_table(size_t elem_size);
HashTable new_hash_table_with_capacity(size_t elem_size, size_t capacity);
void free_hash_table(HashTable*);

bool is_bucket_occupied(const HashTable*, size_t);

bool insert_in_hash_table(HashTable*,
    const void* elem,
    HashCode hash,
    size_t elem_size,
    bool (*compare)(const void*, const void*));

void* find_in_hash_table(
    const HashTable*,
    const void* elem,
    HashCode hash,
    size_t elem_size,
    bool (*compare)(const void*, const void*));

void remove_from_hash_table(HashTable*, void* elem, size_t elem_size);
void clear_hash_table(HashTable*);

#endif
