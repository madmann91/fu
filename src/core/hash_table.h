#ifndef FU_CORE_HASH_TABLE_H
#define FU_CORE_HASH_TABLE_H

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * This table only uses the lower 31 bits of the hash value.
 * The highest bit is used to encode buckets that are used.
 * Hashes are stored in the hash map to speed up comparisons:
 * The hash value is compared with the bucket's hash value first,
 * and the comparison function is only used if they compare equal.
 * The collision resolution strategy is linear probing.
 */

struct hash_table {
    size_t capacity;
    size_t size;
    uint32_t* hashes;
    void* elems;
};

typedef bool (*compare_fn) (const void*, const void*);

struct hash_table new_hash_table(size_t capacity, size_t elem_size);
void free_hash_table(struct hash_table*);

bool is_bucket_occupied(uint32_t hash);

bool insert_in_hash_table(
    struct hash_table*,
    const void* elem,
    uint32_t hash,
    size_t elem_size,
    compare_fn compare);

void* find_in_hash_table(
    struct hash_table*,
    const void* elem,
    uint32_t hash,
    size_t elem_size,
    compare_fn compare);

void remove_from_hash_table(struct hash_table*, void* elem, size_t elem_size);

void clear_hash_table(struct hash_table*);

#endif
