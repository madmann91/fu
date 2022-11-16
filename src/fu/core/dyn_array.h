#ifndef FU_CORE_DYN_ARRAY_H
#define FU_CORE_DYN_ARRAY_H

#include <stddef.h>

/*
 * Dynamically-growing array implementation.
 * In debug mode, assertions check that the array is not used with the wrong element type.
 */

#define push_on_dyn_array(array, ...) \
    push_on_dyn_array_explicit(array, (__VA_ARGS__), sizeof(*(__VA_ARGS__)))
#define new_dyn_array_from_data(begin, size) \
    new_dyn_array_from_data_explicit(begin, size, sizeof(*(begin)));

typedef struct DynArray {
    size_t size;
    size_t elem_size;
    size_t capacity;
    void* elems;
} DynArray;

DynArray new_dyn_array(size_t elem_size);
DynArray new_dyn_array_with_size(size_t elem_size, size_t size);
DynArray new_dyn_array_from_data_explicit(void*, size_t, size_t);
void push_on_dyn_array_explicit(DynArray*, const void*, size_t);
const void* pop_from_dyn_array(DynArray*);
void resize_dyn_array(DynArray*, size_t);
void clear_dyn_array(DynArray*);
void free_dyn_array(DynArray*);

#endif
