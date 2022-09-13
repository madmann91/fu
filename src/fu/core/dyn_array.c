#include "fu/core/dyn_array.h"
#include "fu/core/alloc.h"

#include <string.h>
#include <assert.h>

#define DEFAULT_CAPACITY 4

static inline DynArray new_dyn_array_with_capacity(size_t elem_size, size_t capacity) {
    void* elems = malloc_or_die(elem_size * capacity);
    return (DynArray) {
        .capacity = capacity,
        .elem_size = elem_size,
        .elems = elems
    };
}

DynArray new_dyn_array(size_t elem_size) {
    return new_dyn_array_with_capacity(elem_size, DEFAULT_CAPACITY);
}

DynArray new_dyn_array_with_size(size_t elem_size, size_t size) {
    DynArray array = new_dyn_array_with_capacity(elem_size, size);
    array.size = size;
    return array;
}

DynArray new_dyn_array_from_data_explicit(void* begin, size_t size, size_t elem_size) {
    DynArray array = new_dyn_array_with_size(elem_size, size);
    memcpy(array.elems, begin, size * elem_size);
    return array;
}

static void grow_dyn_array(DynArray* array, size_t capacity) {
    size_t double_capacity = array->capacity * 2;
    array->capacity = double_capacity > capacity ? double_capacity : capacity;
    array->elems = realloc_or_die(array->elems, array->elem_size * array->capacity);
}

void push_on_dyn_array_explicit(DynArray* array, const void* elem, size_t elem_size) {
    assert(elem_size == array->elem_size);
    if (array->size >= array->capacity)
        grow_dyn_array(array, array->size + 1);
    memcpy((char*)array->elems + array->elem_size * array->size, elem, array->elem_size);
    array->size++;
}

void resize_dyn_array_explicit(DynArray* array, size_t size) {
    if (size > array->capacity)
        grow_dyn_array(array, size);
    array->size = size;
}

void free_dyn_array(DynArray* array) {
    free(array->elems);
    memset(array, 0, sizeof(DynArray));
}
