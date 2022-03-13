#include "fu/core/dyn_array.h"
#include "fu/core/alloc.h"

#include <stdalign.h>
#include <string.h>

struct DynArray {
    size_t size;
    size_t capacity;
    alignas(max_align_t) char data[];
};

DynArray* ptr_to_dyn_array(const void* ptr) {
    return (DynArray*)(((char*)ptr) - offsetof(DynArray, data));
}

void* dyn_array_to_ptr(const DynArray* dyn_array) {
    return (void*)dyn_array->data;
}

DynArray* new_dyn_array_explicit(size_t elem_size, size_t capacity) {
    DynArray* dyn_array = malloc_or_die(sizeof(DynArray) + elem_size * capacity);
    dyn_array->size = 0;
    dyn_array->capacity = capacity;
    return dyn_array;
}

size_t get_dyn_array_size_explicit(DynArray* array) {
    return array->size;
}

void push_on_dyn_array_explicit(DynArray* array, const void* elem, size_t elem_size) {
    if (array->size >= array->capacity) {
        array->capacity *= 2;
        array = realloc_or_die(array, sizeof(DynArray) + elem_size * array->capacity);
    }
    memcpy(array->data + elem_size * array->size, elem, elem_size);
    array->size++;
}

void pop_from_dyn_array_explicit(DynArray* array) {
    array->size--;
}

void free_dyn_array_explicit(DynArray* array) {
    free(array);
}
