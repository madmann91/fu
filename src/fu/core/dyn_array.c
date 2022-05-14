#include "fu/core/dyn_array.h"
#include "fu/core/alloc.h"

#include <stdalign.h>
#include <string.h>

struct DynArray {
    size_t size;
    size_t capacity;
    alignas(max_align_t) char data[];
};

static DynArray* ptr_to_dyn_array(const void* ptr) {
    return (DynArray*)(((char*)ptr) - offsetof(DynArray, data));
}

static void* dyn_array_to_ptr(const DynArray* dyn_array) {
    return (void*)dyn_array->data;
}

void* new_dyn_array(size_t elem_size) {
    static const size_t capacity = 4;
    DynArray* dyn_array = malloc_or_die(sizeof(DynArray) + elem_size * capacity);
    dyn_array->size = 0;
    dyn_array->capacity = capacity;
    return dyn_array_to_ptr(dyn_array);
}

size_t get_dyn_array_size(const void* ptr) {
    return ptr_to_dyn_array(ptr)->size;
}

void push_on_dyn_array_explicit(void* ptr, const void* elem, size_t elem_size) {
    DynArray* array = ptr_to_dyn_array(ptr);
    size_t index = array->size;
    resize_dyn_array_explicit(ptr, index + 1, elem_size);
    memcpy(array->data + elem_size * index, elem, elem_size);
}

void pop_from_dyn_array(void* ptr) {
    ptr_to_dyn_array(ptr)->size--;
}

void resize_dyn_array_explicit(void* ptr, size_t size, size_t elem_size) {
    DynArray* array = ptr_to_dyn_array(ptr);
    if (size > array->capacity) {
        size_t double_capacity = array->capacity * 2;
        array->capacity = double_capacity > size ? double_capacity : size;
        array = realloc_or_die(array, sizeof(DynArray) + elem_size * array->capacity);
    }
    array->size = size;
}

void copy_dyn_array_explicit(void* dst, const void* src, size_t elem_size) {
    size_t size = get_dyn_array_size(src);
    resize_dyn_array_explicit(dst, size, elem_size);
    memcpy(ptr_to_dyn_array(dst)->data, ptr_to_dyn_array(src)->data, elem_size * size);
}

void clear_dyn_array(void* ptr) {
    ptr_to_dyn_array(ptr)->size = 0;
}

void free_dyn_array(void* ptr) {
    free(ptr_to_dyn_array(ptr));
}
