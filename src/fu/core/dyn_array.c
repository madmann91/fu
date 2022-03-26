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
    if (array->size >= array->capacity) {
        array->capacity *= 2;
        array = realloc_or_die(array, sizeof(DynArray) + elem_size * array->capacity);
    }
    memcpy(array->data + elem_size * array->size, elem, elem_size);
    array->size++;
}

void pop_from_dyn_array(void* ptr) {
    ptr_to_dyn_array(ptr)->size--;
}

void resize_dyn_array(void* ptr, size_t size) {
    ptr_to_dyn_array(ptr)->size = size;
}

void clear_dyn_array(void* ptr) {
    resize_dyn_array(ptr, 0);
}

void free_dyn_array(void* ptr) {
    free(ptr_to_dyn_array(ptr));
}
