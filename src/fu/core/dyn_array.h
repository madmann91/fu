#ifndef FU_CORE_DYN_ARRAY_H
#define FU_CORE_DYN_ARRAY_H

#include <stddef.h>

/*
 * Dynamically-growing array implementation. The design is such that the
 * returned array can be manipulated just like a regular pointer.
 */

typedef struct DynArray DynArray;

#define push_on_dyn_array(ptr, ...) \
    push_on_dyn_array_explicit(ptr, (__VA_ARGS__), sizeof(ptr[0]))
#define resize_dyn_array(ptr, size) \
    resize_dyn_array_explicit(ptr, size, sizeof(ptr[0]))
#define copy_dyn_array(dst, src) \
    copy_dyn_array_explicit(dst, src, sizeof(src[0]))

void* new_dyn_array(size_t elem_size);
size_t get_dyn_array_size(const void*);
void push_on_dyn_array_explicit(void*, const void*, size_t);
void pop_from_dyn_array(void*);
void resize_dyn_array_explicit(void*, size_t, size_t);
void copy_dyn_array_explicit(void*, const void*, size_t);
void clear_dyn_array(void*);
void free_dyn_array(void*);

#endif
