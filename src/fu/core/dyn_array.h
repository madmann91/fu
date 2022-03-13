#ifndef FU_CORE_DYN_ARRAY_H
#define FU_CORE_DYN_ARRAY_H

#include <stddef.h>

#define DEFAULT_DYN_ARRAY_CAPACITY 4

typedef struct DynArray DynArray;

#define new_dyn_array(T) dyn_array_to_ptr(new_dyn_array_explicit(sizeof(T), DEFAULT_DYN_ARRAY_CAPACITY))
#define get_dyn_array_size(ptr) get_dyn_array_size_explicit(ptr_to_dyn_array(ptr))
#define pop_from_dyn_array(ptr) pop_from_dyn_array_explicit(ptr_to_dyn_array(ptr))
#define push_on_dyn_array(ptr, ...) \
    push_on_dyn_array_explicit(ptr_to_dyn_array(ptr), (__VA_ARGS__), sizeof(*(__VA_ARGS__)))
#define free_dyn_array(ptr) free_dyn_array_explicit(ptr_to_dyn_array(ptr))

DynArray* ptr_to_dyn_array(const void*);
void* dyn_array_to_ptr(const DynArray*);
DynArray* new_dyn_array_explicit(size_t, size_t);
size_t get_dyn_array_size_explicit(DynArray*);
void push_on_dyn_array_explicit(DynArray*, const void*, size_t);
void pop_from_dyn_array_explicit(DynArray*);
void free_dyn_array_explicit(DynArray*);

#endif
