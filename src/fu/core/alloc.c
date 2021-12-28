#include "fu/core/alloc.h"

#include <stdlib.h>
#include <stdio.h>

static inline void die(const char* msg) {
    fputs(msg, stderr);
    abort();
}

void* malloc_or_die(size_t size) {
    void* ptr = malloc(size);
    if (!ptr)
        die("out of memory, malloc() failed\n");
    return ptr;
}

void* calloc_or_die(size_t count, size_t size) {
    void* ptr = calloc(count, size);
    if (!ptr)
        die("out of memory, calloc() failed\n");
    return ptr;
}

void* realloc_or_die(void* ptr, size_t size) {
    ptr = realloc(ptr, size);
    if (!ptr)
        die("out of memory, realloc() failed\n");
    return ptr;
}
