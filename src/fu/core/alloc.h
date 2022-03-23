#ifndef FU_CORE_ALLOC_H
#define FU_CORE_ALLOC_H

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

// GCOV_EXCL_START
static inline void die(const char* msg) {
    fputs(msg, stderr);
    abort();
}
// GCOV_EXCL_STOP

static inline void* malloc_or_die(size_t size) {
    void* ptr = malloc(size);
    if (!ptr)
        die("out of memory, malloc() failed\n"); // GCOV_EXCL_LINE
    return ptr;
}

static inline void* calloc_or_die(size_t count, size_t size) {
    void* ptr = calloc(count, size);
    if (!ptr)
        die("out of memory, calloc() failed\n"); // GCOV_EXCL_LINE
    return ptr;
}

static inline void* realloc_or_die(void* ptr, size_t size) {
    ptr = realloc(ptr, size);
    if (!ptr)
        die("out of memory, realloc() failed\n"); // GCOV_EXCL_LINE
    return ptr;
}

#endif
