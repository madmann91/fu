#ifndef FU_CORE_UTILS_H
#define FU_CORE_UTILS_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <stdnoreturn.h>

// GCOV_EXCL_START
static noreturn inline void die(const char* msg) {
    fputs(msg, stderr);
    abort();
}
// GCOV_EXCL_STOP

static inline unsigned ilog2(uintmax_t i) {
    unsigned p = 0;
    while (i > 0) p++, i >>= 1;
    return p;
}

size_t convert_escape_seq(const char* str, size_t n, char* res);
bool is_color_supported(FILE*);
char* read_file(const char* file_name, size_t* file_size);

#endif
