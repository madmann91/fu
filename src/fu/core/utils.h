#ifndef FU_CORE_UTILS_H
#define FU_CORE_UTILS_H

#include <stdbool.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include <stddef.h>
#include <assert.h>

#define must_succeed(...) \
    do { \
        bool res = __VA_ARGS__; \
        assert(res && #__VA_ARGS__); \
        (void)res; \
    } while (false)

#define IGNORE(...)
#define FORWARD(...) __VA_ARGS__
#define DEFER(x) x IGNORE()

#define  ARG_1(x, ...)  x
#define  ARG_2(x, ...)  ARG_1(__VA_ARGS__)
#define  ARG_3(x, ...)  ARG_2(__VA_ARGS__)
#define  ARG_4(x, ...)  ARG_3(__VA_ARGS__)
#define  ARG_5(x, ...)  ARG_4(__VA_ARGS__)
#define  ARG_6(x, ...)  ARG_5(__VA_ARGS__)
#define  ARG_7(x, ...)  ARG_6(__VA_ARGS__)
#define  ARG_8(x, ...)  ARG_7(__VA_ARGS__)
#define  ARG_9(x, ...)  ARG_8(__VA_ARGS__)
#define ARG_10(x, ...)  ARG_9(__VA_ARGS__)
#define ARG_11(x, ...) ARG_10(__VA_ARGS__)

#define SKIP_1(x, ...) __VA_ARGS__
#define SKIP_2(x, ...) SKIP_1(__VA_ARGS__)
#define SKIP_3(x, ...) SKIP_2(__VA_ARGS__)
#define SKIP_4(x, ...) SKIP_3(__VA_ARGS__)
#define SKIP_5(x, ...) SKIP_4(__VA_ARGS__)
#define SKIP_6(x, ...) SKIP_5(__VA_ARGS__)
#define SKIP_7(x, ...) SKIP_6(__VA_ARGS__)
#define SKIP_8(x, ...) SKIP_7(__VA_ARGS__)
#define SKIP_9(x, ...) SKIP_8(__VA_ARGS__)

size_t convert_escape_seq(const char* str, size_t n, char* res);
bool is_color_supported(FILE*);
char* read_file(const char* file_name, size_t* file_size);

static inline unsigned ilog2(uintmax_t i) {
    unsigned p = 0;
    while (i > 0) p++, i >>= 1;
    return p;
}

#endif
