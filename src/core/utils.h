#ifndef FU_CORE_UTILS_H
#define FU_CORE_UTILS_H

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#define must_succeed(...) \
    do { \
        bool res = __VA_ARGS__; \
        assert(res && #__VA_ARGS__); \
        (void)res; \
    } while (false)

#endif
