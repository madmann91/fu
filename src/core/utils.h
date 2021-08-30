#ifndef FU_CORE_UTILS_H
#define FU_CORE_UTILS_H

#include <stdbool.h>

#define must_succeed(...) \
    { \
        bool res = __VA_ARGS__; \
        assert(res && #__VA_ARGS__); \
        (void)res; \
    }

#endif
