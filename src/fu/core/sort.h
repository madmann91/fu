#ifndef FU_CORE_SORT_H
#define FU_CORE_SORT_H

#include <stddef.h>

#define DECLARE_SHELL_SORT(name, T, is_less_than) \
    static inline void name(T* p, size_t n) { \
        size_t gaps[] = { 701, 301, 132, 57, 23, 10, 4, 1 }; \
        for (size_t k = 0; k < sizeof(gaps) / sizeof(gaps[0]); ++k) { \
            size_t gap = gaps[k]; \
            for (size_t i = gap; i < n; ++i) { \
                T e = p[i]; \
                size_t j = i; \
                for (; j >= gap && is_less_than(&e, &p[j - gap]); j -= gap) \
                    p[j] = p[j - gap]; \
                p[j] = e; \
            } \
        } \
    }

#endif
