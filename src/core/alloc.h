#ifndef FU_CORE_ALLOC_H
#define FU_CORE_ALLOC_H

void* malloc_or_die(size_t);
void* calloc_or_die(size_t, size_t);
void* realloc_or_die(void*, size_t);

#endif
