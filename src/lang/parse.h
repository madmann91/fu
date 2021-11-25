#ifndef FU_LANG_PARSE_H
#define FU_LANG_PARSE_H

#include <stddef.h>

struct ast;
struct log;
struct mem_pool;

struct ast* parse_ast(
    struct mem_pool* mem_pool,
    const char* file_name,
    const char* file_data,
    size_t data_size,
    struct log* log);

#endif
