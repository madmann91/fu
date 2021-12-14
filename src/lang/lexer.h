#ifndef FU_LANG_LEXER_H
#define FU_LANG_LEXER_H

#include "core/hash_table.h"
#include "core/log.h"
#include "lang/token.h"

#include <stddef.h>
#include <stdbool.h>

struct lexer {
    struct log* log;
    struct hash_table keywords;
    struct file_pos cur_pos;
    const char* data;
    size_t data_size;
    const char* file_name;
};

struct lexer new_lexer(
    const char* file_name,
    const char* file_data,
    size_t data_size,
    struct log* log);

void free_lexer(struct lexer*);

bool is_eof_reached(const struct lexer*);
struct token advance_lexer(struct lexer*);

#endif
