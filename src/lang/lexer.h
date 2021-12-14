#ifndef FU_LANG_LEXER_H
#define FU_LANG_LEXER_H

#include "lang/token.h"
#include "core/hash_table.h"

struct log;

struct lexer {
    const char* file_name;
    struct file_pos file_pos;
    const char* file_data;
    size_t file_size;
    struct log* log;
    struct hash_table keywords;
};

struct lexer new_lexer(const char* file_name, const char* file_data, size_t file_size, struct log*);
void free_lexer(struct lexer*);

struct token advance_lexer(struct lexer*);

#endif
