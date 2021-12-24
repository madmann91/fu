#ifndef FU_LANG_LEXER_H
#define FU_LANG_LEXER_H

#include "lang/token.h"
#include "core/log.h"
#include "core/hash_table.h"

typedef struct lexer {
    const char* file_name;
    const char* file_data;
    size_t file_size;
    FilePos file_pos;
    Log* log;
    HashTable keywords;
} Lexer;

struct lexer new_lexer(const char* file_name, const char* file_data, size_t file_size, Log*);
void free_lexer(Lexer*);

struct token advance_lexer(Lexer*);

#endif
