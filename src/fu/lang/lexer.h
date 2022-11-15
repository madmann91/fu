#ifndef FU_LANG_LEXER_H
#define FU_LANG_LEXER_H

#include "fu/lang/token.h"
#include "fu/core/file_loc.h"
#include "fu/core/hash_table.h"

/*
 * The lexer requires to have the entire file data in memory (or a memory mapped file, if needs be),
 * and produces tokens one at a time. The file data must be terminated by a null character.
 */

typedef struct Log Log;

typedef struct Lexer {
    const char* file_name;
    const char* file_data;
    size_t file_size;
    FilePos file_pos;
    Log* log;
    HashTable keywords;
} Lexer;

Lexer new_lexer(const char* file_name, const char* file_data, size_t file_size, Log*);
void free_lexer(Lexer*);

Token advance_lexer(Lexer*);

#endif
