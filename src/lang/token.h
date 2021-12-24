#ifndef FU_LANG_TOKEN_H
#define FU_LANG_TOKEN_H

#include "core/log.h"

#define SYMBOL_LIST(f) \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACKET, "[") \
    f(R_BRACKET, "]") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(SLASH, "/") \
    f(LESS_EQUAL, "<=") \
    f(LESS, "<") \
    f(COMMA, ",")

#define KEYWORD_LIST(f) \
    f(IF, "if") \
    f(ELSE, "else") \
    f(WHILE, "while") \
    f(FUN, "fun")

#define TOKEN_LIST(f) \
    SYMBOL_LIST(f) \
    KEYWORD_LIST(f) \
    f(IDENT, "identifier") \
    f(INT_LITERAL, "integer literal") \
    f(FLOAT_LITERAL, "floating-point literal") \
    f(CHAR_LITERAL, "char literal") \
    f(STRING_LITERAL, "string literal") \
    f(EOF, "end of file") \
    f(ERROR, "invalid token")

typedef enum token_tag {
#define f(name, str) TOKEN_##name,
    TOKEN_LIST(f)
#undef f
} TokenTag;

typedef struct token {
    TokenTag tag;
    union {
        uintmax_t int_val;
        double float_val;
        char char_val;
    };
    FileLoc file_loc;
} Token;

static inline const char* token_tag_to_str(TokenTag tag) {
    switch (tag) {
#define f(name, str) case TOKEN_##name: return str;
        TOKEN_LIST(f)
#undef f
        default:
            return NULL;
    }
}

#endif
