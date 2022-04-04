#ifndef FU_LANG_TOKEN_H
#define FU_LANG_TOKEN_H

#include "fu/core/log.h"
#include "fu/lang/types.h"

#define SYMBOL_LIST(f) \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACKET, "[") \
    f(R_BRACKET, "]") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(HASH, "#") \
    f(BANG, "!") \
    f(DOT, ".") \
    f(COMMA, ",") \
    f(COLON, ":") \
    f(SEMICOLON, ";") \
    f(EQUAL, "=") \
    f(DOUBLE_EQUAL, "==") \
    f(BANG_EQUAL, "!=") \
    f(FAT_ARROW, "=>") \
    f(THIN_ARROW, "->") \
    f(LESS, "<") \
    f(LESS_EQUAL, "<=") \
    f(DOUBLE_LESS, "<<") \
    f(DOUBLE_LESS_EQUAL, "<<=") \
    f(GREATER, ">") \
    f(GREATER_EQUAL, ">=") \
    f(DOUBLE_GREATER, ">>") \
    f(DOUBLE_GREATER_EQUAL, ">>=") \
    f(PLUS, "+") \
    f(MINUS, "-") \
    f(STAR, "*") \
    f(SLASH, "/") \
    f(PERCENT, "%") \
    f(AMP, "&") \
    f(HAT, "^") \
    f(PIPE, "|") \
    f(DOUBLE_AMP, "&&") \
    f(DOUBLE_PIPE, "||") \
    f(DOUBLE_PLUS, "++") \
    f(DOUBLE_MINUS, "--") \
    f(PLUS_EQUAL, "+=") \
    f(MINUS_EQUAL, "-=") \
    f(STAR_EQUAL, "*=") \
    f(SLASH_EQUAL, "/=") \
    f(PERCENT_EQUAL, "%=") \
    f(AMP_EQUAL, "&=") \
    f(HAT_EQUAL, "^=") \
    f(PIPE_EQUAL, "|=")

#define KEYWORD_LIST(f) \
    f(TRUE, "true") \
    f(FALSE, "false") \
    f(IF, "if") \
    f(ELSE, "else") \
    f(MATCH, "match") \
    f(FOR, "for") \
    f(IN, "in") \
    f(WHILE, "while") \
    f(BREAK, "break") \
    f(RETURN, "return") \
    f(CONTINUE, "continue") \
    f(FUN, "fun") \
    f(VAR, "var") \
    f(CONST, "const") \
    f(TYPE, "type") \
    f(NAT, "nat") \
    f(STRUCT, "struct") \
    f(ENUM, "enum") \
    f(MOD, "mod") \
    f(SIG, "sig") \
    f(PUB, "pub") \
    f(OPAQUE, "opaque") \
    f(WHERE, "where") \
    f(USING, "using") \
    PRIM_TYPE_LIST(f)

#define SPECIAL_TOKEN_LIST(f) \
    f(IDENT, "identifier") \
    f(INT_LITERAL, "integer literal") \
    f(FLOAT_LITERAL, "floating-point literal") \
    f(CHAR_LITERAL, "character literal") \
    f(STR_LITERAL, "string literal") \
    f(EOF, "end of file") \
    f(ERROR, "invalid token")

#define TOKEN_LIST(f) \
    SYMBOL_LIST(f) \
    KEYWORD_LIST(f) \
    SPECIAL_TOKEN_LIST(f)

typedef enum {
#define f(name, str) TOKEN_##name,
    TOKEN_LIST(f)
#undef f
} TokenTag;

typedef struct {
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
#define g(name, str) case TOKEN_##name: return "'"str"'";
        SYMBOL_LIST(g)
        KEYWORD_LIST(g)
        SPECIAL_TOKEN_LIST(f)
#undef f
        default:
            return NULL;
    }
}

#endif
