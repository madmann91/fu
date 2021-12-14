#ifndef FU_LANG_TOKEN_H
#define FU_LANG_TOKEN_H

#include "core/log.h"

#include <stdint.h>

#define SYMBOLS(f) \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACKET, "[") \
    f(R_BRACKET, "]") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(DOT, ".") \
    f(COLON, ":") \
    f(SEMICOLON, ";") \
    f(COMMA, ",") \
    f(HASH, "#") \
    f(EQUAL, "=") \
    f(BANG, "!") \
    f(MINUS, "-") \
    f(PLUS, "+") \
    f(STAR, "*") \
    f(SLASH, "/") \
    f(PERCENT, "%") \
    f(HAT, "^") \
    f(PIPE, "|") \
    f(AMPERSAND, "&") \
    f(FAT_ARROW, "=>") \
    f(THIN_ARROW, "->") \
    f(DOUBLE_AMPERSAND, "&&") \
    f(DOUBLE_PIPE, "||") \
    f(DOUBLE_MINUS, "--") \
    f(DOUBLE_PLUS, "++") \
    f(PLUS_EQUAL, "+=") \
    f(MINUS_EQUAL, "-=") \
    f(STAR_EQUAL, "*=") \
    f(SLASH_EQUAL, "/=") \
    f(PERCENT_EQUAL, "%=") \
    f(HAT_EQUAL, "^=") \
    f(PIPE_EQUAL, "|=") \
    f(AMPERSAND_EQUAL, "&=") \
    f(BANG_EQUAL, "!=") \
    f(DOUBLE_EQUAL, "==") \
    f(GREATER, ">") \
    f(GREATER_EQUAL, ">=") \
    f(LESS_EQUAL, "<=") \
    f(LESS, "<")

#define KEYWORDS(f) \
    f(FUN, "fun") \
    f(STRUCT, "struct") \
    f(ENUM, "enum") \
    f(FILTER, "filter") \
    f(IF, "if") \
    f(ELSE, "else") \
    f(WHILE, "while") \
    f(FOR, "for") \
    f(IN, "in") \
    f(VAR, "var") \
    f(CONST, "const") \
    f(MATCH, "match") \
    f(MOD, "mod") \
    f(BOOL, "bool") \
    f(TRUE, "true") \
    f(FALSE, "false") \
    f(I8, "i8") \
    f(I16, "i16") \
    f(I32, "i32") \
    f(I64, "i64") \
    f(U8, "u8") \
    f(U16, "u16") \
    f(U32, "u32") \
    f(U64, "u64") \
    f(F32, "f32") \
    f(F64, "f64")

#define TOKENS(f) \
    f(EOF, "end of file") \
    f(IDENT, "identifier") \
    f(INT_LITERAL, "integer literal") \
    f(FLOAT_LITERAL, "floating-point literal") \
    f(CHAR_LITERAL, "character literal") \
    f(STRING_LITERAL, "string literal") \
    f(INVALID, "invalid token") \
    SYMBOLS(f) \
    KEYWORDS(f)

struct token {
    enum token_tag {
#define f(x, str) TOKEN_##x,
        TOKENS(f)
#undef f
        TOKEN_COUNT
    } tag;
    union {
        uintmax_t int_val;
        double float_val;
    };
	struct file_loc loc;
};

static inline const char* token_tag_to_string(enum token_tag tag) {
    static const char* token_strs[] = {
#define f(x, str) str,
        TOKENS(f)
#undef f
    };
    return token_strs[tag];
}

#endif
