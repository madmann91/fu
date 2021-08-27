#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <ctype.h>

#include "ir/parse.h"
#include "ir/node.h"

#define SYMBOLS(f) \
    f(LPAREN, "(") \
    f(RPAREN, ")") \
    f(LBRACE, "{") \
    f(RBRACE, "}") \
    f(LBRACKET, "[") \
    f(RBRACKET, "]") \
    f(LANGLE, "<") \
    f(RANGLE, ">") \
    f(THINARROW, "->") \
    f(FATARROW, "=>") \
    f(DOT, ".") \
    f(COLON, ":") \
    f(SEMICOLON, ";") \
    f(COMMA, ",") \
    f(PLUS, "+") \
    f(MINUS, "-") \
    f(STAR, "*") \
    f(VBAR, "|") \
    f(EQ, "=")

#define KEYWORDS(f) \
    f(FUN, "fun")

#define TOKENS(f) \
    SYMBOLS(f) \
    KEYWORDS(f) \
    f(IDENT, "identifier") \
    f(LITERAL, "literal") \
    f(ERROR, "error") \
    f(EOF, "end-of-file")

struct literal {
    enum literal_tag {
        FLOAT_LITERAL,
        INT_LITERAL
    } tag;
    union {
        double float_val;
        uint64_t int_val;
    } data;
};

struct token {
    enum token_tag {
#define f(x, str) TOKEN_##x,
        TOKENS(f)
#undef f
    } tag;
    struct literal lit;
	struct location loc;
};

struct lexer {
    struct hash_table* keywords;
    struct position cur_pos;
    const char* data_end;
    const char* file_name;
};

struct parser {
    struct lexer lexer;
    struct token ahead;
};

static inline bool is_eof_reached(const struct lexer* lexer) {
    return lexer->cur_pos.data_ptr >= lexer->data_end;
}

static inline const char* data_ptr(const struct lexer* lexer) {
    return lexer->cur_pos.data_ptr;
}

static inline char next_char(const struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return *data_ptr(lexer);
}

static inline bool is_utf8_multibyte_begin(uint8_t first_byte) {
    return first_byte & 0x80;
}

static inline size_t count_utf8_bytes(uint8_t first_byte) {
    size_t n = 0;
    while (first_byte & 0x80) first_byte <<= 1, n++;
    return n;
}

static inline bool check_utf8_bytes(const uint8_t* ptr, size_t n) {
    if (n > 4 || n < 2)
        return false;
    for (size_t i = 1; i < n; ++i) {
        if ((ptr[i] & 0xC0) != 0x80)
            return false;
    }
    return true;
}

static inline size_t eat_utf8_bytes(const uint8_t* ptr, const uint8_t* end) {
    ptrdiff_t n = count_utf8_bytes(*ptr);
    return n <= (end - ptr) && check_utf8_bytes(ptr, n) ? n : 1;
}

static void advance_lexer(struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    if (is_utf8_multibyte_begin(next_char(lexer))) {
        size_t n = eat_utf8_bytes((uint8_t*)data_ptr(lexer), (uint8_t*)lexer->data_end);
        lexer->cur_pos.data_ptr += n;
        lexer->cur_pos.col++;
    } else if (next_char(lexer) == '\n') {
        lexer->cur_pos.data_ptr++;
        lexer->cur_pos.col = 1;
        lexer->cur_pos.row++;
    } else {
        lexer->cur_pos.data_ptr++;
        lexer->cur_pos.col++;
    }
}

static bool accept_char(struct lexer* lexer, char c) {
    if (next_char(lexer) == c) {
        advance_lexer(lexer);
        return true;
    }
    return false;
}

static inline void eat_spaces(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && isspace(next_char(lexer)))
        advance_lexer(lexer);
}

static inline void eat_single_line_comment(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && next_char(lexer) != '\n')
        advance_lexer(lexer);
}

static inline struct token make_token(const struct lexer* lexer, const struct position* begin, enum token_tag tag) {
    return (struct token) {
        .tag = tag,
        .loc = (struct location) {
            .file_name = lexer->file_name,
            .begin = *begin,
            .end = lexer->cur_pos
        }
    };
}

static struct token next_token(struct lexer* lexer) {
    while (true) {
        eat_spaces(lexer);
        if (accept_char(lexer, '#')) {
            eat_single_line_comment(lexer);
            continue;
        }

        struct position last_pos = lexer->cur_pos;
        advance_lexer(lexer);
        return make_token(lexer, &last_pos, TOKEN_ERROR);
    }
}

static inline void eat_token(struct parser* parser) {
    parser->ahead = next_token(&parser->lexer);
}

static inline bool accept_token(struct parser* parser, enum token_tag tag) {
    if (parser->ahead.tag == tag) {
        eat_token(parser);
        return true;
    }
    return false;
}

struct ir_node* parse_ir(const char* data_ptr, size_t data_size, const char* file_name) {
    struct parser parser = {
        .lexer = (struct lexer) {
            .cur_pos = (struct position) {
                .row = 1,
                .col = 1,
                .data_ptr = data_ptr
            },
            .data_end = data_ptr + data_size,
            .file_name = file_name
        }
    };
    eat_token(&parser);
    return NULL;
}
