#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "core/alloc.h"
#include "core/log.h"
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
	struct file_loc loc;
};

struct lexer {
    struct hash_table* keywords;
    struct file_pos cur_pos;
    const char* data_end;
    const char* file_name;
};

struct parser {
    struct log* log;
    struct lexer lexer;
    struct token ahead;
    struct mem_pool mem_pool;
    struct file_pos prev_end;
};

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

static inline bool is_eof_reached(const struct lexer* lexer) {
    return lexer->cur_pos.data_ptr >= lexer->data_end;
}

static inline char next_char(const struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return *lexer->cur_pos.data_ptr;
}

static void eat_char(struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    if (is_utf8_multibyte_begin(next_char(lexer))) {
        size_t n = eat_utf8_bytes((const uint8_t*)lexer->cur_pos.data_ptr, (const uint8_t*)lexer->data_end);
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
        eat_char(lexer);
        return true;
    }
    return false;
}

static inline void eat_spaces(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && isspace(next_char(lexer)))
        eat_char(lexer);
}

static inline void eat_single_line_comment(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && next_char(lexer) != '\n')
        eat_char(lexer);
}

static inline struct token make_token(const struct lexer* lexer, const struct file_pos* begin, enum token_tag tag) {
    return (struct token) {
        .tag = tag,
        .loc = (struct file_loc) {
            .file_name = lexer->file_name,
            .begin = *begin,
            .end = lexer->cur_pos
        }
    };
}

static inline size_t token_length(const struct token* token) {
    return token->loc.end.data_ptr - token->loc.begin.data_ptr;
}

static struct token advance_lexer(struct lexer* lexer) {
    while (true) {
        eat_spaces(lexer);
        if (is_eof_reached(lexer))
            return make_token(lexer, &lexer->cur_pos, TOKEN_EOF);

        if (accept_char(lexer, '#')) {
            eat_single_line_comment(lexer);
            continue;
        }

        struct file_pos begin = lexer->cur_pos;
        if (next_char(lexer) == '_' || isalpha(next_char(lexer))) {
            eat_char(lexer);
            while (isalnum(next_char(lexer)) || next_char(lexer) == '_')
                eat_char(lexer);
            return make_token(lexer, &begin, TOKEN_IDENT);
        }

        eat_char(lexer);
        return make_token(lexer, &begin, TOKEN_ERROR);
    }
}

static inline void eat_token(struct parser* parser, enum token_tag tag) {
    assert(parser->ahead.tag == tag);
    parser->prev_end = parser->ahead.loc.end;
    parser->ahead = advance_lexer(&parser->lexer);
}

static inline bool accept_token(struct parser* parser, enum token_tag tag) {
    if (parser->ahead.tag == tag) {
        eat_token(parser, tag);
        return true;
    }
    return false;
}

static inline const char* copy_string_from_token(struct parser* parser) {
    size_t len = parser->ahead.loc.end.data_ptr - parser->ahead.loc.begin.data_ptr;
    char* str = alloc_from_mem_pool(&parser->mem_pool, len + 1);
    memcpy(str, parser->ahead.loc.begin.data_ptr, len);
    str[len] = 0;
    return str;
}

static inline struct ir_node* make_ir_node(
    struct parser* parser,
    const struct file_pos* begin,
    enum ir_node_tag tag,
    size_t data_size)
{
    struct ir_node* node = alloc_from_mem_pool(&parser->mem_pool, sizeof(struct ir_node) + data_size);
    memset(node, 0, sizeof(struct ir_node));
    node->tag = tag;
    node->data_size = data_size;
    node->loc = (struct file_loc) {
        .file_name = parser->ahead.loc.file_name,
        .begin = *begin,
        .end = parser->prev_end
    };
    return node;
}

static struct ir_node* parse_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    const char* name = copy_string_from_token(parser);
    eat_token(parser, TOKEN_IDENT);
    struct ir_node* var = make_ir_node(parser, &begin, IR_NODE_VAR, sizeof(struct var_data));
    var->data[0].var_data.name = name;
    var->data[0].var_data.index = 0;
    return var;
}

static struct ir_node* parse_error(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, parser->ahead.tag);
    return make_ir_node(parser, &begin, IR_NODE_ERROR, 0);
}

static struct ir_node* parse(struct parser* parser) {
    switch (parser->ahead.tag) {
        case TOKEN_IDENT:
            return parse_var(parser);
        default:
            log_error(parser->log, &parser->ahead.loc, "unknown token '%sl'", (union format_arg[]) {
                { .s = parser->ahead.loc.begin.data_ptr },
                { .len = token_length(&parser->ahead) }
            });
            return parse_error(parser);
    }
}

struct ir_node* parse_ir(struct log* log, const char* data_ptr, size_t data_size, const char* file_name) {
    struct file_pos begin = {
        .row = 1,
        .col = 1,
        .data_ptr = data_ptr
    };
    struct parser parser = {
        .lexer = (struct lexer) {
            .cur_pos = begin,
            .data_end = data_ptr + data_size,
            .file_name = file_name
        },
        .log = log
    };
    parser.ahead = advance_lexer(&parser.lexer);
    parser.prev_end = begin;
    return parse(&parser);
}
