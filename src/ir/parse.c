#include "core/utils.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/hash_table.h"
#include "core/hash.h"
#include "core/log.h"
#include "ir/parse.h"
#include "ir/node.h"
#include "ir/module.h"

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <ctype.h>

#define SYMBOLS(f) \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACE, "{") \
    f(R_BRACE, "}") \
    f(L_BRACKET, "[") \
    f(R_BRACKET, "]") \
    f(L_ANGLE, "<") \
    f(R_ANGLE, ">") \
    f(THIN_ARROW, "->") \
    f(FAT_ARROW, "=>") \
    f(DOT, ".") \
    f(COLON, ":") \
    f(SEMICOLON, ";") \
    f(COMMA, ",") \
    f(PLUS, "+") \
    f(SLASH, "/") \
    f(BACK_SLASH, "\\") \
    f(MINUS, "-") \
    f(STAR, "*") \
    f(HASH, "#") \
    f(VERT_BAR, "|") \
    f(EQUAL, "=")

#define KEYWORDS(f) \
    f(FUN, "fun") \
    f(LET, "let") \
    f(IN, "in") \
    f(INT, "int") \
    f(FLOAT, "float") \
    f(NAT, "nat")

#define TOKENS(f) \
    KEYWORDS(f) \
    SYMBOLS(f) \
    f(IDENT, "identifier") \
    f(LITERAL, "literal") \
    f(ERROR, "error") \
    f(EOF, "end-of-file")

enum {
#define f(x, str) KEYWORD_##x,
    KEYWORDS(f)
#undef f
    KEYWORD_COUNT
} tag;

struct literal {
    enum literal_tag {
        LITERAL_FLOAT,
        LITERAL_INT
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
        TOKEN_COUNT
    } tag;
    struct literal lit;
	struct file_loc loc;
};

struct keyword {
    const char* name;
    unsigned len;
    enum token_tag tag;
};

struct lexer {
    struct log* log;
    struct hash_table keywords;
    struct file_pos cur_pos;
    const char* data;
    size_t data_size;
    const char* file_name;
};

struct parser {
    struct token ahead;
    struct file_pos prev_end;
    struct ir_module* module;
    struct mem_pool* mem_pool;
    struct hash_table env;
    struct lexer lexer;
};

static inline const char* token_tag_to_string(enum token_tag tag) {
    static const char* token_strs[] = {
#define f(x, str) str,
        TOKENS(f)
#undef f
    };
    return token_strs[tag];
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

static inline bool is_eof_reached(const struct lexer* lexer) {
    return lexer->cur_pos.byte_offset >= lexer->data_size;
}

static inline char next_char(const struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return lexer->data[lexer->cur_pos.byte_offset];
}

static void eat_char(struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    if (is_utf8_multibyte_begin(next_char(lexer))) {
        size_t n = eat_utf8_bytes(
            (const uint8_t*)(lexer->data + lexer->cur_pos.byte_offset),
            (const uint8_t*)(lexer->data + lexer->data_size));
        lexer->cur_pos.byte_offset += n;
        lexer->cur_pos.col++;
    } else if (next_char(lexer) == '\n') {
        lexer->cur_pos.byte_offset++;
        lexer->cur_pos.col = 1;
        lexer->cur_pos.row++;
    } else {
        lexer->cur_pos.byte_offset++;
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

static inline void eat_multiline_comment(struct lexer* lexer, const struct file_pos* begin) {
    while (true) {
        while (!is_eof_reached(lexer) && next_char(lexer) != '*')
            eat_char(lexer);
        if (is_eof_reached(lexer)) {
            struct file_loc loc = {
                .file_name = lexer->file_name,
                .begin = *begin,
                .end = lexer->cur_pos
            };
            log_error(lexer->log, &loc, "unterminated multiline comment", NULL);
            break;
        }
        if (accept_char(lexer, '*') && accept_char(lexer, '/'))
            break;
    }
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

static bool compare_keywords(const void* left, const void* right) {
    return
        ((struct keyword*)left)->len == ((struct keyword*)right)->len &&
        !memcmp(((struct keyword*)left)->name, ((struct keyword*)right)->name, ((struct keyword*)left)->len);
}

static void register_keywords(struct lexer* lexer) {
#define f(x, str) \
    must_succeed(insert_in_hash_table( \
        &lexer->keywords, \
        &(struct keyword) { .name = token_tag_to_string(TOKEN_##x), .len = strlen(str), .tag = TOKEN_##x }, \
        hash_string(hash_init(), str), \
        sizeof(struct keyword), \
        compare_keywords));
    KEYWORDS(f)
#undef f
}

static struct token advance_lexer(struct lexer* lexer) {
    while (true) {
        eat_spaces(lexer);
        if (is_eof_reached(lexer))
            return make_token(lexer, &lexer->cur_pos, TOKEN_EOF);

        struct file_pos begin = lexer->cur_pos;

        if (accept_char(lexer, '/')) {
            if (accept_char(lexer, '/')) eat_single_line_comment(lexer);
            else if (accept_char(lexer, '*')) eat_multiline_comment(lexer, &begin);
            else return make_token(lexer, &begin, TOKEN_SLASH);
            continue;
        }

        if (accept_char(lexer, '=')) return make_token(lexer, &begin, TOKEN_EQUAL);
        if (accept_char(lexer, '#')) return make_token(lexer, &begin, TOKEN_HASH);
        if (accept_char(lexer, ',')) return make_token(lexer, &begin, TOKEN_COMMA);

        if (next_char(lexer) == '_' || isalpha(next_char(lexer))) {
            eat_char(lexer);
            while (isalnum(next_char(lexer)) || next_char(lexer) == '_')
                eat_char(lexer);
            size_t len = lexer->cur_pos.byte_offset - begin.byte_offset;
            struct keyword* keyword = find_in_hash_table(
                &lexer->keywords,
                &(struct keyword) { .name = lexer->data + begin.byte_offset, .len = len },
                hash_raw_bytes(hash_init(), lexer->data + begin.byte_offset, len),
                sizeof(struct keyword), compare_keywords);
            return make_token(lexer, &begin, keyword ? keyword->tag : TOKEN_IDENT);
        }

        if (isdigit(next_char(lexer))) {
            while (isdigit(next_char(lexer)))
                eat_char(lexer);
            bool has_dot = false;
            if (accept_char(lexer, '.')) {
                has_dot = true;
                while (isdigit(next_char(lexer)))
                    eat_char(lexer);
            }
            if (accept_char(lexer, 'e')) {
                if (!accept_char(lexer, '+'))
                    accept_char(lexer, '-');
                while (isdigit(next_char(lexer)))
                    eat_char(lexer);
            }
            struct token token = make_token(lexer, &begin, TOKEN_LITERAL);
            if (has_dot) {
                token.lit.tag = LITERAL_FLOAT;
                token.lit.data.float_val = strtod(lexer->data + begin.byte_offset, NULL);
            } else {
                token.lit.tag = LITERAL_INT;
                token.lit.data.int_val = strtoumax(lexer->data + begin.byte_offset, NULL, 10);
            }
            return token;
        }

        eat_char(lexer);
        struct file_loc loc = {
            .file_name = lexer->file_name,
            .begin = begin,
            .end = lexer->cur_pos
        };
        log_error(lexer->log, &loc, "unknown token '{sl}'", (union format_arg[]) {
            { .s = lexer->data + begin.byte_offset },
            { .len = lexer->cur_pos.byte_offset - begin.byte_offset }
        });
        return make_token(lexer, &begin, TOKEN_ERROR);
    }
}

static inline void skip_token(struct parser* parser) {
    parser->prev_end = parser->ahead.loc.end;
    parser->ahead = advance_lexer(&parser->lexer);
}

static inline void eat_token(struct parser* parser, enum token_tag tag) {
    assert(parser->ahead.tag == tag);
    skip_token(parser);
}

static inline bool accept_token(struct parser* parser, enum token_tag tag) {
    assert(tag != TOKEN_EOF && tag != TOKEN_ERROR);
    if (parser->ahead.tag == tag) {
        eat_token(parser, tag);
        return true;
    }
    return false;
}

static inline size_t byte_span(const struct file_loc* loc) {
    return loc->end.byte_offset - loc->begin.byte_offset;
}

static inline void expect_token(struct parser* parser, enum token_tag tag) {
    if (!accept_token(parser, tag)) {
        bool is_special = tag == TOKEN_IDENT || tag == TOKEN_LITERAL;
        log_error(
            parser->lexer.log, &parser->ahead.loc,
            is_special ? "expected {s}, but got '{sl}'" : "expected '{s}', but got '{sl}'",
            (union format_arg[]) {
                { .s = token_tag_to_string(tag) },
                { .s = parser->lexer.data + parser->ahead.loc.begin.byte_offset },
                { .len = byte_span(&parser->ahead.loc) }
            });
    }
}

static inline const char* copy_string_from_token(struct parser* parser) {
    size_t len = byte_span(&parser->ahead.loc);
    char* str = alloc_from_mem_pool(parser->mem_pool, len + 1);
    memcpy(str, parser->lexer.data + parser->ahead.loc.begin.byte_offset, len);
    str[len] = 0;
    return str;
}

static inline struct file_loc make_loc(const struct parser* parser, const struct file_pos* begin) {
    return (struct file_loc) {
        .file_name = parser->lexer.file_name,
        .begin = *begin,
        .end = parser->ahead.loc.end
    };
}

static inline ir_node_t find_var(const struct parser* parser, size_t var_index, const struct file_pos* begin) {
    // TODO
    return NULL;
}

static ir_node_t parse_node(struct parser*);
static ir_type_t parse_type(struct parser*);

static const char* parse_ident(struct parser* parser) {
    if (parser->ahead.tag != TOKEN_IDENT)
        return NULL;
    const char* name = copy_string_from_token(parser);
    eat_token(parser, TOKEN_IDENT);
    return name;
}

static ir_node_t parse_error_at(struct parser* parser, const char* msg, const struct file_loc* loc) {
    log_error(
        parser->lexer.log, loc,
        parser->ahead.tag == TOKEN_EOF
            ? "expected {s}, but got end of file"
            : "expected {s}, but got '{sl}'",
        (union format_arg[]) {
            { .s = msg },
            { .s = parser->lexer.data + parser->ahead.loc.begin.byte_offset },
            { .len = byte_span(&parser->ahead.loc) }
        });
    return NULL;
}

static ir_node_t parse_error(struct parser* parser, const char* msg) {
    return parse_error_at(parser, msg, &parser->ahead.loc);
}

static size_t parse_var_index(struct parser* parser) {
    if (parser->ahead.tag != TOKEN_LITERAL || parser->ahead.lit.tag != LITERAL_INT) {
        parse_error(parser, "variable index");
        return 0;
    }
    return parser->ahead.lit.data.int_val;
}

static ir_node_t parse_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    expect_token(parser, TOKEN_IDENT);
    expect_token(parser, TOKEN_HASH);
    size_t var_index = parse_var_index(parser);
    eat_token(parser, TOKEN_LITERAL);
    return find_var(parser, var_index, &begin);
}

static ir_node_t parse_let_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    const char* name = parse_ident(parser);
    expect_token(parser, TOKEN_HASH);
    size_t var_index = parse_var_index(parser);
    eat_token(parser, TOKEN_LITERAL);
    expect_token(parser, TOKEN_COLON);
    ir_type_t type = parse_type(parser);
    expect_token(parser, TOKEN_EQUAL);
    ir_node_t val = parse_node(parser);
    return type && val ? make_tied_var(parser->module, type, var_index, val,
        &(struct debug_info) { .name = name, .loc = make_loc(parser, &begin) }) : NULL;
}

static ir_node_t parse_let(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, TOKEN_LET);
    ir_node_t* vars = NULL;
    size_t var_count = 0, var_capacity = 0;
    while (true) {
        ir_node_t var = parse_let_var(parser);
        if (var_count + 2 >= var_capacity) {
            var_capacity = (var_count + 2) * 2;
            vars = realloc_or_die(vars, sizeof(struct ir_node*) * var_capacity);
        }

        vars[var_count++] = var;
        if (!accept_token(parser, TOKEN_COMMA))
            break;
    }
    expect_token(parser, TOKEN_IN);
    ir_node_t body = parse_node(parser);
    if (!body)
        return NULL;
    ir_node_t node = make_let(parser->module, vars, var_count, body,
        &(struct debug_info) { .loc = make_loc(parser, &begin) });
    free(vars);
    return node;
}

static ir_node_t parse_node(struct parser* parser) {
    switch (parser->ahead.tag) {
        case TOKEN_HASH:
        case TOKEN_IDENT:
            return parse_var(parser);
        case TOKEN_LET:
            return parse_let(parser);
        default:
            return parse_error(parser, "expression");
    }
}

static ir_type_t parse_type(struct parser* parser) {
    ir_type_t type = parse_node(parser);
    if (type && !is_type(type))
        return parse_error_at(parser, "type", &type->debug->loc);
    return type;
}

ir_node_t parse_ir(
    struct log* log,
    struct ir_module* module,
    struct mem_pool* mem_pool,
    const char* data,
    size_t data_size,
    const char* file_name)
{
    struct file_pos begin = {
        .row = 1,
        .col = 1,
        .byte_offset = 0
    };
    struct parser parser = {
        .mem_pool = mem_pool,
        .lexer = (struct lexer) {
            .log = log,
            .cur_pos = begin,
            .keywords = new_hash_table(KEYWORD_COUNT, sizeof(struct keyword)),
            .data = data,
            .data_size = data_size,
            .file_name = file_name
        },
        .module = module
    };
    register_keywords(&parser.lexer);
    parser.ahead = advance_lexer(&parser.lexer);
    parser.prev_end = begin;
    ir_node_t node = parse_node(&parser);
    free_hash_table(&parser.lexer.keywords);
    return node;
}
