#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <ctype.h>

#include "core/utils.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/hash_table.h"
#include "core/hash.h"
#include "core/log.h"
#include "ir/parse.h"
#include "ir/node.h"
#include "ir/module.h"

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
    f(MINUS, "-") \
    f(STAR, "*") \
    f(VERT_BAR, "|") \
    f(EQUAL, "=")

#define KEYWORDS(f) \
    f(FUN, "fun") \
    f(LET, "let") \
    f(IN, "in")

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
    const char* data_end;
    const char* file_name;
};

struct parser {
    struct token ahead;
    struct file_pos prev_end;
    struct ir_module* module;
    struct mem_pool* mem_pool;
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

        if (accept_char(lexer, '#')) {
            eat_single_line_comment(lexer);
            continue;
        }

        if (accept_char(lexer, '=')) return make_token(lexer, &lexer->cur_pos, TOKEN_EQUAL);
        if (accept_char(lexer, ',')) return make_token(lexer, &lexer->cur_pos, TOKEN_COMMA);

        struct file_pos begin = lexer->cur_pos;
        if (next_char(lexer) == '_' || isalpha(next_char(lexer))) {
            eat_char(lexer);
            while (isalnum(next_char(lexer)) || next_char(lexer) == '_')
                eat_char(lexer);
            size_t len = lexer->cur_pos.data_ptr - begin.data_ptr;
            struct keyword* keyword = find_in_hash_table(
                &lexer->keywords,
                &(struct keyword) { .name = begin.data_ptr, .len = len },
                hash_raw_bytes(hash_init(), begin.data_ptr, len),
                sizeof(struct keyword), compare_keywords);
            return make_token(lexer, &begin, keyword ? keyword->tag : TOKEN_IDENT);
        }

        eat_char(lexer);
        struct file_loc loc = {
            .file_name = lexer->file_name,
            .begin = begin,
            .end = lexer->cur_pos
        };
        log_error(lexer->log, &loc, "unknown token '{sl}'", (union format_arg[]) {
            { .s = begin.data_ptr },
            { .len = lexer->cur_pos.data_ptr - begin.data_ptr }
        });
        return make_token(lexer, &begin, TOKEN_ERROR);
    }
}

static inline void eat_token(struct parser* parser, enum token_tag tag) {
    assert(parser->ahead.tag == tag);
    parser->prev_end = parser->ahead.loc.end;
    parser->ahead = advance_lexer(&parser->lexer);
}

static inline bool accept_token(struct parser* parser, enum token_tag tag) {
    assert(tag != TOKEN_EOF && tag != TOKEN_ERROR);
    if (parser->ahead.tag == tag) {
        eat_token(parser, tag);
        return true;
    }
    return false;
}

static inline size_t loc_byte_span(const struct file_loc* loc) {
    return loc->end.data_ptr - loc->begin.data_ptr;
}

static inline void expect_token(struct parser* parser, enum token_tag tag) {
    if (!accept_token(parser, tag)) {
        bool is_special = tag == TOKEN_IDENT || tag == TOKEN_LITERAL;
        log_error(
            parser->lexer.log, &parser->ahead.loc,
            is_special ? "expected {s}, but got '{sl}'" : "expected '{s}', but got '{sl}'",
            (union format_arg[]) {
                { .s = token_tag_to_string(tag) },
                { .s = parser->ahead.loc.begin.data_ptr },
                { .len = loc_byte_span(&parser->ahead.loc) }
            });
    }
}

static inline const char* copy_string_from_token(struct parser* parser) {
    size_t len = parser->ahead.loc.end.data_ptr - parser->ahead.loc.begin.data_ptr;
    char* str = alloc_from_mem_pool(parser->mem_pool, len + 1);
    memcpy(str, parser->ahead.loc.begin.data_ptr, len);
    str[len] = 0;
    return str;
}

static inline struct ir_node* make_ir_node(
    struct parser* parser,
    const struct file_pos* begin,
    enum ir_node_tag tag,
    size_t op_count,
    const char* name)
{
    struct ir_node* node = alloc_from_mem_pool(
        parser->mem_pool, sizeof(struct ir_node) + sizeof(struct ir_node*) * op_count);
    memset(node, 0, sizeof(struct ir_node));
    node->tag = tag;
    node->op_count = op_count;
    node->debug = make_debug_info(parser->module, &(struct file_loc) {
        .file_name = parser->ahead.loc.file_name,
        .begin = *begin,
        .end = parser->prev_end
    }, name);
    return node;
}

static struct ir_node* parse(struct parser*);

static struct ir_node* parse_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    const char* name = copy_string_from_token(parser);
    eat_token(parser, TOKEN_IDENT);
    struct ir_node* var = make_ir_node(parser, &begin, IR_NODE_VAR, 0, name);
    var->data.var_index = 0;
    return var;
}

static struct ir_node* parse_error(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, parser->ahead.tag);
    return make_ir_node(parser, &begin, IR_NODE_ERROR, 0, NULL);
}

static inline struct ir_node* expect_var(struct parser* parser) {
    if (parser->ahead.tag == TOKEN_IDENT)
        return parse_var(parser);
    log_error(
        parser->lexer.log, &parser->ahead.loc,
        "expected variable, but got '{sl}'",
        (union format_arg[]) {
            { .s = parser->ahead.loc.begin.data_ptr },
            { .len = loc_byte_span(&parser->ahead.loc) }
        });
    return parse_error(parser);
}

static struct ir_node* parse_let(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, TOKEN_LET);
    struct ir_node** ops = NULL;
    size_t op_count = 0, op_capacity = 0;
    while (true) {
        struct ir_node* var = expect_var(parser);
        expect_token(parser, TOKEN_EQUAL);
        struct ir_node* val = parse(parser);
        if (op_count + 2 >= op_capacity) {
            op_capacity = (op_capacity + 1) * 2;
            ops = realloc_or_die(ops, sizeof(struct ir_node*) * op_capacity);
        }
        ops[op_count++] = var;
        ops[op_count++] = val;
        if (!accept_token(parser, TOKEN_COMMA))
            break;
    }
    expect_token(parser, TOKEN_IN);
    struct ir_node* body = parse(parser);
    struct ir_node* let = make_ir_node(parser, &begin, IR_NODE_LET, op_count + 1, NULL);
    memcpy(let->ops, ops, sizeof(struct ir_node*) * op_count);
    free(ops);
    let->ops[op_count] = body;
    return let;
}

static struct ir_node* parse(struct parser* parser) {
    switch (parser->ahead.tag) {
        case TOKEN_IDENT:
            return parse_var(parser);
        case TOKEN_LET:
            return parse_let(parser);
        default:
            log_error(
                parser->lexer.log, &parser->ahead.loc,
                "expected expression, but got '{sl}'",
                (union format_arg[]) {
                    { .s = parser->ahead.loc.begin.data_ptr },
                    { .len = loc_byte_span(&parser->ahead.loc) }
                });
            return parse_error(parser);
    }
}

struct ir_node* parse_ir(
    struct log* log,
    struct ir_module* module,
    struct mem_pool* mem_pool,
    const char* data_ptr,
    size_t data_size,
    const char* file_name)
{
    struct file_pos begin = {
        .row = 1,
        .col = 1,
        .data_ptr = data_ptr
    };
    struct parser parser = {
        .mem_pool = mem_pool,
        .lexer = (struct lexer) {
            .log = log,
            .cur_pos = begin,
            .keywords = new_hash_table(KEYWORD_COUNT, sizeof(struct keyword)),
            .data_end = data_ptr + data_size,
            .file_name = file_name
        },
        .module = module
    };
    register_keywords(&parser.lexer);
    parser.ahead = advance_lexer(&parser.lexer);
    parser.prev_end = begin;
    struct ir_node* node = parse(&parser);
    free_hash_table(&parser.lexer.keywords);
    return node;
}
