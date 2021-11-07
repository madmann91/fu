#include "core/utils.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/hash_table.h"
#include "core/hash.h"
#include "core/log.h"
#include "ir/parse.h"
#include "ir/node.h"
#include "ir/module.h"
#include "ir/node_list.h"

#include <stdlib.h>
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <inttypes.h>
#include <string.h>
#include <ctype.h>

#define DEFAULT_ENV_CAPACITY 8

#define SYMBOLS(f) \
    f(L_PAREN, "(") \
    f(R_PAREN, ")") \
    f(L_BRACKET, "[") \
    f(R_BRACKET, "]") \
    f(COLON, ":") \
    f(COMMA, ",") \
    f(SLASH, "/") \
    f(HASH, "#") \
    f(EQUAL, "=")

// Note: types are added separately because some have the same name as nodes.
#define EXTRACT_TAG_AND_STR(tag, str, ...) (tag, str)
#define EXTRACT_VEC_TAG_AND_STR(tag, str, ...) (V##tag, "v" str)

#define KEYWORDS(f) \
    f(IN,     "in") \
    f(CONST,  "const") \
    f(VEC,    "vec") \
    f(PTR,    "ptr") \
    f(ERR,    "err") \
    f(MEM,    "mem") \
    f(OPTION, "option") \
    f(INT,    "int") \
    f(FLOAT,  "float") \
    DEFER(IR_VEC_OP_LIST(DEFER(f)EXTRACT_TAG_AND_STR)) \
    DEFER(IR_VEC_OP_LIST(DEFER(f)EXTRACT_VEC_TAG_AND_STR)) \
    DEFER(IR_SCALAR_OP_LIST(DEFER(f)EXTRACT_TAG_AND_STR)) \
    DEFER(IR_KIND_LIST(DEFER(f)EXTRACT_TAG_AND_STR))

#define TOKENS(f) \
    KEYWORDS(f) \
    SYMBOLS(f) \
    f(IDENT, "identifier") \
    f(LITERAL, "literal") \
    f(INVALID, "invalid token") \
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
        ir_float_t float_val;
        ir_uint_t int_val;
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
        if (accept_char(lexer, ':')) return make_token(lexer, &begin, TOKEN_COLON);
        if (accept_char(lexer, '#')) return make_token(lexer, &begin, TOKEN_HASH);
        if (accept_char(lexer, ',')) return make_token(lexer, &begin, TOKEN_COMMA);
        if (accept_char(lexer, '[')) return make_token(lexer, &begin, TOKEN_L_BRACKET);
        if (accept_char(lexer, ']')) return make_token(lexer, &begin, TOKEN_R_BRACKET);
        if (accept_char(lexer, '(')) return make_token(lexer, &begin, TOKEN_L_PAREN);
        if (accept_char(lexer, ')')) return make_token(lexer, &begin, TOKEN_R_PAREN);

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
        return make_token(lexer, &begin, TOKEN_INVALID);
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
    assert(tag != TOKEN_EOF && tag != TOKEN_INVALID);
    if (parser->ahead.tag == tag) {
        eat_token(parser, tag);
        return true;
    }
    return false;
}

static inline size_t byte_span(const struct file_loc* loc) {
    return loc->end.byte_offset - loc->begin.byte_offset;
}

static inline bool expect_token(struct parser* parser, enum token_tag tag) {
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
        return false;
    }
    return true;
}

static inline struct file_loc make_loc(const struct parser* parser, const struct file_pos* begin) {
    return (struct file_loc) {
        .file_name = parser->lexer.file_name,
        .begin = *begin,
        .end = parser->ahead.loc.end
    };
}

static bool compare_vars(const void* left, const void* right) {
    return (*(ir_node_t*)left)->data.var_index == (*(ir_node_t*)right)->data.var_index;
}

static uint32_t hash_var_index(size_t var_index) {
    return hash_uint64(hash_init(), var_index);
}

static inline ir_node_t find_var(const struct parser* parser, size_t var_index) {
    struct ir_node node = { .data.var_index = var_index };
    ir_node_t node_ptr = &node;
    ir_node_t* var_ptr = find_in_hash_table(
        &parser->env, &node_ptr, hash_var_index(var_index), sizeof(ir_node_t), compare_vars);
    return var_ptr ? *var_ptr : NULL;
}

static bool add_var(struct parser* parser, ir_node_t var) {
    assert(var->tag == IR_VAR);
    if (!insert_in_hash_table(&parser->env, &var, hash_var_index(var->data.var_index), sizeof(ir_node_t), compare_vars)) {
        log_error(parser->lexer.log, &var->debug->loc,
            "shadowing variable with index '{u64}'",
            (union format_arg[]) { { .u64 = var->data.var_index } });
        return false;
    }
    return true;
}

static void remove_var(struct parser* parser, ir_node_t var) {
    ir_node_t* var_ptr = find_in_hash_table(
        &parser->env, &var, hash_var_index(var->data.var_index), sizeof(ir_node_t), compare_vars);
    if (var_ptr)
        remove_from_hash_table(&parser->env, var_ptr, sizeof(ir_node_t));
}

static ir_val_t parse_val(struct parser*, ir_type_t);
static ir_type_t parse_type(struct parser*);

static ir_node_t* parse_many(struct parser* parser, size_t* count, enum token_tag sep, ir_node_t (*parse_one)(struct parser*)) {
    ir_node_t* nodes = NULL;
    size_t capacity = 0;

    *count = 0;
    while (true) {
        ir_node_t node = parse_one(parser);

        if (*count >= capacity) {
            capacity = (*count + 1) * 2;
            nodes = realloc_or_die(nodes, sizeof(ir_node_t) * capacity);
        }

        nodes[(*count)++] = node;
        if (!accept_token(parser, sep))
            break;
    }
    return nodes;
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
    return make_error(parser->module);
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
    eat_token(parser, TOKEN_HASH);
    size_t var_index = parse_var_index(parser);
    eat_token(parser, TOKEN_LITERAL);
    ir_node_t var = find_var(parser, var_index);
    if (!var) {
        struct file_loc loc = make_loc(parser, &begin);
        log_error(parser->lexer.log, &loc,
            "variable with index '{u64}' is not in the current scope",
            (union format_arg[]) { { .u64 = var_index } });
        return make_error(parser->module);
    }
    return var;
}

static ir_node_t parse_const(struct parser* parser, ir_node_t type) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, TOKEN_CONST);
    struct literal literal = parser->ahead.lit;
    bool was_literal = expect_token(parser, TOKEN_LITERAL);
    if (accept_token(parser, TOKEN_COLON))
        type = as_node(parse_type(parser));
    if (!type) {
        struct file_loc loc = make_loc(parser, &begin);
        log_error(parser->lexer.log, &loc, "type annotation required for constant", NULL);
        return make_error(parser->module);
    }
    union ir_node_data data;
    if (was_literal) {
        if (literal.tag == LITERAL_INT) {
            if (type->tag == IR_TYPE_FLOAT)
                data = make_float_node_data(literal.data.int_val);
            else
                data = make_int_node_data(literal.data.int_val);
        } else
            data = make_float_node_data(literal.data.float_val);
    }
    return make_const(parser->module, type, &data);
}

static ir_node_t parse_type_op(struct parser* parser) {
    return as_node(parse_type(parser));
}

static ir_type_t parse_type(struct parser* parser) {
    enum ir_node_tag node_tag;
    switch (parser->ahead.tag) {
#define type(tag, str, n) case TOKEN_##tag: node_tag = IR_TYPE_##tag; break;
        IR_TYPE_LIST(type)
#undef type
        case TOKEN_CONST:
            return to_type(parse_const(parser, as_node(make_nat(parser->module))));
        case TOKEN_HASH:
            return to_type(parse_var(parser));
        default:
            return to_type(parse_error(parser, "type"));
    }
    struct file_pos begin = parser->ahead.loc.begin;
    skip_token(parser);
    size_t op_count = 0;
    ir_node_t* ops = NULL;
    if (accept_token(parser, TOKEN_L_BRACKET)) {
        ops = parse_many(parser, &op_count, TOKEN_COMMA, parse_type_op);
        expect_token(parser, TOKEN_R_BRACKET);
    }
    ir_type_t type = to_type(make_node(parser->module, node_tag, as_node(make_star(parser->module)), ops, op_count, NULL,
        &(struct debug_info) { .loc = make_loc(parser, &begin) }));
    free(ops);
    return type;
}

static ir_node_t parse_let_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    expect_token(parser, TOKEN_HASH);
    size_t var_index = parse_var_index(parser);
    eat_token(parser, TOKEN_LITERAL);
    expect_token(parser, TOKEN_COLON);
    ir_type_t type = parse_type(parser);
    expect_token(parser, TOKEN_EQUAL);
    ir_val_t val = parse_val(parser, type);
    return make_tied_var(parser->module, as_node(type), var_index, as_node(val),
        &(struct debug_info) { .loc = make_loc(parser, &begin) });
}

static ir_val_t parse_let(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, TOKEN_LET);
    size_t var_count = 0;
    ir_val_t* vars = (ir_val_t*)parse_many(parser, &var_count, TOKEN_COMMA, parse_let_var);
    expect_token(parser, TOKEN_IN);
    if (var_count == 0)
        return to_val(make_error(parser->module));
    for (size_t i = 0; i < var_count; ++i)
        add_var(parser, as_node(vars[i]));
    ir_val_t body = parse_val(parser, NULL);
    for (size_t i = 0; i < var_count; ++i)
        remove_var(parser, as_node(vars[i]));
    ir_val_t val = make_let(parser->module, vars, var_count, body,
        &(struct debug_info) { .loc = make_loc(parser, &begin) });
    free(vars);
    return val;
}

static ir_node_t parse_func_var(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    expect_token(parser, TOKEN_HASH);
    size_t var_index = parse_var_index(parser);
    eat_token(parser, TOKEN_LITERAL);
    expect_token(parser, TOKEN_COLON);
    ir_type_t type = parse_type(parser);
    return make_untied_var(parser->module, as_node(type), var_index, &(struct debug_info) { .loc = make_loc(parser, &begin) });
}

static ir_val_t parse_func(struct parser* parser) {
    struct file_pos begin = parser->ahead.loc.begin;
    eat_token(parser, TOKEN_FUNC);
    expect_token(parser, TOKEN_L_PAREN);
    ir_node_t var = parse_func_var(parser);
    add_var(parser, var);
    expect_token(parser, TOKEN_R_PAREN);
    ir_node_t body = as_node(parse_val(parser, NULL));
    remove_var(parser, var);
    return to_val(make_func(parser->module, var, body, &(struct debug_info) { .loc = make_loc(parser, &begin) }));
}

static ir_node_t parse_val_op(struct parser* parser) {
    return as_node(parse_val(parser, NULL));
}

static ir_val_t parse_val(struct parser* parser, ir_type_t expected) {
    enum ir_node_tag node_tag;
    switch (parser->ahead.tag) {
#define val(x, str, n)    case TOKEN_##x:  node_tag = IR_VAL_##x;  break;
#define vec_op(x, str, n) case TOKEN_V##x: node_tag = IR_VAL_V##x; break;
        IR_VEC_OP_LIST(val)
        IR_VEC_OP_LIST(vec_op)
#undef val
#undef vec_op
        case TOKEN_UNDEF:     node_tag = IR_VAL_UNDEF; break;
        case TOKEN_ANY:       node_tag = IR_VAL_ANY; break;
        case TOKEN_ALL:       node_tag = IR_VAL_ANY; break;
        case TOKEN_BROADCAST: node_tag = IR_VAL_BROADCAST; break;
        case TOKEN_PTRTOVEC:  node_tag = IR_VAL_PTRTOVEC; break;
        case TOKEN_ALLOC:     node_tag = IR_VAL_ALLOC; break;
        case TOKEN_ARRAY:     node_tag = IR_VAL_ARRAY; break;
        case TOKEN_TUPLE:     node_tag = IR_VAL_TUPLE; break;
        case TOKEN_HASH:
            return to_val(parse_var(parser));
        case TOKEN_LET:
            return parse_let(parser);
        case TOKEN_FUNC:
            return parse_func(parser);
        case TOKEN_CONST:
            return to_val(parse_const(parser, as_node(expected)));
        default:
            return to_val(parse_error(parser, "value"));
    }
    struct file_pos begin = parser->ahead.loc.begin;
    skip_token(parser);
    size_t op_count = 0;
    ir_node_t* ops = NULL;
    if (accept_token(parser, TOKEN_L_PAREN)) {
        ops = parse_many(parser, &op_count, TOKEN_COMMA, parse_val_op);
        expect_token(parser, TOKEN_R_PAREN);
    }
    struct debug_info debug = { .loc = make_loc(parser, &begin) };
    ir_type_t type = infer_type(parser->module, node_tag, (ir_val_t*)ops, op_count, &debug);
    ir_val_t val = to_val(make_node(parser->module, node_tag, as_node(type), ops, op_count, NULL, &debug));
    free(ops);
    return val;
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
        .module = module,
        .env = new_hash_table(DEFAULT_ENV_CAPACITY, sizeof(ir_node_t)),
    };
    register_keywords(&parser.lexer);
    parser.ahead = advance_lexer(&parser.lexer);
    parser.prev_end = begin;
    ir_node_t node = as_node(parse_val(&parser, NULL));
    free_hash_table(&parser.lexer.keywords);
    free_hash_table(&parser.env);
    return node;
}
