#include "lang/parse.h"
#include "lang/ast.h"
#include "core/mem_pool.h"
#include "core/log.h"
#include "core/utils.h"
#include "core/hash.h"
#include "core/hash_table.h"
#include "core/utf8.h"

#include <string.h>
#include <inttypes.h>
#include <ctype.h>

#define LOOKAHEAD 2

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
    f(SLASH, "/") \
    f(HASH, "#") \
    f(EQUAL, "=") \
    f(FAT_ARROW, "=>")

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
    struct token ahead[LOOKAHEAD];
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

static bool compare_keywords(const void* left, const void* right) {
    return
        ((struct keyword*)left)->len == ((struct keyword*)right)->len &&
        !memcmp(((struct keyword*)left)->name, ((struct keyword*)right)->name, ((struct keyword*)left)->len);
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

        if (accept_char(lexer, '=')) {
            if (accept_char(lexer, '>'))
                return make_token(lexer, &begin, TOKEN_FAT_ARROW);
            return make_token(lexer, &begin, TOKEN_EQUAL);
        }
        if (accept_char(lexer, '.')) return make_token(lexer, &begin, TOKEN_DOT);
        if (accept_char(lexer, ':')) return make_token(lexer, &begin, TOKEN_COLON);
        if (accept_char(lexer, ';')) return make_token(lexer, &begin, TOKEN_SEMICOLON);
        if (accept_char(lexer, '#')) return make_token(lexer, &begin, TOKEN_HASH);
        if (accept_char(lexer, ',')) return make_token(lexer, &begin, TOKEN_COMMA);
        if (accept_char(lexer, '[')) return make_token(lexer, &begin, TOKEN_L_BRACKET);
        if (accept_char(lexer, ']')) return make_token(lexer, &begin, TOKEN_R_BRACKET);
        if (accept_char(lexer, '(')) return make_token(lexer, &begin, TOKEN_L_PAREN);
        if (accept_char(lexer, ')')) return make_token(lexer, &begin, TOKEN_R_PAREN);
        if (accept_char(lexer, '{')) return make_token(lexer, &begin, TOKEN_L_BRACE);
        if (accept_char(lexer, '}')) return make_token(lexer, &begin, TOKEN_R_BRACE);

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
            struct token token = make_token(lexer, &begin, TOKEN_INVALID);
            if (has_dot) {
                token.tag = TOKEN_FLOAT_LITERAL;
                token.float_val = strtod(lexer->data + begin.byte_offset, NULL);
            } else {
                token.tag = TOKEN_INT_LITERAL;
                token.int_val = strtoumax(lexer->data + begin.byte_offset, NULL, 10);
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
    for (size_t i = 1; i < LOOKAHEAD; ++i)
        parser->ahead[i - 1] = parser->ahead[i];
    parser->ahead[LOOKAHEAD - 1] = advance_lexer(&parser->lexer);
}

static inline void eat_token(struct parser* parser, enum token_tag tag) {
    (void)tag;
    assert(parser->ahead->tag == tag);
    skip_token(parser);
}

static inline bool accept_token(struct parser* parser, enum token_tag tag) {
    assert(tag != TOKEN_EOF && tag != TOKEN_INVALID);
    if (parser->ahead->tag == tag) {
        eat_token(parser, tag);
        return true;
    }
    return false;
}

static inline size_t byte_span(const struct file_loc* loc) {
    return loc->end.byte_offset - loc->begin.byte_offset;
}

static inline bool expect_token(struct parser* parser, enum token_tag tag) {
    assert(tag != TOKEN_EOF);
    if (!accept_token(parser, tag)) {
        bool needs_quotes =
            tag != TOKEN_IDENT &&
            tag != TOKEN_INT_LITERAL &&
            tag != TOKEN_FLOAT_LITERAL &&
            tag != TOKEN_STRING_LITERAL &&
            tag != TOKEN_CHAR_LITERAL;
        log_error(
            parser->lexer.log, &parser->ahead->loc,
            needs_quotes ? "expected '{s}', but got '{sl}'" : "expected {s}, but got '{sl}'",
            (union format_arg[]) {
                { .s = token_tag_to_string(tag) },
                { .s = parser->lexer.data + parser->ahead->loc.begin.byte_offset },
                { .len = byte_span(&parser->ahead->loc) }
            });
        return false;
    }
    return true;
}

static inline struct file_loc make_loc(const struct parser* parser, const struct file_pos* begin) {
    return (struct file_loc) {
        .file_name = parser->lexer.file_name,
        .begin = *begin,
        .end = parser->ahead->loc.end
    };
}

static inline struct ast* make_ast(struct parser* parser, const struct file_pos* begin, const struct ast* ast) {
    struct ast* copy = alloc_from_mem_pool(parser->mem_pool, sizeof(struct ast));
    memcpy(copy, ast, sizeof(struct ast));
    copy->loc = make_loc(parser, begin);
    return copy;
}

static inline struct ast* append_ast(struct ast** first, struct ast** last, struct ast* ast) {
    if (!*first)
        *first = ast;
    else
        (*last)->next = ast;
    (*last) = ast;
    return ast;
}

static struct ast* parse_pattern(struct parser* parser);
static struct ast* parse_type(struct parser* parser);
static struct ast* parse_expr(struct parser* parser);
static struct ast* parse_stmt(struct parser* parser, bool*);
static struct ast* parse_expr_without_struct(struct parser* parser);

static struct ast* parse_many(
    struct parser* parser,
    enum token_tag stop,
    enum token_tag sep,
    struct ast* (*parse_one)(struct parser*))
{
    struct ast* first = NULL, *last = NULL;
    while (
        (stop == TOKEN_INVALID || parser->ahead->tag != stop) &&
        parser->ahead->tag != TOKEN_EOF)
    {
        append_ast(&first, &last, parse_one(parser));
        if (sep != TOKEN_INVALID && !accept_token(parser, sep))
            break;
    }
    return first;
}

static struct ast* parse_error_at(struct parser* parser, const char* msg, const struct file_loc* loc) {
    log_error(
        parser->lexer.log, loc,
        parser->ahead->tag == TOKEN_EOF
            ? "expected {s}, but got end of file"
            : "expected {s}, but got '{sl}'",
        (union format_arg[]) {
            { .s = msg },
            { .s = parser->lexer.data + loc->begin.byte_offset },
            { .len = byte_span(loc) }
        });
    return make_ast(parser, &loc->begin, &(struct ast) { .tag = AST_ERROR, .loc = *loc });
}

static struct ast* parse_error(struct parser* parser, const char* msg) {
    struct file_loc loc = parser->ahead->loc; 
    skip_token(parser);
    return parse_error_at(parser, msg, &loc);
}

static const char* parse_ident_as_string(struct parser* parser) {
    if (parser->ahead->tag != TOKEN_IDENT)
        return "";
    size_t len = byte_span(&parser->ahead->loc);
    char* str = alloc_from_mem_pool(parser->mem_pool, len + 1);
    memcpy(str, parser->lexer.data + parser->ahead->loc.begin.byte_offset, len);
    str[len] = 0;
    eat_token(parser, TOKEN_IDENT);
    return str;
}

static struct ast* parse_ident(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    const char* name = parse_ident_as_string(parser);
    return make_ast(parser, &begin, &(struct ast) { .tag = AST_IDENT, .ident.name = name });
}

static struct ast* parse_path_elem(struct parser* parser) {
    struct ast* ident = parse_ident(parser);
    if (accept_token(parser, TOKEN_L_BRACKET)) {
        struct ast* type_args = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_type);
        expect_token(parser, TOKEN_R_BRACKET);
        return make_ast(parser, &ident->loc.begin, &(struct ast) {
            .tag = AST_PATH_ELEM,
            .path_elem = {
                .ident = ident,
                .type_args = type_args
            }
        });
    }
    return ident;
}

static struct ast* parse_path(struct parser* parser) {
    struct ast* first = parse_path_elem(parser);
    struct ast* last = first;
    while (accept_token(parser, TOKEN_DOT)) {
        last->next = parse_path_elem(parser);
        last = last->next;
    }
    return make_ast(parser, &first->loc.begin, &(struct ast) { .tag = AST_PATH, .path.elems = first });
}

static struct ast* parse_prim_type(struct parser* parser, enum prim_type_tag tag) {
    struct file_pos begin = parser->ahead->loc.begin;
    skip_token(parser);
    return make_ast(parser, &begin, &(struct ast) { .tag = AST_PRIM_TYPE, .prim_type.tag = tag });
}

static struct ast* parse_type_param(struct parser* parser) {
    if (parser->ahead->tag == TOKEN_IDENT)
        return parse_ident(parser);
    return parse_error(parser, "type parameter");
}

static struct ast* parse_type_params(struct parser* parser) {
    if (!accept_token(parser, TOKEN_L_BRACKET))
        return NULL;
    struct ast* type_params = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_type_param);
    expect_token(parser, TOKEN_R_BRACKET);
    return type_params;
}

static struct ast* parse_field_decl(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    const char* name = parse_ident_as_string(parser);
    expect_token(parser, TOKEN_COLON);
    struct ast* type = parse_type(parser);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_FIELD_DECL,
        .field_decl = {
            .name = name,
            .type = type
        }
    });
}

static struct ast* parse_struct_decl(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_STRUCT);
    const char* name  = parse_ident_as_string(parser);
    struct ast* type_params = parse_type_params(parser);
    expect_token(parser, TOKEN_L_BRACE);
    struct ast* fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field_decl);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_STRUCT_DECL,
        .struct_decl = {
            .name = name,
            .type_params = type_params,
            .fields = fields
        }
    });
}

static struct ast* parse_option_decl(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    const char* name = parse_ident_as_string(parser);
    struct ast* param = parser->ahead->tag == TOKEN_L_PAREN ? parse_type(parser) : NULL;
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_OPTION_DECL,
        .option_decl = { .name = name, .param = param }
    });
}

static struct ast* parse_enum_decl(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_ENUM);
    const char* name  = parse_ident_as_string(parser);
    struct ast* type_params = parse_type_params(parser);
    expect_token(parser, TOKEN_L_BRACE);
    struct ast* options = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_option_decl);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_ENUM_DECL,
        .enum_decl = {
            .name = name,
            .type_params = type_params,
            .options = options
        }
    });
}

static struct ast* parse_tuple_type_or_expr_or_pattern(
    struct parser* parser,
    enum ast_tag tag,
    struct ast* (*parse_arg)(struct parser*))
{
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_L_PAREN);
    struct ast* args = parse_many(parser, TOKEN_R_PAREN, TOKEN_COMMA, parse_arg);
    expect_token(parser, TOKEN_R_PAREN);
    if (args && !args->next)
        return args;
    return make_ast(parser, &begin, &(struct ast) { .tag = tag, .tuple_type.args = args });
}

static struct ast* parse_tuple_type(struct parser* parser) {
    return parse_tuple_type_or_expr_or_pattern(parser, AST_TUPLE_TYPE, parse_type);
}

static struct ast* parse_tuple_expr(struct parser* parser) {
    return parse_tuple_type_or_expr_or_pattern(parser, AST_TUPLE_EXPR, parse_expr);
}

static struct ast* parse_tuple_pattern(struct parser* parser) {
    return parse_tuple_type_or_expr_or_pattern(parser, AST_TUPLE_PATTERN, parse_pattern);
}

static struct ast* parse_type(struct parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_BOOL:    return parse_prim_type(parser, PRIM_TYPE_BOOL);
        case TOKEN_I8:      return parse_prim_type(parser, PRIM_TYPE_I8);
        case TOKEN_I16:     return parse_prim_type(parser, PRIM_TYPE_I16);
        case TOKEN_I32:     return parse_prim_type(parser, PRIM_TYPE_I32);
        case TOKEN_I64:     return parse_prim_type(parser, PRIM_TYPE_I64);
        case TOKEN_U8:      return parse_prim_type(parser, PRIM_TYPE_U8);
        case TOKEN_U16:     return parse_prim_type(parser, PRIM_TYPE_U16);
        case TOKEN_U32:     return parse_prim_type(parser, PRIM_TYPE_U32);
        case TOKEN_U64:     return parse_prim_type(parser, PRIM_TYPE_U64);
        case TOKEN_F32:     return parse_prim_type(parser, PRIM_TYPE_F32);
        case TOKEN_F64:     return parse_prim_type(parser, PRIM_TYPE_F64);
        case TOKEN_IDENT:   return parse_path(parser);
        case TOKEN_STRUCT:  return parse_struct_decl(parser);
        case TOKEN_ENUM:    return parse_enum_decl(parser);
        case TOKEN_L_PAREN: return parse_tuple_type(parser);
        default:
            return parse_error(parser, "type");
    }
}

static struct ast* parse_type_annot(struct parser* parser, struct ast* left) {
    eat_token(parser, TOKEN_COLON);
    struct ast* type = parse_type(parser);
    return make_ast(parser, &left->loc.begin, &(struct ast) {
        .tag = AST_TYPE_ANNOT,
        .type_annot = {
            .left = left,
            .type = type
        }
    });
}

static struct ast* parse_filter(struct parser* parser) {
    eat_token(parser, TOKEN_FILTER);
    expect_token(parser, TOKEN_L_PAREN);
    struct ast* filter = parse_expr(parser);
    expect_token(parser, TOKEN_R_PAREN);
    return filter;
}

static struct ast* parse_array_expr_or_pattern(
    struct parser* parser,
    enum ast_tag tag, 
    struct ast* (*parse_arg)(struct parser*))
{
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_L_BRACKET);
    struct ast* elems = parse_many(parser, TOKEN_R_PAREN, TOKEN_COMMA, parse_arg);
    expect_token(parser, TOKEN_R_BRACKET);
    if (!elems->next)
        return elems;
    return make_ast(parser, &begin, &(struct ast) { .tag = tag, .array_expr.elems = elems });
}

static struct ast* parse_array_expr(struct parser* parser) {
    return parse_array_expr_or_pattern(parser, AST_ARRAY_EXPR, parse_expr);
}

static struct ast* parse_array_pattern(struct parser* parser) {
    return parse_array_expr_or_pattern(parser, AST_ARRAY_PATTERN, parse_pattern);
}

static struct ast* parse_int_literal(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    uintmax_t int_val = parser->ahead->int_val;
    eat_token(parser, TOKEN_INT_LITERAL);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_LITERAL,
        .literal = { .tag = LITERAL_INT, .int_val = int_val }
    });
}

static struct ast* parse_float_literal(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    double float_val = parser->ahead->float_val;
    eat_token(parser, TOKEN_FLOAT_LITERAL);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_LITERAL,
        .literal = { .tag = LITERAL_FLOAT, .float_val = float_val }
    });
}

static struct ast* parse_block_expr(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_L_BRACE);
    struct ast* stmts = NULL, *last_stmt = NULL;
    while (
        parser->ahead->tag != TOKEN_R_BRACE &&
        parser->ahead->tag != TOKEN_EOF)
    {
        bool needs_semicolon = false;
        append_ast(&stmts, &last_stmt, parse_stmt(parser, &needs_semicolon));
        if (needs_semicolon && !accept_token(parser, TOKEN_SEMICOLON))
            break;
    }
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast(parser, &begin, &(struct ast) { .tag = AST_BLOCK_EXPR, .block_expr.stmts = stmts });
}

static struct ast* parse_block_or_error_expr(struct parser* parser) {
    if (parser->ahead->tag == TOKEN_L_BRACE)
        return parse_block_expr(parser);
    return parse_error(parser, "block");
}

static struct ast* parse_while_loop(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_WHILE);
    struct ast* cond = parse_expr_without_struct(parser);
    struct ast* body = parse_block_or_error_expr(parser);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_WHILE_LOOP,
        .while_loop = {
            .cond = cond,
            .body = body
        }
    });
}

static struct ast* parse_for_loop(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_FOR);
    struct ast* pattern = parse_pattern(parser);
    eat_token(parser, TOKEN_IN);
    struct ast* iter_expr = parse_expr(parser);
    struct ast* body = parse_block_or_error_expr(parser);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_FOR_LOOP,
        .for_loop = {
            .pattern = pattern,
            .iter_expr = iter_expr,
            .body = body
        }
    });
}

static struct ast* parse_param(struct parser* parser) {
    if (parser->ahead->tag != TOKEN_L_PAREN)
        return parse_error(parser, "function parameter");
    return parse_pattern(parser);
}

static struct ast* parse_fun_decl(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_FUN);

    struct ast* filter = NULL;
    if (parser->ahead->tag == TOKEN_FILTER)
        filter = parse_filter(parser);

    const char* name  = parse_ident_as_string(parser);
    struct ast* type_params = parse_type_params(parser);
    struct ast* param = parse_param(parser);

    bool needs_equal = parser->ahead->tag != TOKEN_L_BRACE;
    if (needs_equal) expect_token(parser, TOKEN_EQUAL);
    struct ast* body  = parse_expr(parser);
    if (needs_equal) expect_token(parser, TOKEN_SEMICOLON);

    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_FUN_DECL,
        .fun_decl = {
            .name = name,
            .type_params = type_params,
            .filter = filter,
            .body = body,
            .param = param
        }
    });
}

static struct ast* parse_var_or_const_decl(struct parser* parser, enum ast_tag tag) {
    struct file_pos begin = parser->ahead->loc.begin;
    skip_token(parser);
    struct ast* pattern = parse_pattern(parser);
    struct ast* init = NULL;
    if (tag == AST_CONST_DECL || parser->ahead->tag == TOKEN_EQUAL) {
        expect_token(parser, TOKEN_EQUAL);
        init = parse_expr(parser);
    }
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = tag,
        .var_decl = {
            .pattern = pattern,
            .init = init
        }
    });
}

static struct ast* parse_decl(struct parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_FUN:    return parse_fun_decl(parser);
        case TOKEN_STRUCT: return parse_struct_decl(parser);
        case TOKEN_ENUM:   return parse_enum_decl(parser);
        case TOKEN_VAR:    return parse_var_or_const_decl(parser, AST_VAR_DECL);
        case TOKEN_CONST:  return parse_var_or_const_decl(parser, AST_CONST_DECL);
        default:
            return parse_error(parser, "declaration");
    }
}

static struct ast* parse_stmt(struct parser* parser, bool* needs_semicolon) {
    switch (parser->ahead->tag) {
        case TOKEN_WHILE:  return parse_while_loop(parser);
        case TOKEN_FOR:    return parse_for_loop(parser);
        case TOKEN_FUN:    return parse_fun_decl(parser);
        case TOKEN_STRUCT: return parse_struct_decl(parser);
        case TOKEN_ENUM:   return parse_enum_decl(parser);
        case TOKEN_VAR:    return parse_var_or_const_decl(parser, AST_VAR_DECL);
        case TOKEN_CONST:  return parse_var_or_const_decl(parser, AST_CONST_DECL);
        default: {
            struct ast* expr = parse_expr(parser);
            *needs_semicolon = expr->tag != AST_IF_EXPR;
            return expr;
        }
    }
}

static struct ast* parse_if_expr(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_IF);
    struct ast* cond = parse_expr_without_struct(parser);
    struct ast* branch_true = parse_block_or_error_expr(parser);
    struct ast* branch_false = NULL;
    if (accept_token(parser, TOKEN_ELSE)) {
        branch_false = parser->ahead->tag == TOKEN_IF
            ? parse_if_expr(parser)
            : parse_block_or_error_expr(parser);
    }
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_IF_EXPR,
        .if_expr = {
            .cond = cond,
            .branch_true = branch_true,
            .branch_false = branch_false
        }
    });
}

static struct ast* parse_match_case(struct parser* parser) {
    struct ast* pattern = parse_pattern(parser);
    expect_token(parser, TOKEN_FAT_ARROW);
    struct ast* case_val = parse_expr(parser);
    return make_ast(parser, &pattern->loc.begin, &(struct ast) {
        .tag = AST_MATCH_CASE,
        .match_case = {
            .pattern = pattern,
            .case_val = case_val
        }
    });
}

static struct ast* parse_match_expr(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    eat_token(parser, TOKEN_MATCH);
    struct ast* match_val = parse_expr_without_struct(parser);
    expect_token(parser, TOKEN_L_BRACE);
    struct ast* cases = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_match_case);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_MATCH_EXPR,
        .match_expr = {
            .match_val = match_val,
            .cases = cases
        }
    });
}

static struct ast* parse_field_expr(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    const char* name = parse_ident_as_string(parser);
    expect_token(parser, TOKEN_COLON);
    struct ast* val = parse_expr(parser);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_FIELD_EXPR,
        .field_expr = {
            .name = name,
            .val = val
        }
    });
}

static struct ast* parse_field_pattern(struct parser* parser) {
    struct file_pos begin = parser->ahead->loc.begin;
    const char* name = parse_ident_as_string(parser);
    expect_token(parser, TOKEN_COLON);
    struct ast* val = parse_pattern(parser);
    return make_ast(parser, &begin, &(struct ast) {
        .tag = AST_FIELD_PATTERN,
        .field_expr = {
            .name = name,
            .val = val
        }
    });
}

static struct ast* parse_struct_expr_or_pattern(
    struct parser* parser,
    enum ast_tag tag,
    struct ast* path,
    struct ast* (*parse_field)(struct parser*))
{
    eat_token(parser, TOKEN_L_BRACE);
    struct ast* fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast(parser, &path->loc.begin, &(struct ast) {
        .tag = tag,
        .struct_expr = {
            .path = path,
            .fields = fields
        }
    });
}

static struct ast* parse_struct_expr(struct parser* parser, struct ast* path) {
    return parse_struct_expr_or_pattern(parser, AST_STRUCT_EXPR, path, parse_field_expr);
}

static struct ast* parse_struct_pattern(struct parser* parser, struct ast* path) {
    return parse_struct_expr_or_pattern(parser, AST_STRUCT_PATTERN, path, parse_field_pattern);
}

static struct ast* parse_primary_expr(struct parser* parser, bool accept_structs) {
    switch (parser->ahead->tag) {
        case TOKEN_IDENT: {
            struct ast* path = parse_path(parser);
            if (accept_structs && parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct_expr(parser, path);
            return path;
        }
        case TOKEN_L_PAREN:       return parse_tuple_expr(parser);
        case TOKEN_L_BRACKET:     return parse_array_expr(parser);
        case TOKEN_L_BRACE:       return parse_block_expr(parser);
        case TOKEN_INT_LITERAL:   return parse_int_literal(parser);
        case TOKEN_FLOAT_LITERAL: return parse_float_literal(parser);
        case TOKEN_IF:            return parse_if_expr(parser);
        case TOKEN_MATCH:         return parse_match_expr(parser);
        default:
            return parse_error(parser, "expression");
    }
}

static struct ast* parse_call_expr(struct parser* parser, struct ast* callee) {
    struct ast* arg = parse_tuple_expr(parser);
    return make_ast(parser, &callee->loc.begin, &(struct ast) {
        .tag = AST_CALL_EXPR,
        .call_expr = {
            .callee = callee,
            .arg = arg
        }
    });
}

static struct ast* parse_postfix_expr(struct parser* parser, bool accept_structs) {
    struct ast* expr = parse_primary_expr(parser, accept_structs);
    while (true) {
        switch (parser->ahead->tag) {
            case TOKEN_COLON:
                return parse_type_annot(parser, expr);
            case TOKEN_L_PAREN:
                return parse_call_expr(parser, expr);
            default:
                return expr;
        }
    }
}

static struct ast* parse_expr(struct parser* parser) {
    return parse_postfix_expr(parser, true);
}

static struct ast* parse_expr_without_struct(struct parser* parser) {
    return parse_postfix_expr(parser, false);
}

static struct ast* parse_call_pattern(struct parser* parser, struct ast* callee) {
    struct ast* arg = parse_tuple_pattern(parser);
    return make_ast(parser, &callee->loc.begin, &(struct ast) {
        .tag = AST_CALL_PATTERN,
        .call_pattern = {
            .callee = callee,
            .arg = arg
        }
    });
}

static struct ast* parse_primary_pattern(struct parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_IDENT: {
            struct ast* path = parse_path(parser);
            if (parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct_pattern(parser, path);
            if (parser->ahead->tag == TOKEN_L_PAREN)
                return parse_call_pattern(parser, path);
            return path;
        }
        case TOKEN_L_PAREN:     return parse_tuple_pattern(parser);
        case TOKEN_L_BRACKET:   return parse_array_pattern(parser);
        case TOKEN_INT_LITERAL: return parse_int_literal(parser);
        default:
            return parse_error(parser, "pattern");
    }
}

static struct ast* parse_postfix_pattern(struct parser* parser) {
    struct ast* pattern = parse_primary_pattern(parser);
    if (parser->ahead->tag == TOKEN_COLON)
        return parse_type_annot(parser, pattern);
    return pattern;
}

static struct ast* parse_pattern(struct parser* parser) {
    return parse_postfix_pattern(parser);
}

static struct ast* parse_program(struct parser* parser) {
    return parse_many(parser, TOKEN_EOF, TOKEN_INVALID, parse_decl);
}

static void register_keywords(struct lexer* lexer) {
#define f(x, str) \
    must_succeed(insert_in_hash_table( \
        &lexer->keywords, \
        &(struct keyword) { .name = str, .len = strlen(str), .tag = TOKEN_##x }, \
        hash_string(hash_init(), str), \
        sizeof(struct keyword), \
        compare_keywords));
    KEYWORDS(f)
#undef f
}

struct ast* parse_ast(
    struct mem_pool* mem_pool,
    const char* file_name,
    const char* file_data,
    size_t data_size,
    struct log* log)
{
    enum {
#define f(x, str) KEYWORD_##x,
        KEYWORDS(f)
#undef f
        KEYWORD_COUNT
    };
    struct file_pos begin = { .row = 1, .col = 1, .byte_offset = 0 };
    struct parser parser = {
        .mem_pool = mem_pool,
        .lexer = {
            .log = log,
            .cur_pos = begin,
            .keywords = new_hash_table(KEYWORD_COUNT, sizeof(struct keyword)),
            .file_name = file_name,
            .data = file_data,
            .data_size = data_size
        }
    };
    register_keywords(&parser.lexer);
    for (size_t i = 0; i < LOOKAHEAD; ++i)
        parser.ahead[i] = advance_lexer(&parser.lexer);
    struct ast* ast = parse_program(&parser);
    free_hash_table(&parser.lexer.keywords);
    return ast;
}
