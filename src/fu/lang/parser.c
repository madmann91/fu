#include "fu/lang/parser.h"
#include "fu/lang/ast.h"
#include "fu/lang/lexer.h"
#include "fu/core/mem_pool.h"
#include "fu/core/alloc.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
    AstNode* first;
    AstNode* last;
} AstNodeList;

static inline AstNode* parse_decl_without_attr_list(Parser*, bool, bool, bool);
static inline AstNode* parse_struct_decl(Parser*, bool, bool);
static inline AstNode* parse_enum_decl(Parser*, bool, bool);
static inline AstNode* parse_type_decl(Parser*, bool, bool, bool);
static inline AstNode* parse_sig_decl(Parser*, bool);

static inline void add_ast_node_to_list(AstNodeList* list, AstNode* node) {
    if (!list->last)
        list->first = node;
    else {
        assert(list->first);
        list->last->next = node;
    }
    while (node->next) node = node->next;
    list->last = node;
}

Parser make_parser(Lexer* lexer, MemPool* mem_pool) {
    Parser parser = {
        .lexer = lexer,
        .mem_pool = mem_pool,
        .prev_end = { .row = 1, .col = 1 }
    };
    for (size_t i = 0; i < LOOK_AHEAD; ++i)
        parser.ahead[i] = advance_lexer(lexer);
    return parser;
}

static inline AstNode* make_ast_node(Parser* parser, const FilePos* begin, const AstNode* node) {
    AstNode* copy = alloc_from_mem_pool(parser->mem_pool, sizeof(AstNode));
    memcpy(copy, node, sizeof(AstNode));
    copy->file_loc.file_name = parser->lexer->file_name;
    copy->file_loc.begin = *begin;
    copy->file_loc.end = parser->prev_end;
    return copy;
}

static inline const char* copy_file_data(Parser* parser, size_t begin, size_t end) {
    size_t len = end - begin;
    char* name = alloc_from_mem_pool(parser->mem_pool, len + 1);
    memcpy(name, parser->lexer->file_data + begin, len);
    name[len] = 0;
    return name;
}

static inline void skip_token(Parser* parser) {
    parser->prev_end = parser->ahead->file_loc.end;
    for (size_t i = 0; i < LOOK_AHEAD - 1; ++i)
        parser->ahead[i] = parser->ahead[i + 1];
    parser->ahead[LOOK_AHEAD - 1] = advance_lexer(parser->lexer);
}

static inline void eat_token(Parser* parser, TokenTag tag) {
    assert(parser->ahead->tag == tag);
    skip_token(parser);
    (void)tag;
}

static inline bool accept_token(Parser* parser, TokenTag tag) {
    if (parser->ahead->tag == tag) {
        skip_token(parser);
        return true;
    }
    return false;
}

static void report_invalid_token(
    Parser* parser,
    const char* expected_msg,
    const char* found_msg,
    const FileLoc* file_loc)
{
    log_error(parser->lexer->log, file_loc, "expected {s}, but got {s}",
        (FormatArg[]) { { .s = expected_msg }, { .s = found_msg } });
}

static void report_empty(Parser* parser, const char* msg, const FileLoc* file_loc)
{
    log_error(parser->lexer->log, file_loc,
        "empty {s} are not allowed", (FormatArg[]) { { .s = msg } });
}

static inline bool expect_token(Parser* parser, TokenTag tag) {
    if (!accept_token(parser, tag)) {
        report_invalid_token(parser, token_tag_to_str(tag), token_tag_to_str(parser->ahead->tag), &parser->ahead->file_loc);
        return false;
    }
    return true;
}

static AstNode* parse_many(Parser* parser, TokenTag end, TokenTag sep, AstNode* (*parse_one)(Parser*)) {
    AstNodeList list = { NULL, NULL };
    while (parser->ahead->tag != TOKEN_EOF) {
        if (end != TOKEN_ERROR && parser->ahead->tag == end)
            break;
        add_ast_node_to_list(&list, parse_one(parser));
        if (sep != TOKEN_ERROR && !accept_token(parser, sep))
            break;
    }
    return list.first;
}

static AstNode* parse_many_at_least_one(
    Parser* parser,
    const char* msg,
    TokenTag end, TokenTag sep,
    AstNode* (*parse_one)(Parser*))
{
    AstNode* ast_nodes = parse_many(parser, end, sep, parse_one);
    if (!ast_nodes)
        report_empty(parser, msg, &parser->ahead->file_loc);
    return ast_nodes;
}

static inline const char* parse_ident(Parser* parser) {
    const char* name = copy_file_data(parser,
        parser->ahead->file_loc.begin.byte_offset,
        parser->ahead->file_loc.end.byte_offset);
    expect_token(parser, TOKEN_IDENT);
    return name;
}

static inline AstNode* parse_error(Parser* parser, const char* msg) {
    FilePos begin = parser->ahead->file_loc.begin;
    report_invalid_token(parser, msg, token_tag_to_str(parser->ahead->tag), &parser->ahead->file_loc);
    skip_token(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_ERROR });
}

static inline AstNode* parse_tuple(Parser* parser, AstNodeTag tag, AstNode* (*parse_arg)(Parser*)) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_L_PAREN);
    AstNode* args = parse_many(parser, TOKEN_R_PAREN, TOKEN_COMMA, parse_arg);
    expect_token(parser, TOKEN_R_PAREN);
    if (args && !args->next)
        return args;
    return make_ast_node(parser, &begin, &(AstNode) { .tag = tag, .tuple_expr.args = args });
}

static inline AstNode* parse_tuple_or_error(Parser* parser, const char* msg, AstNode* (*parse_tuple)(Parser*)) {
    return parser->ahead->tag == TOKEN_L_PAREN ? parse_tuple(parser) : parse_error(parser, msg);
}

static inline AstNode* parse_tuple_type(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_TYPE, parse_type);
}

static inline AstNode* parse_tuple_pattern(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_PATTERN, parse_pattern);
}

static inline AstNode* parse_tuple_expr(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_EXPR, parse_expr);
}

static AstNode* parse_array(Parser* parser, AstNodeTag tag, AstNode* (*parse_elem)(Parser*)) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_L_BRACKET);
    AstNode* elems = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_elem);
    expect_token(parser, TOKEN_R_BRACKET);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = tag, .array_expr.elems = elems });
}

static inline AstNode* parse_array_expr(Parser* parser) {
    return parse_array(parser, AST_ARRAY_EXPR, parse_expr);
}

static inline AstNode* parse_array_pattern(Parser* parser) {
    return parse_array(parser, AST_ARRAY_PATTERN, parse_pattern);
}

static inline AstNode* parse_array_type(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_L_BRACKET);
    AstNode* elem_type = parse_type(parser);
    expect_token(parser, TOKEN_R_BRACKET);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_ARRAY_TYPE,
        .array_type.elem_type = elem_type
    });
}

static inline AstNode* parse_path_elem(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    AstNode* type_args = NULL;
    if (accept_token(parser, TOKEN_L_BRACKET)) {
        type_args = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_type);
        expect_token(parser, TOKEN_R_BRACKET);
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_PATH_ELEM,
        .path_elem = { .name = name, .type_args = type_args }
    });
}

static inline AstNode* parse_path_elems(Parser* parser) {
    AstNodeList elem_list = { NULL, NULL };
    do {
        add_ast_node_to_list(&elem_list, parse_path_elem(parser));
        // Note that `x.0` is not a path, and thus should not be parsed as a path,
        // but as a member expression.
        if (parser->ahead[0].tag != TOKEN_DOT || parser->ahead[1].tag != TOKEN_IDENT)
            break;
        eat_token(parser, TOKEN_DOT);
    } while (true);
    return elem_list.first;
}

static inline AstNode* parse_path(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* elems = parse_path_elems(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_PATH, .path.elems = elems });
}

static inline AstNode* make_single_elem_path(Parser* parser, const char* name, const FileLoc* file_loc) {
    AstNode* path_elem = make_ast_node(parser, &file_loc->begin, &(AstNode) {
        .tag = AST_PATH_ELEM,
        .path_elem = { .name = name }
    });
    AstNode* path = make_ast_node(parser, &file_loc->begin, &(AstNode) {
        .tag = AST_PATH,
        .path.elems = path_elem
    });
    path_elem->file_loc = *file_loc;
    path->file_loc = *file_loc;
    return path;
}

static inline AstNode* parse_bool_literal(Parser* parser, bool val) {
    FilePos begin = parser->ahead->file_loc.begin;
    skip_token(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_BOOL_LITERAL, .bool_literal.val = val });
}

static inline AstNode* parse_str_literal(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    // Skip enclosing `"`
    const char* val = copy_file_data(parser,
        parser->ahead->file_loc.begin.byte_offset + 1,
        parser->ahead->file_loc.end.byte_offset - 1);
    eat_token(parser, TOKEN_STR_LITERAL);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_STR_LITERAL, .str_literal.val = val });
}

static inline AstNode* parse_char_literal(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    // Skip enclosing `'`
    char val = parser->lexer->file_data[parser->ahead->file_loc.begin.byte_offset + 1];
    eat_token(parser, TOKEN_CHAR_LITERAL);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_CHAR_LITERAL, .char_literal.val = val });
}

static inline AstNode* parse_int_literal(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    uintmax_t val = parser->ahead->int_val;
    eat_token(parser, TOKEN_INT_LITERAL);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_INT_LITERAL, .int_literal.val = val });
}

static inline AstNode* parse_float_literal(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    double val = parser->ahead->float_val;
    eat_token(parser, TOKEN_FLOAT_LITERAL);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_FLOAT_LITERAL, .float_literal.val = val });
}

static inline AstNode* parse_attr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    if (accept_token(parser, TOKEN_L_PAREN)) {
        AstNode* attrs = parse_many(parser, TOKEN_R_PAREN, TOKEN_COMMA, parse_attr);
        expect_token(parser, TOKEN_R_PAREN);
        return make_ast_node(parser, &begin, &(AstNode) {
            .tag = AST_ATTR,
            .attr = { .name = name, .val = attrs }
        });
    }
    AstNode* val = NULL;
    if (accept_token(parser, TOKEN_EQUAL)) {
        switch (parser->ahead->tag) {
            case TOKEN_TRUE:          val = parse_bool_literal(parser, true); break;
            case TOKEN_FALSE:         val = parse_bool_literal(parser, false); break;
            case TOKEN_INT_LITERAL:   val = parse_int_literal(parser);   break;
            case TOKEN_FLOAT_LITERAL: val = parse_float_literal(parser); break;
            case TOKEN_CHAR_LITERAL:  val = parse_char_literal(parser);  break;
            case TOKEN_STR_LITERAL:   val = parse_str_literal(parser);   break;
            default:
                val = parse_error(parser, "attribute value");
                break;
        }
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_ATTR,
        .attr = { .name = name, .val = val }
    });
}

static inline AstNode* parse_attr_list(Parser* parser) {
    eat_token(parser, TOKEN_HASH);
    expect_token(parser, TOKEN_L_BRACKET);
    AstNode* attrs = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_attr);
    expect_token(parser, TOKEN_R_BRACKET);
    return attrs;
}

static inline AstNode* parse_block_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_L_BRACE);
    AstNodeList stmt_list = { NULL, NULL };
    bool ends_with_semicolon = false;
    while (parser->ahead->tag != TOKEN_R_BRACE) {
        AstNode* stmt = parse_stmt(parser);
        add_ast_node_to_list(&stmt_list, stmt);
        ends_with_semicolon = accept_token(parser, TOKEN_SEMICOLON);
        if (needs_semicolon(stmt->tag) && !ends_with_semicolon)
            break;
    }
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_BLOCK_EXPR,
        .block_expr = { .stmts = stmt_list.first, .ends_with_semicolon = ends_with_semicolon }
    });
}

static AstNode* parse_primary_kind(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    if (accept_token(parser, TOKEN_STAR))
        return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_KIND_STAR });
    return parse_path(parser);
}

static AstNode* parse_kind(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    if (accept_token(parser, TOKEN_L_PAREN)) {
        AstNode* dom_kinds = parse_many_at_least_one(
            parser, "domain kinds", TOKEN_R_PAREN, TOKEN_COMMA, parse_primary_kind);
        if (accept_token(parser, TOKEN_FAT_ARROW)) {
            AstNode* codom_kind = parse_kind(parser);
            return make_ast_node(parser, &begin, &(AstNode) {
                .tag = AST_KIND_ARROW,
                .arrow_kind = {
                    .dom_kinds = dom_kinds,
                    .codom_kind = codom_kind
                }
            });
        } else if (!dom_kinds->next) // Allow placing kinds in parentheses
            return dom_kinds;
        else
            return parse_error(parser, "kind");
    } else
        return parse_primary_kind(parser);
}

static AstNode* parse_type_param(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    AstNode* kind = NULL;
    if (accept_token(parser, TOKEN_COLON))
        kind = parse_kind(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_TYPE_PARAM,
        .type_param = { .name = name, .kind = kind }
    });
}

static inline AstNode* parse_basic_type(Parser* parser, AstNodeTag tag) {
    FilePos begin = parser->ahead->file_loc.begin;
    skip_token(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = tag });
}

static inline AstNode* parse_fun_type(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FUN);
    AstNode* dom_type = parse_tuple_or_error(parser, "function type domain", parse_tuple_type);
    expect_token(parser, TOKEN_THIN_ARROW);
    AstNode* codom_type = parse_type(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FUN_TYPE,
        .fun_type = { .dom_type = dom_type, .codom_type = codom_type }
    });
}

static inline AstNode* parse_ptr_type(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_AMP);
    bool is_const = accept_token(parser, TOKEN_CONST);
    AstNode* pointed_type = parse_type(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_PTR_TYPE,
        .ptr_type = { .is_const = is_const, .pointed_type = pointed_type }
    });
}

static inline AstNode* parse_where_clause(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    expect_token(parser, TOKEN_EQUAL);
    AstNode* type = parse_type(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_WHERE_CLAUSE,
        .where_clause = { .name = name, .type = type }
    });
}

static inline AstNode* parse_where_type(Parser* parser, AstNode* path) {
    eat_token(parser, TOKEN_WHERE);
    AstNode* clauses = NULL;
    if (accept_token(parser, TOKEN_L_BRACE)) {
        clauses = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_where_clause);
        expect_token(parser, TOKEN_R_BRACE);
    } else
        clauses = parse_where_clause(parser);
    return make_ast_node(parser, &path->file_loc.begin, &(AstNode) {
        .tag = AST_WHERE_TYPE,
        .where_type = { .path = path, .clauses = clauses }
    });
}

AstNode* parse_type(Parser* parser) {
    switch (parser->ahead->tag) {
#define f(name, ...) case TOKEN_##name: return parse_basic_type(parser, AST_TYPE_##name);
        PRIM_TYPE_LIST(f)
#undef f
        case TOKEN_IDENT: {
            AstNode* path = parse_path(parser);
            if (parser->ahead->tag == TOKEN_WHERE)
                return parse_where_type(parser, path);
            return path;
        }
        case TOKEN_L_PAREN:   return parse_tuple_type(parser);
        case TOKEN_L_BRACKET: return parse_array_type(parser);
        case TOKEN_FUN:       return parse_fun_type(parser);
        case TOKEN_BANG:      return parse_basic_type(parser, AST_NORET_TYPE);
        case TOKEN_AMP:       return parse_ptr_type(parser);
        case TOKEN_SIG:       return parse_sig_decl(parser, false);
        default:
            return parse_error(parser, "type");
    }
}

static AstNode* parse_struct(
    Parser* parser,
    AstNodeTag tag,
    AstNode* left,
    AstNode* (*parse_field)(Parser*))
{
    eat_token(parser, TOKEN_L_BRACE);
    AstNode* fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &left->file_loc.begin, &(AstNode) {
        .tag = tag,
        .struct_expr = { .left = left, .fields = fields }
    });
}

static AstNode* parse_field(Parser* parser, AstNodeTag tag, AstNode* (*parse_val)(Parser*)) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    expect_token(parser, TOKEN_EQUAL);
    AstNode* val = parse_val(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = tag,
        .field_expr = { .name = name, .val = val }
    });
}

static AstNode* parse_field_pattern(Parser* parser) {
    return parse_field(parser, AST_FIELD_PATTERN, parse_pattern);
}

static AstNode* parse_field_expr(Parser* parser) {
    return parse_field(parser, AST_FIELD_EXPR, parse_expr);
}

static AstNode* parse_ctor_pattern(Parser* parser, AstNode* path) {
    AstNode* arg = parse_tuple_pattern(parser);
    return make_ast_node(parser, &path->file_loc.begin, &(AstNode) {
        .tag = AST_CTOR_PATTERN,
        .ctor_pattern = { .path = path, .arg = arg }
    });
}

static inline AstNodeTag token_tag_to_binary_expr_tag(TokenTag tag) {
    switch (tag) {
#define f(name, prec, token, ...) case TOKEN_##token: return AST_##name##_EXPR;
        AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            return AST_ERROR;
    }
}

static inline AstNode* parse_call_expr(Parser* parser, AstNode* callee) {
    AstNode* arg = parse_tuple_expr(parser);
    return make_ast_node(parser, &callee->file_loc.begin, &(AstNode) {
        .tag = AST_CALL_EXPR,
        .call_expr = { .callee = callee, .arg = arg }
    });
}

static inline AstNode* parse_member_expr(Parser* parser, AstNode* left) {
    AstNode* elems_or_index = NULL;
    if (parser->ahead->tag == TOKEN_INT_LITERAL)
        elems_or_index = parse_int_literal(parser);
    else
        elems_or_index = parse_path_elems(parser);
    return make_ast_node(parser, &left->file_loc.begin, &(AstNode) {
        .tag = AST_MEMBER_EXPR,
        .member_expr = { .left = left, .elems_or_index = elems_or_index }
    });
}

static inline AstNode* parse_postfix_expr(Parser* parser, AstNode* (*parse_primary_expr)(Parser*)) {
    AstNode* operand = parse_primary_expr(parser);
    while (true) {
        AstNodeTag tag = AST_ERROR;
        switch (parser->ahead->tag) {
            case TOKEN_DOUBLE_PLUS:  tag = AST_POST_INC_EXPR; break;
            case TOKEN_DOUBLE_MINUS: tag = AST_POST_DEC_EXPR; break;
            case TOKEN_DOT:
                eat_token(parser, TOKEN_DOT);
                if (parser->ahead->tag == TOKEN_L_BRACE)
                    operand = parse_struct(parser, AST_UPDATE_EXPR, operand, parse_field_expr);
                else
                    operand = parse_member_expr(parser, operand);
                continue;
            case TOKEN_L_PAREN:
                operand = parse_call_expr(parser, operand);
                continue;
            default:
                return operand;
        }
        FilePos begin = parser->ahead->file_loc.begin;
        skip_token(parser);
        operand = make_ast_node(parser,
            &begin, &(AstNode) { .tag = tag, .unary_expr = { .operand = operand } });
    }
}

static inline AstNode* parse_prefix_expr(Parser* parser, AstNode* (*parse_primary_expr)(Parser*)) {
    AstNodeTag tag = AST_ERROR;
    switch (parser->ahead->tag) {
        case TOKEN_DOUBLE_PLUS:  tag = AST_PRE_INC_EXPR; break;
        case TOKEN_DOUBLE_MINUS: tag = AST_PRE_DEC_EXPR; break;
        case TOKEN_BANG:         tag = AST_NOT_EXPR;     break;
        case TOKEN_MINUS:        tag = AST_MINUS_EXPR;   break;
        case TOKEN_PLUS:         tag = AST_PLUS_EXPR;    break;
        case TOKEN_AMP:          tag = AST_ADDR_OF_EXPR; break;
        case TOKEN_STAR:         tag = AST_DEREF_EXPR;   break;
        default:
            return parse_postfix_expr(parser, parse_primary_expr);
    }
    FilePos begin = parser->ahead->file_loc.begin;
    skip_token(parser);
    AstNode* operand = parse_prefix_expr(parser, parse_primary_expr);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = tag, .unary_expr = { .operand = operand } });
}

static inline AstNode* parse_binary_expr(
    Parser* parser, AstNode* left, AstNode* (*parse_primary_expr)(Parser*), int prec)
{
    while (true) {
        AstNodeTag tag = token_tag_to_binary_expr_tag(parser->ahead->tag);
        if (tag == AST_ERROR)
            break;
        int next_prec = get_binary_expr_precedence(tag);
        if (next_prec > prec)
            break;
        if (next_prec < prec)
            left = parse_binary_expr(parser, left, parse_primary_expr, next_prec);
        else {
            skip_token(parser);
            AstNode* right = parse_prefix_expr(parser, parse_primary_expr);
            left = make_ast_node(parser, &left->file_loc.begin, &(AstNode) {
                .tag = tag,
                .binary_expr = { .left = left, .right = right }
            });
        }
    }
    return left;
}

static inline AstNodeTag token_tag_to_assign_expr_tag(TokenTag tag) {
    switch (tag) {
        case TOKEN_EQUAL: return AST_ASSIGN_EXPR;
#define f(name, prec, token, ...) case TOKEN_##token##_EQUAL: return AST_##name##_ASSIGN_EXPR;
        AST_ASSIGN_EXPR_LIST(f)
#undef f
        default:
            return AST_ERROR;
    }
}

static inline AstNode* parse_assign_expr(Parser* parser, AstNode* (*parse_primary_expr)(Parser*)) {
    AstNode* left = parse_prefix_expr(parser, parse_primary_expr);
    AstNodeTag tag = token_tag_to_assign_expr_tag(parser->ahead->tag);
    if (tag != AST_ERROR) {
        skip_token(parser);
        AstNode* right = parse_assign_expr(parser, parse_primary_expr);
        return make_ast_node(parser, &left->file_loc.begin, &(AstNode) {
            .tag = tag,
            .binary_expr = { .left = left, .right = right }
        });
    }
    return parse_binary_expr(parser, left, parse_primary_expr, get_max_binary_expr_precedence());
}

static AstNode* parse_primary_expr(Parser*, bool);

static AstNode* parse_primary_expr_with_structs(Parser* parser) {
    return parse_primary_expr(parser, true);
}

static AstNode* parse_primary_expr_without_structs(Parser* parser) {
    return parse_primary_expr(parser, false);
}

AstNode* parse_expr(Parser* parser) {
    return parse_assign_expr(parser, parse_primary_expr_with_structs);
}

static inline AstNode* parse_expr_without_structs(Parser* parser) {
    return parse_assign_expr(parser, parse_primary_expr_without_structs);
}

static inline AstNode* parse_block_expr_or_error(Parser* parser) {
    return parser->ahead->tag == TOKEN_L_BRACE
        ? parse_block_expr(parser) : parse_error(parser, "block expression");
}

static inline AstNode* parse_if_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_IF);
    AstNode* cond = parse_expr_without_structs(parser);
    AstNode* then_expr = parse_block_expr_or_error(parser);
    AstNode* else_expr = NULL;
    if (accept_token(parser, TOKEN_ELSE)) {
        else_expr = parser->ahead->tag == TOKEN_IF
            ? parse_if_expr(parser) : parse_block_expr_or_error(parser);
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_IF_EXPR,
        .if_expr = { .cond = cond, .then_expr = then_expr, .else_expr = else_expr }
    });
}

static AstNode* parse_match_case(Parser* parser) {
    AstNode* pattern = parse_pattern(parser);
    expect_token(parser, TOKEN_FAT_ARROW);
    AstNode* val = parse_expr(parser);
    return make_ast_node(parser, &pattern->file_loc.begin, &(AstNode) {
        .tag = AST_MATCH_CASE,
        .match_case = { .pattern = pattern, .val = val }
    });
}

static inline AstNode* parse_match_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_MATCH);
    AstNode* arg = parse_expr_without_structs(parser);
    expect_token(parser, TOKEN_L_BRACE);
    AstNode* cases = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_match_case);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_MATCH_EXPR,
        .match_expr = { .arg = arg, .cases = cases }
    });
}

static inline AstNode* parse_fun_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FUN);
    AstNode* param = parse_tuple_or_error(parser, "anonymous function parameter", parse_tuple_pattern);
    AstNode* ret_type = NULL;
    if (accept_token(parser, TOKEN_THIN_ARROW))
        ret_type = parse_type(parser);
    expect_token(parser, TOKEN_FAT_ARROW);
    AstNode* body = parse_expr(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FUN_EXPR,
        .fun_expr = { .param = param, .ret_type = ret_type, .body = body }
    });
}

static inline AstNode* parse_untyped_expr(Parser* parser, bool allow_structs) {
    switch (parser->ahead->tag) {
        case TOKEN_TRUE:          return parse_bool_literal(parser, true);
        case TOKEN_FALSE:         return parse_bool_literal(parser, false);
        case TOKEN_STR_LITERAL:   return parse_str_literal(parser);
        case TOKEN_CHAR_LITERAL:  return parse_char_literal(parser);
        case TOKEN_INT_LITERAL:   return parse_int_literal(parser);
        case TOKEN_FLOAT_LITERAL: return parse_float_literal(parser);
        case TOKEN_IF:            return parse_if_expr(parser);
        case TOKEN_MATCH:         return parse_match_expr(parser);
        case TOKEN_L_PAREN:       return parse_tuple_expr(parser);
        case TOKEN_L_BRACE:       return parse_block_expr(parser);
        case TOKEN_L_BRACKET:     return parse_array_expr(parser);
        case TOKEN_FUN:           return parse_fun_expr(parser);
        case TOKEN_IDENT: {
            AstNode* path = parse_path(parser);
            if (allow_structs && parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct(parser, AST_STRUCT_EXPR, path, parse_field_expr);
            return path;
        }
        case TOKEN_BREAK:
        case TOKEN_CONTINUE:
        case TOKEN_RETURN: {
            FilePos begin = parser->ahead->file_loc.begin;
            AstNodeTag tag =
                parser->ahead->tag == TOKEN_BREAK ? AST_BREAK_EXPR :
                parser->ahead->tag == TOKEN_CONTINUE ? AST_CONTINUE_EXPR :
                AST_RETURN_EXPR;
            skip_token(parser);
            return make_ast_node(parser, &begin, &(AstNode) { .tag = tag });
        }
        default:
            return parse_error(parser, "expression");
    }
}

static inline AstNode* parse_primary_expr(Parser* parser, bool allow_structs) {
    AstNode* expr = parse_untyped_expr(parser, allow_structs);
    if (accept_token(parser, TOKEN_COLON)) {
        AstNode* type = parse_type(parser);
        return make_ast_node(parser, &expr->file_loc.begin, &(AstNode) {
            .tag = AST_TYPED_EXPR,
            .typed_pattern = { .left = expr, .type = type }
        });
    }
    return expr;
}

static AstNode* parse_anonymous_pattern(Parser* parser, AstNode* type) {
    return make_ast_node(parser, &type->file_loc.begin, &(AstNode) {
        .tag = AST_TYPED_PATTERN,
        .typed_pattern = {
            .left = make_ast_node(parser, &type->file_loc.begin, &(AstNode) {
                .tag = AST_IDENT_PATTERN,
                .ident_pattern.name = "_"
            }),
            .type = type
        }
    });
}

static AstNode* parse_untyped_pattern(Parser* parser, bool is_fun_param) {
    switch (parser->ahead->tag) {
        case TOKEN_TRUE:          return parse_bool_literal(parser, true);
        case TOKEN_FALSE:         return parse_bool_literal(parser, false);
        case TOKEN_STR_LITERAL:   return parse_str_literal(parser);
        case TOKEN_CHAR_LITERAL:  return parse_char_literal(parser);
        case TOKEN_INT_LITERAL:   return parse_int_literal(parser);
        case TOKEN_L_PAREN:       return parse_tuple_pattern(parser);
        case TOKEN_L_BRACKET:     return parse_array_pattern(parser);
        case TOKEN_IDENT: {
            // Accept types as patterns for function parameters,
            // so as to allow function prototypes without parameter names.
            if (is_fun_param &&
                parser->ahead[1].tag != TOKEN_COLON &&
                parser->ahead[1].tag != TOKEN_DOT &&
                parser->ahead[1].tag != TOKEN_L_BRACE &&
                parser->ahead[1].tag != TOKEN_L_BRACKET)
                return parse_anonymous_pattern(parser, parse_type(parser));
            AstNode* path = parse_path(parser);
            if (parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct(parser, AST_STRUCT_PATTERN, path, parse_field_pattern);
            if (parser->ahead->tag == TOKEN_L_PAREN)
                return parse_ctor_pattern(parser, path);
            // If the pattern is just an identifier, then this is an identifier pattern, not a path
            if (!path->path.elems->next && !path->path.elems->path_elem.type_args) {
                *path = (AstNode) {
                    .tag = AST_IDENT_PATTERN,
                    .file_loc = path->file_loc,
                    .ident_pattern.name = path->path.elems->path_elem.name
                };
            }
            return path;
        }
        case TOKEN_MINUS:
        case TOKEN_PLUS:
            // Accept `-` and `+` in front of integer literals
            if (parser->ahead[1].tag == TOKEN_INT_LITERAL) {
                bool has_minus = parser->ahead->tag == TOKEN_MINUS;
                skip_token(parser);
                AstNode* literal = parse_int_literal(parser);
                literal->int_literal.has_minus = has_minus;
                return literal;
            }
            break;
#define f(name, ...) case TOKEN_##name:
        PRIM_TYPE_LIST(f)
#undef f
        case TOKEN_BANG:
        case TOKEN_AMP:
            if (is_fun_param)
                return parse_anonymous_pattern(parser, parse_type(parser));
            // fallthrough
        default:
            break;
    }
    return parse_error(parser, "pattern");
}

static inline AstNode* parse_typed_pattern(Parser* parser, AstNode* pattern) {
    if (accept_token(parser, TOKEN_COLON)) {
        AstNode* type = parse_type(parser);
        return make_ast_node(parser, &pattern->file_loc.begin, &(AstNode) {
            .tag = AST_TYPED_PATTERN,
            .typed_pattern = { .left = pattern, .type = type }
        });
    }
    return pattern;
}

AstNode* parse_pattern(Parser* parser) {
    return parse_typed_pattern(parser, parse_untyped_pattern(parser, false));
}

static inline AstNode* parse_for_loop(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FOR);
    AstNode* pattern = parse_pattern(parser);
    expect_token(parser, TOKEN_IN);
    AstNode* range = parse_expr(parser);
    AstNode* body = parse_block_expr_or_error(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FOR_LOOP,
        .for_loop = { .pattern = pattern, .range = range, .body = body }
    });
}

static inline AstNode* parse_while_loop(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_WHILE);
    AstNode* cond = parse_expr_without_structs(parser);
    AstNode* body = parse_block_expr_or_error(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_WHILE_LOOP,
        .while_loop = { .cond = cond, .body = body }
    });
}

static inline AstNode* parse_type_params(Parser* parser) {
    if (accept_token(parser, TOKEN_L_BRACKET)) {
        AstNode* type_params = parse_many_at_least_one(
            parser, "type parameter lists", TOKEN_R_BRACKET, TOKEN_COMMA, parse_type_param);
        expect_token(parser, TOKEN_R_BRACKET);
        return type_params;
    }
    return NULL;
}

static inline AstNode* parse_fun_param(Parser* parser) {
    return parse_typed_pattern(parser, parse_untyped_pattern(parser, true));
}

static inline AstNode* parse_fun_params(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_PATTERN, parse_fun_param);
}

static inline AstNode* parse_fun_decl(Parser* parser, bool is_public) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FUN);

    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    AstNode* param = parse_tuple_or_error(parser, "function parameters", parse_fun_params);

    AstNode* ret_type = NULL;
    if (accept_token(parser, TOKEN_THIN_ARROW))
        ret_type = parse_type(parser);

    AstNode* used_sigs = NULL;
    if (accept_token(parser, TOKEN_USING))
        used_sigs = parse_many(parser, TOKEN_ERROR, TOKEN_COMMA, parse_type);

    AstNode* body = NULL;
    if (accept_token(parser, TOKEN_EQUAL)) {
        body = parse_expr(parser);
        expect_token(parser, TOKEN_SEMICOLON);
    } else if (parser->ahead->tag == TOKEN_L_BRACE)
        body = parse_block_expr(parser);
    else
        accept_token(parser, TOKEN_SEMICOLON);

    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FUN_DECL,
        .fun_decl = {
            .name = name,
            .is_public = is_public,
            .param = param,
            .type_params = type_params,
            .ret_type = ret_type,
            .used_sigs = used_sigs,
            .body = body
        }
    });
}

static AstNode* parse_field_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    expect_token(parser, TOKEN_COLON);
    AstNode* type = parse_type(parser);
    AstNode* val = NULL;
    if (accept_token(parser, TOKEN_EQUAL))
        val = parse_expr(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FIELD_DECL,
        .field_decl = { .name = name, .type = type, .val = val }
    });
}

static AstNode* parse_option_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    bool is_struct_like = false;
    AstNode* param_type = NULL;
    if (parser->ahead->tag == TOKEN_L_PAREN) {
        param_type = parse_tuple_type(parser);
        if (param_type->tag == AST_TUPLE_TYPE && !param_type->tuple_type.args)
            report_empty(parser, "option parameter lists", &param_type->file_loc);
    } else if (accept_token(parser, TOKEN_L_BRACE)) {
        is_struct_like = true;
        param_type = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field_decl);
        expect_token(parser, TOKEN_R_BRACE);
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_OPTION_DECL,
        .option_decl = {
            .name = name,
            .is_struct_like = is_struct_like,
            .param_type = param_type
        }
    });
}

static inline AstNode* parse_struct_decl(Parser* parser, bool is_public, bool is_opaque) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_STRUCT);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);

    AstNode* super_type = NULL;
    if (accept_token(parser, TOKEN_COLON))
        super_type = parse_type(parser);

    AstNode* fields = NULL;
    bool is_tuple_like = false;
    if (accept_token(parser, TOKEN_L_BRACE)) {
        fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field_decl);
        expect_token(parser, TOKEN_R_BRACE);
    } else {
        is_tuple_like = true;
        if (!super_type && accept_token(parser, TOKEN_L_PAREN)) {
            fields = parse_many_at_least_one(parser, "structure fields", TOKEN_R_PAREN, TOKEN_COMMA, parse_type);
            expect_token(parser, TOKEN_R_PAREN);
        }
        expect_token(parser, TOKEN_SEMICOLON);
    }

    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_STRUCT_DECL,
        .struct_decl = {
            .name = name,
            .super_type = super_type,
            .is_public = is_public,
            .is_opaque = is_opaque,
            .is_tuple_like = is_tuple_like,
            .type_params = type_params,
            .fields = fields
        }
    });
}

static inline AstNode* parse_enum_decl(Parser* parser, bool is_public, bool is_opaque) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_ENUM);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);

    AstNode* sub_type = NULL;
    if (accept_token(parser, TOKEN_COLON))
        sub_type = parse_type(parser);

    expect_token(parser, TOKEN_L_BRACE);
    AstNode* options = parse_many_at_least_one(
        parser, "enumerations", TOKEN_R_BRACE, TOKEN_COMMA, parse_option_decl);
    expect_token(parser, TOKEN_R_BRACE);

    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_ENUM_DECL,
        .enum_decl = {
            .name = name,
            .sub_type = sub_type,
            .is_public = is_public,
            .is_opaque = is_opaque,
            .type_params = type_params,
            .options = options
        }
    });
}

static AstNode* parse_sig_member_decl(Parser* parser) {
    return parse_decl_without_attr_list(parser, false, false, true);
}

static inline AstNode* parse_sig_decl(Parser* parser, bool needs_name) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_SIG);
    const char* name = needs_name || parser->ahead->tag == TOKEN_IDENT ? parse_ident(parser) : NULL;
    AstNode* type_params = parse_type_params(parser);
    AstNode* members = NULL;
    if (accept_token(parser, TOKEN_L_BRACE)) {
        members = parse_many_at_least_one(
            parser, "signatures", TOKEN_R_BRACE, TOKEN_ERROR, parse_sig_member_decl);
        expect_token(parser, TOKEN_R_BRACE);
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_SIG_DECL,
        .sig_decl = {
            .name = name,
            .type_params = type_params,
            .members = members,
        }
    });
}

static inline AstNode* parse_mod_decl(Parser* parser, bool is_public) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_MOD);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    AstNode* signature = NULL;
    if (accept_token(parser, TOKEN_COLON))
        signature = parse_type(parser);
    AstNode* aliased_mod = NULL;
    AstNode* members = NULL;
    if (accept_token(parser, TOKEN_L_BRACE)) {
        members = parse_many_at_least_one(parser, "modules", TOKEN_R_BRACE, TOKEN_ERROR, parse_decl);
        expect_token(parser, TOKEN_R_BRACE);
    } else {
        if (accept_token(parser, TOKEN_EQUAL))
            aliased_mod = parse_type(parser);
        expect_token(parser, TOKEN_SEMICOLON);
    }
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_MOD_DECL,
        .mod_decl = {
            .name = name,
            .is_public = is_public,
            .type_params = type_params,
            .members = members,
            .aliased_mod = aliased_mod,
            .signature = signature
        }
    });
}

static inline AstNode* parse_type_decl(
    Parser* parser,
    bool is_public,
    bool is_opaque,
    bool allow_unbound_types)
{
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_TYPE);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    AstNode* aliased_type = NULL;
    if (!allow_unbound_types)
        expect_token(parser, TOKEN_EQUAL);
    if (!allow_unbound_types || accept_token(parser, TOKEN_EQUAL))
        aliased_type = parse_type(parser);
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_TYPE_DECL,
        .type_decl = {
            .name = name,
            .is_public = is_public,
            .is_opaque = is_opaque,
            .type_params = type_params,
            .aliased_type = aliased_type
        }
    });
}

static inline AstNode* parse_const_or_var_decl(
    Parser* parser,
    TokenTag token_tag,
    AstNodeTag ast_node_tag,
    bool is_public)
{
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, token_tag);
    AstNode* pattern = parse_pattern(parser);
    AstNode* init = NULL;
    if (token_tag == TOKEN_CONST)
        expect_token(parser, TOKEN_EQUAL);
    if (token_tag == TOKEN_CONST || accept_token(parser, TOKEN_EQUAL))
        init = parse_expr(parser);
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = ast_node_tag,
        .const_decl = {
            .is_public = is_public,
            .pattern = pattern,
            .init = init
        }
    });
}

static inline AstNode* parse_const_decl(Parser* parser, bool is_public) {
    return parse_const_or_var_decl(parser, TOKEN_CONST, AST_CONST_DECL, is_public);
}

static inline AstNode* parse_var_decl(Parser* parser, bool is_public) {
    return parse_const_or_var_decl(parser, TOKEN_VAR, AST_VAR_DECL, is_public);
}

static inline AstNode* parse_using_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_USING);
    AstNode* type_params = parse_type_params(parser);
    AstNode* used_mod = parse_type(parser);
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_USING_DECL,
        .using_decl = { .type_params = type_params, .used_mod = used_mod }
    });
}

AstNode* parse_stmt(Parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_FOR:    return parse_for_loop(parser);
        case TOKEN_WHILE:  return parse_while_loop(parser);
        case TOKEN_TYPE:   return parse_type_decl(parser, false, false, false);
        case TOKEN_STRUCT: return parse_struct_decl(parser, false, false);
        case TOKEN_USING:  return parse_using_decl(parser);
        case TOKEN_ENUM:   return parse_enum_decl(parser, false, false);
        case TOKEN_VAR:    return parse_var_decl(parser, false);
        case TOKEN_CONST:  return parse_const_decl(parser, false);
        case TOKEN_FUN:
            // This test here prevents an ambiguity with anonymous function expressions.
            // Those also start with `fun`, just like function declarations,
            // but do not have an identifier after that.
            if (parser->ahead[1].tag == TOKEN_IDENT)
                return parse_fun_decl(parser, false);
            // fallthrough
        default:
            return parse_expr(parser);
    }
}

static inline AstNode* parse_decl_without_attr_list(
    Parser* parser,
    bool is_public,
    bool is_opaque,
    bool allow_unbound_types)
{
    switch (parser->ahead->tag) {
        case TOKEN_STRUCT: return parse_struct_decl(parser, is_public, is_opaque);
        case TOKEN_ENUM:   return parse_enum_decl(parser, is_public, is_opaque);
        case TOKEN_MOD:    return parse_mod_decl(parser, is_public);
        case TOKEN_SIG:    return parse_sig_decl(parser, true);
        case TOKEN_USING:  return parse_using_decl(parser);
        case TOKEN_TYPE:   return parse_type_decl(parser, is_public, is_opaque, allow_unbound_types);
        case TOKEN_CONST:  return parse_const_decl(parser, is_public);
        case TOKEN_VAR:    return parse_var_decl(parser, is_public);
        case TOKEN_FUN:    return parse_fun_decl(parser, is_public);
        default:
            return parse_error(parser, "declaration");
    }
}

AstNode* parse_decl(Parser* parser) {
    AstNode* attrs = NULL;
    if (parser->ahead->tag == TOKEN_HASH)
        attrs = parse_attr_list(parser);
    bool is_public = accept_token(parser, TOKEN_PUB);
    FileLoc opaque_loc = parser->ahead->file_loc;
    bool is_opaque = is_public && accept_token(parser, TOKEN_OPAQUE);
    AstNode* decl = parse_decl_without_attr_list(parser, is_public, is_opaque, false);
    if (is_opaque && is_value_decl(decl->tag)) {
        log_error(parser->lexer->log, &opaque_loc, "cannot use '{$}opaque{$}' here",
            (FormatArg[]) { { .style = keyword_style }, { .style = reset_style } });
    }
    decl->attrs = attrs;
    return decl;
}

AstNode* parse_program(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* members = parse_many(parser, TOKEN_EOF, TOKEN_ERROR, parse_decl);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_MOD_DECL,
        .mod_decl = { .members = members }
    });
}
