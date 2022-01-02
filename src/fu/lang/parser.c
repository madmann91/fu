#include "fu/lang/parser.h"
#include "fu/core/alloc.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

typedef struct {
    AstNode* first;
    AstNode* last;
} AstList;

static inline void add_ast_node_to_list(AstList* list, AstNode* node) {
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

static inline FileLoc make_file_loc(Parser* parser, FilePos* begin) {
    return (FileLoc) {
        .file_name = parser->lexer->file_name,
        .begin = *begin,
        .end = parser->prev_end
    };
}

static inline AstNode* make_ast_node(Parser* parser, FilePos* begin, const AstNode* node) {
    AstNode* copy = alloc_from_mem_pool(parser->mem_pool, sizeof(AstNode));
    memcpy(copy, node, sizeof(AstNode));
    copy->file_loc = make_file_loc(parser, begin);
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

static void expect_fail(Parser* parser, const char* expected_msg, const char* found_msg, const FileLoc* loc) {
    log_error(parser->lexer->log, loc, "expected {s}, but got {s}", (FormatArg[]) { { .s = expected_msg }, { .s = found_msg } });
}

static inline bool expect_token(Parser* parser, TokenTag tag) {
    if (!accept_token(parser, tag)) {
        expect_fail(parser, token_tag_to_str(tag), token_tag_to_str(parser->ahead->tag), &parser->ahead->file_loc);
        return false;
    }
    return true;
}

static AstNode* parse_many(Parser* parser, TokenTag end, TokenTag sep, AstNode* (*parse_one)(Parser*)) {
    AstList list = { NULL };
    while (true) {
        if (end != TOKEN_ERROR && parser->ahead->tag == end)
            break;
        add_ast_node_to_list(&list, parse_one(parser));
        if (sep != TOKEN_ERROR && !accept_token(parser, sep))
            break;
    }
    return list.first;
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
    expect_fail(parser, msg, token_tag_to_str(parser->ahead->tag), &parser->ahead->file_loc);
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

static inline AstNode* parse_tuple_type(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_TYPE, parse_type);
}

static inline AstNode* parse_tuple_pattern(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_PATTERN, parse_pattern);
}

static inline AstNode* parse_tuple_expr(Parser* parser) {
    return parse_tuple(parser, AST_TUPLE_EXPR, parse_expr);
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

static inline AstNode* parse_path(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* elems = parse_many(parser, TOKEN_ERROR, TOKEN_DOT, parse_path_elem);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_PATH, .path.elems = elems });
}

static inline AstNode* parse_block_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_L_BRACE);
    AstList stmt_list = { NULL };
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

static inline AstNode* parse_param(Parser* parser) {
    return parser->ahead->tag == TOKEN_L_PAREN
        ? parse_tuple_pattern(parser) : parse_error(parser, "function parameter");
}

static AstNode* parse_type_param(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    // TODO: Kind
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_TYPE_PARAM,
        .type_param = { .name = name }
    });
}

static inline AstNode* parse_type_params(Parser* parser) {
    if (accept_token(parser, TOKEN_L_BRACKET)) {
        AstNode* type_params = parse_many(parser, TOKEN_R_BRACKET, TOKEN_COMMA, parse_type_param);
        expect_token(parser, TOKEN_R_BRACKET);
        return type_params;
    }
    return NULL;
}

static inline AstNode* parse_fun_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FUN);
    const char* name = parse_ident(parser);
    AstNode* param = parse_param(parser);
    AstNode* type_params = parse_type_params(parser);
    AstNode* ret_type = NULL;
    if (accept_token(parser, TOKEN_THIN_ARROW))
        ret_type = parse_type(parser);
    AstNode* body = NULL;
    if (accept_token(parser, TOKEN_EQUAL)) {
        body = parse_expr(parser);
        expect_token(parser, TOKEN_SEMICOLON);
    } else if (parser->ahead->tag == TOKEN_L_BRACE)
        body = parse_block_expr(parser);
    else
        body = parse_error(parser, "function body");
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FUN_DECL,
        .fun_decl = {
            .name = name,
            .param = param,
            .type_params = type_params,
            .ret_type = ret_type,
            .body = body
        }
    });
}

static AstNode* parse_name(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* ident = parse_ident(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_NAME, .name.ident = ident });
}

static AstNode* parse_names(Parser* parser, TokenTag stop) {
    AstNode* names = parse_many(parser, stop, TOKEN_COMMA, parse_name);
    if (!names)
        expect_fail(parser, "name", token_tag_to_str(parser->ahead->tag), &parser->ahead->file_loc);
    return names;
}

static AstNode* parse_field_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* names = parse_names(parser, TOKEN_COLON);
    expect_token(parser, TOKEN_COLON);
    AstNode* type = parse_type(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_FIELD_DECL,
        .field_decl = { .names = names, .type = type }
    });
}

static inline AstNode* parse_struct_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_STRUCT);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    expect_token(parser, TOKEN_L_BRACE);
    AstNode* fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field_decl);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_STRUCT_DECL,
        .struct_decl = { .name = name, .type_params = type_params, .fields = fields }
    });
}

static AstNode* parse_option_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    const char* name = parse_ident(parser);
    AstNode* param_type = NULL;
    if (parser->ahead->tag == TOKEN_L_PAREN)
        param_type = parse_tuple_type(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_OPTION_DECL,
        .option_decl = { .name = name, .param_type = param_type }
    });
}

static inline AstNode* parse_enum_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_ENUM);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    expect_token(parser, TOKEN_L_BRACE);
    AstNode* options = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_option_decl);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_ENUM_DECL,
        .enum_decl = { .name = name, .type_params = type_params, .options = options }
    });
}

static inline AstNode* parse_type_decl(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_TYPE);
    const char* name = parse_ident(parser);
    AstNode* type_params = parse_type_params(parser);
    expect_token(parser, TOKEN_EQUAL);
    AstNode* aliased_type = parse_type(parser);
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_TYPE_DECL,
        .type_decl = { .name = name, .type_params = type_params, .aliased_type = aliased_type }
    });
}

static inline AstNode* parse_const_or_var_decl(Parser* parser, AstNodeTag tag) {
    FilePos begin = parser->ahead->file_loc.begin;
    skip_token(parser);
    AstNode* pattern = parse_pattern(parser);
    AstNode* init = NULL;
    if (accept_token(parser, TOKEN_EQUAL))
        init = parse_expr(parser);
    expect_token(parser, TOKEN_SEMICOLON);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = tag,
        .const_decl = { .pattern = pattern, .init = init }
    });
}

static inline AstNode* parse_const_decl(Parser* parser) {
    return parse_const_or_var_decl(parser, AST_CONST_DECL);
}

static inline AstNode* parse_var_decl(Parser* parser) {
    return parse_const_or_var_decl(parser, AST_VAR_DECL);
}

AstNode* parse_decl(Parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_FUN:    return parse_fun_decl(parser);
        case TOKEN_STRUCT: return parse_struct_decl(parser);
        case TOKEN_ENUM:   return parse_enum_decl(parser);
        case TOKEN_TYPE:   return parse_type_decl(parser);
        case TOKEN_CONST:  return parse_const_decl(parser);
        case TOKEN_VAR:    return parse_var_decl(parser);
        default:
            return parse_error(parser, "declaration");
    }
}

static inline AstNode* parse_prim_type(Parser* parser, AstNodeTag tag) {
    FilePos begin = parser->ahead->file_loc.begin;
    skip_token(parser);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = tag });
}

AstNode* parse_type(Parser* parser) {
    switch (parser->ahead->tag) {
#define f(name, ...) case TOKEN_##name: return parse_prim_type(parser, AST_TYPE_##name);
        AST_PRIM_TYPE_LIST(f)
#undef f
        case TOKEN_IDENT:   return parse_path(parser);
        case TOKEN_L_PAREN: return parse_tuple_type(parser);
        case TOKEN_STRUCT:  return parse_struct_decl(parser);
        case TOKEN_ENUM:    return parse_enum_decl(parser);
        default:
            return parse_error(parser, "type");
    }
}

static AstNode* parse_struct(Parser* parser, AstNodeTag tag, AstNode* path, AstNode* (*parse_field)(Parser*)) {
    eat_token(parser, TOKEN_L_BRACE);
    AstNode* fields = parse_many(parser, TOKEN_R_BRACE, TOKEN_COMMA, parse_field);
    expect_token(parser, TOKEN_R_BRACE);
    return make_ast_node(parser, &path->file_loc.begin, &(AstNode) {
        .tag = tag,
        .struct_expr = { .path = path, .fields = fields }
    });
}

static AstNode* parse_field(Parser* parser, AstNodeTag tag, AstNode* (*parse_val)(Parser*)) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* names = parse_names(parser, TOKEN_EQUAL);
    expect_token(parser, TOKEN_EQUAL);
    AstNode* val = parse_val(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = tag,
        .field_expr = { .names = names, .val = val }
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

static inline AstNode* parse_postfix_expr(Parser* parser, AstNode* (*parse_primary_expr)(Parser*)) {
    AstNode* operand = parse_primary_expr(parser);
    while (true) {
        AstNodeTag tag = AST_ERROR;
        switch (parser->ahead->tag) {
            case TOKEN_DOUBLE_PLUS:  tag = AST_POST_INC_EXPR; break;
            case TOKEN_DOUBLE_MINUS: tag = AST_POST_DEC_EXPR; break;
            case TOKEN_L_PAREN:
                operand = parse_call_expr(parser, operand);
                continue;
            default:
                return operand;
        }
        FilePos begin = parser->ahead->file_loc.begin;
        skip_token(parser);
        operand = make_ast_node(parser, &begin, &(AstNode) { .tag = tag, .unary_expr = { .operand = operand } });
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
        int next_prec = precedence(tag);
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
    return parse_binary_expr(parser, left, parse_primary_expr, max_precedence());
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

static inline AstNode* parse_block_expr_or_error(Parser* parser) {
    return parser->ahead->tag == TOKEN_L_BRACE
        ? parse_block_expr(parser) : parse_error(parser, "block expression");
}

static inline AstNode* parse_if_expr(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_IF);
    AstNode* cond = parse_expr_without_structs(parser);
    AstNode* if_true = parse_block_expr_or_error(parser);
    AstNode* if_false = NULL;
    if (accept_token(parser, TOKEN_ELSE))
        if_false = parse_block_expr_or_error(parser);
    return make_ast_node(parser, &begin, &(AstNode) {
        .tag = AST_IF_EXPR,
        .if_expr = { .cond = cond, .if_true = if_true, .if_false = if_false }
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

static inline AstNode* parse_untyped_expr(Parser* parser, bool allow_structs) {
    switch (parser->ahead->tag) {
        case TOKEN_TRUE:  return parse_bool_literal(parser, true);
        case TOKEN_FALSE: return parse_bool_literal(parser, false);
        case TOKEN_STR_LITERAL:   return parse_str_literal(parser);
        case TOKEN_CHAR_LITERAL:  return parse_char_literal(parser);
        case TOKEN_INT_LITERAL:   return parse_int_literal(parser);
        case TOKEN_FLOAT_LITERAL: return parse_float_literal(parser);
        case TOKEN_IDENT: {
            AstNode* path = parse_path(parser);
            if (allow_structs && parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct(parser, AST_STRUCT_EXPR, path, parse_field_expr);
            return path;
        }
        case TOKEN_IF:
            return parse_if_expr(parser);
        case TOKEN_MATCH:
            return parse_match_expr(parser);
        case TOKEN_L_PAREN:
            return parse_tuple_expr(parser);
        case TOKEN_L_BRACE:
            return parse_block_expr(parser);
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

static AstNode* parse_untyped_pattern(Parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_TRUE:  return parse_bool_literal(parser, true);
        case TOKEN_FALSE: return parse_bool_literal(parser, false);
        case TOKEN_STR_LITERAL:   return parse_str_literal(parser);
        case TOKEN_CHAR_LITERAL:  return parse_char_literal(parser);
        case TOKEN_INT_LITERAL:   return parse_int_literal(parser);
        case TOKEN_FLOAT_LITERAL: return parse_float_literal(parser);
        case TOKEN_IDENT: {
            AstNode* path = parse_path(parser);
            if (parser->ahead->tag == TOKEN_L_BRACE)
                return parse_struct(parser, AST_STRUCT_PATTERN, path, parse_field_pattern);
            if (parser->ahead->tag == TOKEN_L_PAREN)
                return parse_ctor_pattern(parser, path);
            if (path->path.elems->next)
                expect_fail(parser, "pattern", "path", &path->file_loc);
            else if (path->path.elems->path_elem.type_args)
                expect_fail(parser, "pattern", "type application", &path->file_loc);
            else
                return path->path.elems;
            return path;
        }
        case TOKEN_L_PAREN:
            return parse_tuple_pattern(parser);
        default:
            return parse_error(parser, "pattern");
    }
}

AstNode* parse_pattern(Parser* parser) {
    AstNode* pattern = parse_untyped_pattern(parser);
    if (accept_token(parser, TOKEN_COLON)) {
        AstNode* type = parse_type(parser);
        return make_ast_node(parser, &pattern->file_loc.begin, &(AstNode) {
            .tag = AST_TYPED_PATTERN,
            .typed_pattern = { .left = pattern, .type = type }
        });
    }
    return pattern;
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

AstNode* parse_stmt(Parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_FOR:    return parse_for_loop(parser);
        case TOKEN_WHILE:  return parse_while_loop(parser);
        case TOKEN_TYPE:   return parse_type_decl(parser);
        case TOKEN_STRUCT: return parse_struct_decl(parser);
        case TOKEN_ENUM:   return parse_enum_decl(parser);
        case TOKEN_VAR:    return parse_var_decl(parser);
        case TOKEN_CONST:  return parse_const_decl(parser);
        case TOKEN_FUN:    return parse_fun_decl(parser);
        default:
            return parse_expr(parser);
    }
}

AstNode* parse_program(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* decls = parse_many(parser, TOKEN_EOF, TOKEN_ERROR, parse_decl);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_PROGRAM, .program.decls = decls });
}
