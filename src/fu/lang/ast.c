#include "fu/lang/ast.h"
#include "fu/core/utils.h"

#include <limits.h>

static inline void print_many(FormatState* state, const char* sep, const AstNode* elems) {
    for (; elems; elems = elems->next) {
        print_ast(state, elems);
        if (elems->next)
            format(state, sep, NULL);
    }
}

static inline void print_many_with_delim(
    FormatState* state,
    const char* open,
    const char* sep,
    const char* close,
    const AstNode* elems)
{
    format(state, open, NULL);
    print_many(state, sep, elems);
    format(state, close, NULL);
}

static inline void print_with_delim(
    FormatState* state,
    const char* open,
    const char* close,
    const AstNode* elem)
{
    assert(!elem->next);
    print_many_with_delim(state, open, "", close, elem);
}

static inline void print_many_inside_block(FormatState* state, const char* sep, const AstNode* elems) {
    if (!elems->next)
        print_with_delim(state, "{{ ", " }", elems);
    else
        print_many_with_delim(state, "{{{>}\n", sep, "{<}\n}", elems);
}

static inline void print_with_style(FormatState* state, const char* str, FormatStyle style) {
    format(state, "{$}{s}{$}", (FormatArg[]) { { .style = style }, { .s = str }, { .style = reset_style } });
}

static void print_keyword(FormatState* state, const char* keyword) {
    print_with_style(state, keyword, keyword_style);
}

static inline void print_as_tuple(FormatState* state, const AstNode* ast_node) {
    if (is_tuple(ast_node->tag))
        print_ast(state, ast_node);
    else
        print_with_delim(state, "(", ")", ast_node);
}

static inline void print_prim_type(FormatState* state, AstNodeTag tag) {
    print_keyword(state, ast_node_tag_to_prim_type_name(tag));
}

static inline void print_operand(FormatState* state, const AstNode* ast_node, int prec) {
    if (is_binary_expr(ast_node->tag) && precedence(ast_node->tag) > prec)
        print_with_delim(state, "(", ")", ast_node);
    else
        print_ast(state, ast_node);
}

static inline void print_binary_or_assign_expr(FormatState* state, const AstNode* ast_node, const char* op) {
    int prec = precedence(ast_node->tag);
    print_operand(state, ast_node->binary_expr.left, prec);
    format(state, " {s} ", (FormatArg[]) { { .s = op } });
    print_operand(state, ast_node->binary_expr.right, prec);
}

static inline void print_binary_expr(FormatState* state, const AstNode* ast_node) {
    print_binary_or_assign_expr(state, ast_node, ast_node_tag_to_binary_expr_op(ast_node->tag));
}

static inline void print_assign_expr(FormatState* state, const AstNode* ast_node) {
    print_binary_or_assign_expr(state, ast_node, ast_node_tag_to_assign_expr_op(ast_node->tag));
}

static inline void print_decl_head(FormatState* state, const char* keyword, const char* name, const AstNode* type_params) {
    print_keyword(state, keyword);
    format(state, " {s}", (FormatArg[]) { { .s = name } });
    if (type_params)
        print_many_with_delim(state, "[", ", ", "]", type_params);
}

void print_ast(FormatState* state, const AstNode* ast_node) {
    switch (ast_node->tag) {
        case AST_BOOL_LITERAL:
            print_keyword(state, ast_node->bool_literal.val ? "true" : "false");
            break;
        case AST_CHAR_LITERAL:
            format(state, "{$}{u8}{$}", (FormatArg[]) {
                { .style = literal_style },
                { .u8 = ast_node->char_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_STR_LITERAL:
            format(state, "{$}\"{s}\"{$}", (FormatArg[]) {
                { .style = literal_style },
                { .s = ast_node->str_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_INT_LITERAL:
            format(state, "{$}{um}{$}", (FormatArg[]) {
                { .style = literal_style },
                { .um = ast_node->int_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_FLOAT_LITERAL:
            format(state, "{f64}", (FormatArg[]) {
                { .style = literal_style },
                { .f64 = ast_node->float_literal.val },
                { .style = reset_style }
            });
            break;
#define f(name, ...) case AST_TYPE_##name:
        AST_PRIM_TYPE_LIST(f)
#undef f
            print_prim_type(state, ast_node->tag);
            break;
        case AST_PATH:
            print_many(state, ".", ast_node->path.elems);
            break;
        case AST_PATH_ELEM:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->path_elem.name } });
            if (ast_node->path_elem.type_args)
                print_many_with_delim(state, "[", ", ", "]", ast_node->path_elem.type_args);
            break;
        case AST_PROGRAM:
            print_many(state, "\n", ast_node->program.decls);
            break;
        case AST_ERROR:
            print_with_style(state, "<error>", error_style);
            break;
        case AST_NAME:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->name.ident } });
            break;
#define f(name, ...) case AST_##name##_EXPR:
        AST_BINARY_EXPR_LIST(f)
#undef f
            print_binary_expr(state, ast_node);
            break;
#define f(name, ...) case AST_##name##_ASSIGN_EXPR:
        AST_ASSIGN_EXPR_LIST(f)
#undef f
            print_assign_expr(state, ast_node);
            break;
        case AST_TYPE_DECL:
            print_decl_head(state, "type", ast_node->type_decl.name, ast_node->type_decl.type_params);
            print_with_delim(state, " = ", ";", ast_node->type_decl.aliased_type);
            break;
        case AST_FIELD_DECL:
            print_many_with_delim(state, "", ", ", " : ", ast_node->field_decl.names);
            print_ast(state, ast_node->field_decl.type);
            break;
        case AST_STRUCT_DECL:
            print_decl_head(state, "struct", ast_node->struct_decl.name, ast_node->struct_decl.type_params);
            format(state, " ", NULL);
            print_many_inside_block(state, ",\n", ast_node->struct_decl.fields);
            break;
        case AST_OPTION_DECL:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->option_decl.name } });
            if (ast_node->option_decl.param_type)
                print_as_tuple(state, ast_node->option_decl.param_type);
            break;
        case AST_ENUM_DECL:
            print_decl_head(state, "enum", ast_node->enum_decl.name, ast_node->enum_decl.type_params);
            format(state, " ", NULL);
            print_many_inside_block(state, ",\n", ast_node->enum_decl.options);
            break;
        case AST_FUN_DECL:
            print_decl_head(state, "fun", ast_node->fun_decl.name, ast_node->fun_decl.type_params);
            print_as_tuple(state, ast_node->fun_decl.param);
            if (ast_node->fun_decl.ret_type)
                print_with_delim(state, "->", "", ast_node->fun_decl.ret_type);
            format(state, " ", NULL);
            if (ast_node->fun_decl.body->tag != AST_BLOCK_EXPR)
                print_with_delim(state, "= ", ";", ast_node->fun_decl.body);
            else
                print_ast(state, ast_node->fun_decl.body);
            break;
        case AST_CONST_DECL:
        case AST_VAR_DECL:
            print_keyword(state, ast_node->tag == AST_CONST_DECL ? "const" : "var");
            print_with_delim(state, " ", "", ast_node->const_decl.pattern);
            if (ast_node->const_decl.init)
                print_with_delim(state, " = ", "", ast_node->const_decl.init);
            format(state, ";", NULL);
            break;
        case AST_FIELD_PATTERN:
        case AST_FIELD_EXPR:
            print_many_with_delim(state, "", ", ", " = ", ast_node->field_pattern.names);
            print_ast(state, ast_node->field_pattern.val);
            break;
        case AST_STRUCT_PATTERN:
        case AST_STRUCT_EXPR:
            print_with_delim(state, "", " ", ast_node->struct_pattern.path);
            print_many_inside_block(state, ",\n", ast_node->struct_pattern.fields);
            break;
        case AST_TUPLE_TYPE:
        case AST_TUPLE_PATTERN:
        case AST_TUPLE_EXPR:
            print_many_with_delim(state, "(", ", ", ")", ast_node->tuple_type.args);
            break;
        case AST_ARRAY_TYPE:
            print_with_delim(state, "[", "]", ast_node->array_type.elem_type);
            break;
        case AST_ARRAY_PATTERN:
        case AST_ARRAY_EXPR:
            print_many_with_delim(state, "[", ", ", "]", ast_node->array_expr.elems);
            break;
        case AST_TYPED_PATTERN:
        case AST_TYPED_EXPR:
            print_ast(state, ast_node->typed_pattern.left);
            format(state, " : ", NULL);
            print_ast(state, ast_node->typed_pattern.type);
            break;
        case AST_BLOCK_EXPR:
            if (!ast_node->block_expr.stmts)
                format(state, "{{}", NULL);
            else {
                format(state, "{{{>}\n", NULL);
                for (AstNode* stmt = ast_node->block_expr.stmts; stmt; stmt = stmt->next) {
                    print_ast(state, stmt);
                    if (stmt->next)
                        format(state, needs_semicolon(stmt->tag) ? ";\n" : "\n", NULL);
                }
                format(state, "{<}\n}", NULL);
            }
            break;
        case AST_IF_EXPR:
            print_keyword(state, "if");
            print_with_delim(state, " ", " ", ast_node->if_expr.cond);
            print_ast(state, ast_node->if_expr.if_true);
            if (ast_node->if_expr.if_false) {
                format(state, " ", NULL);
                print_keyword(state, "else");
                print_with_delim(state, " ", "", ast_node->if_expr.if_false);
            }
            break;
        case AST_MATCH_CASE:
            print_with_delim(state, "", " => ", ast_node->match_case.pattern);
            print_ast(state, ast_node->match_case.val);
            break;
        case AST_MATCH_EXPR:
            print_keyword(state, "match");
            print_with_delim(state, " ", " ", ast_node->match_expr.arg);
            print_many_inside_block(state, ",\n", ast_node->match_expr.cases);
            break;
        case AST_CALL_EXPR:
            print_ast(state, ast_node->call_expr.callee);
            print_as_tuple(state, ast_node->call_expr.arg);
            break;
        case AST_CTOR_PATTERN:
            print_ast(state, ast_node->ctor_pattern.path);
            print_as_tuple(state, ast_node->ctor_pattern.arg);
            break;
        case AST_FOR_LOOP:
            print_keyword(state, "for");
            print_with_delim(state, " ", " ", ast_node->for_loop.pattern);
            print_keyword(state, "in");
            print_with_delim(state, " ", " ", ast_node->for_loop.range);
            print_ast(state, ast_node->for_loop.body);
            break;
        case AST_WHILE_LOOP:
            print_keyword(state, "while");
            print_with_delim(state, " ", " ", ast_node->while_loop.cond);
            print_ast(state, ast_node->while_loop.body);
            break;
        case AST_BREAK_EXPR:    print_keyword(state, "break"); break;
        case AST_CONTINUE_EXPR: print_keyword(state, "continue"); break;
        case AST_RETURN_EXPR:   print_keyword(state, "return"); break;
        default:
            assert(false && "invalid node tag");
            break;
    }
}

void dump_ast(const AstNode* ast_node) {
    FormatState state = { .tab = "    ", .ignore_style = !is_color_supported(stdout) };
    print_ast(&state, ast_node);
    print_format_bufs(state.first_buf, stdout);
    free_format_bufs(state.first_buf);
    printf("\n");
}

bool needs_semicolon(AstNodeTag tag) {
    switch (tag) {
        case AST_CONST_DECL:
        case AST_VAR_DECL:
        case AST_FUN_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
        case AST_TYPE_DECL:
        case AST_IF_EXPR:
        case AST_MATCH_EXPR:
        case AST_BLOCK_EXPR:
        case AST_WHILE_LOOP:
        case AST_FOR_LOOP:
            return false;
        default:
            return true;
    }
}

bool is_tuple(AstNodeTag tag) {
    return
        tag == AST_TUPLE_TYPE ||
        tag == AST_TUPLE_PATTERN ||
        tag == AST_TUPLE_EXPR;
}

bool is_binary_expr(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, ...) case AST_##name##_EXPR:
    AST_BINARY_EXPR_LIST(f)
#undef f
            return true;
        default:
            return false;
    }
}

const char* ast_node_tag_to_prim_type_name(AstNodeTag tag) {
    switch (tag) {
#define f(name, str) case AST_TYPE_##name: return str;
        AST_PRIM_TYPE_LIST(f)
#undef f
        default:
            assert(false && "invalid primitive type");
            return "";
    }
}

const char* ast_node_tag_to_binary_expr_op(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, tok, str) case AST_##name##_EXPR: return str;
        AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid binary expression");
            return "";
    }
}

const char* ast_node_tag_to_assign_expr_op(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, tok, str) case AST_##name##_ASSIGN_EXPR: return str"=";
        AST_ASSIGN_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid assignment expression");
            return "";
    }
}

int max_precedence() {
    int max = 0;
#define f(name, prec, ...) max = max < prec ? prec : max;
    AST_BINARY_EXPR_LIST(f)
#undef f
    return max;
}

int precedence(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, ...) case AST_##name##_EXPR: return prec;
    AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            return INT_MAX;
    }
}
