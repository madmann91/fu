#include "fu/lang/ast.h"
#include "fu/core/utils.h"

static inline void print_many(FormatState* state, const char* sep, AstNode* elems) {
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
    AstNode* elems)
{
    if (elems) {
        format(state, open, NULL);
        print_many(state, sep, elems);
        format(state, close, NULL);
    }
}

void print_ast(FormatState* state, const AstNode* ast_node) {
    switch (ast_node->tag) {
        case AST_PATH:
            print_many(state, ".", ast_node->path.elems);
            break;
        case AST_PATH_ELEM:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->path_elem.name } });
            print_many_with_delim(state, "[", ",", "]", ast_node->path_elem.type_args);
            break;
        case AST_PROGRAM:
            print_many(state, "\n", ast_node->program.decls);
            break;
        case AST_ERROR:
            format(state, "{$}<error>{$}",
                (FormatArg[]) { { .style = error_style }, { .style = reset_style } });
            break;
        case AST_FUN_DECL:
            format(state, "{$}fun{$} {s}", (FormatArg[]) {
                { .style = keyword_style },
                { .style = reset_style },
                { .s = ast_node->fun_decl.name }
            });
            print_many_with_delim(state, "[", ",", "]", ast_node->fun_decl.type_params);
            print_ast(state, ast_node->fun_decl.param);
            print_many_with_delim(state, "->", "", "", ast_node->fun_decl.ret_type);
            if (ast_node->fun_decl.body->tag != AST_BLOCK_EXPR)
                format(state, " = ", NULL);
            print_ast(state, ast_node->fun_decl.body);
            break;
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

bool needs_semicolon(const AstNode* ast_node) {
    switch (ast_node->tag) {
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
            assert(false && "invalid binary expression tag");
            return max_precedence();
    }
}
