#include "fu/lang/ast.h"
#include "fu/core/utils.h"

void print_ast(FormatState* state, const AstNode* ast_node) {
    switch (ast_node->tag) {
        default:
            assert(false && "invalid node tag");
            break;
    }
}

void dump_ast(const AstNode* ast_node) {
    FormatState state = { .tab = "    ", .ignore_style = is_color_supported(stdout) };
    print_ast(&state, ast_node);
    print_format_bufs(state.first_buf, stdout);
    free_format_bufs(state.first_buf);
}
