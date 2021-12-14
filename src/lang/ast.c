#include "lang/ast.h"

bool is_trivial_pattern(const struct ast* ast) {
    switch (ast->tag) {
        case AST_IDENT:
            return true;
        case AST_TYPE_ANNOT:
            return is_trivial_pattern(ast->type_annot.left);
        case AST_TUPLE_PATTERN:
            for (struct ast* arg = ast->tuple_pattern.args; arg; arg = arg->next) {
                if (!is_trivial_pattern(arg))
                    return false;
            }
            return true;
        case AST_CALL_PATTERN:
            return
                ast->call_pattern.callee->type->tag == TYPE_STRUCT &&
                is_trivial_pattern(ast->call_pattern.arg);
        default:
            return false;
    }
}

size_t get_ast_list_length(const struct ast* ast) {
    size_t len = 0;
    while (ast) ast = ast->next, len++;
    return len;
}

struct ast* get_ast_list_elem(struct ast* ast, size_t i) {
    while (i > 0) ast = ast->next, i--;
    return ast;
}
