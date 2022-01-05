#include "fu/lang/check.h"

TypingContext make_typing_context(TypeTable* type_table, Log* log) {
    return (TypingContext) {
        .log = log,
        .type_table = type_table
    };
}

void check_program(TypingContext* context, AstNode* program) {
    // TODO
}
