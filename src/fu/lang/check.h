#ifndef FU_LANG_CHECK_H
#define FU_LANG_CHECK_H

#include "fu/lang/types.h"

/*
 * The type-checker is an implementation of a bidirectional type-checking algorithm.
 * It is therefore local in nature, only looking at "neighboring" nodes to make typing judgments.
 */

typedef struct TypingContext {
    Log* log;
    TypeTable* type_table;
} TypingContext;

TypingContext make_typing_context(TypeTable*, Log*);

void check_stmt(TypingContext*, AstNode*, const Type*);
void check_decl(TypingContext*, AstNode*, const Type*);
void check_pattern(TypingContext*, AstNode*, const Type*);
void check_expr(TypingContext*, AstNode*, const Type*);
void check_type(TypingContext*, AstNode*, const Type*);
void check_program(TypingContext*, AstNode*);

#endif
