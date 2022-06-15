#ifndef FU_LANG_CHECK_H
#define FU_LANG_CHECK_H

#include "fu/lang/ast.h"
#include "fu/lang/types.h"
#include "fu/core/hash_table.h"

typedef struct MemPool MemPool;

/*
 * The type-checker is an implementation of a bidirectional type-checking algorithm.
 * It is therefore local in nature, only looking at "neighboring" nodes to make typing judgments.
 */

typedef struct TypingContext {
    Log* log;
    TypeTable* type_table;
    MemPool* mem_pool;
    HashTable visited_decls;
} TypingContext;

TypingContext new_typing_context(TypeTable*, MemPool* mem_pool, Log*);
void free_typing_context(TypingContext*);

const Type* infer_kind(TypingContext*, AstNode*);
const Type* infer_type(TypingContext*, AstNode*);
const Type* infer_pattern(TypingContext*, AstNode*);
const Type* check_pattern(TypingContext*, AstNode*, const Type*);
const Type* infer_expr(TypingContext*, AstNode*);
const Type* check_expr(TypingContext*, AstNode*, const Type*);
const Type* infer_stmt(TypingContext*, AstNode*);
const Type* check_stmt(TypingContext*, AstNode*, const Type*);
const Type* infer_decl(TypingContext*, AstNode*);
void infer_program(TypingContext*, AstNode*);

#endif
