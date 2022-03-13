#ifndef FU_LANG_BIND_H
#define FU_LANG_BIND_H

/*
 * The name binding algorithm requires an environment to be able to register symbols and find
 * out the declaration sites of identifiers.
 * The environment is just a linked list of scopes, each containing a hash table that maps all
 * of the symbols declared in the scope to their actual AST node.
 */

typedef struct AstNode AstNode;
typedef struct Scope Scope;
typedef struct Log Log;

typedef struct {
    Log* log;
    Scope* first_scope;
    Scope* cur_scope;
} Env;

Env new_env(Log*);
void free_env(Env*);

void bind_stmt(Env*, AstNode*);
void bind_decl(Env*, AstNode*);
void bind_const_pattern(Env*, AstNode*);
void bind_non_const_pattern(Env*, AstNode*);
void bind_expr(Env*, AstNode*);
void bind_type(Env*, AstNode*);
void bind_program(Env*, AstNode*);

#endif
