#ifndef FU_LANG_BIND_H
#define FU_LANG_BIND_H

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
void bind_pattern(Env*, AstNode*);
void bind_expr(Env*, AstNode*);
void bind_type(Env*, AstNode*);
void bind_program(Env*, AstNode*);

#endif
