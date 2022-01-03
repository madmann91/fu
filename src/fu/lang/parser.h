#ifndef FU_LANG_PARSER_H
#define FU_LANG_PARSER_H

#include "fu/lang/lexer.h"
#include "fu/lang/ast.h"
#include "fu/core/mem_pool.h"

#define LOOK_AHEAD 2

typedef struct {
    Lexer* lexer;
    MemPool* mem_pool;
    FilePos prev_end;
    Token ahead[LOOK_AHEAD];
} Parser;

Parser make_parser(Lexer*, MemPool*);

AstNode* parse_stmt(Parser*);
AstNode* parse_decl(Parser*);
AstNode* parse_pattern(Parser*);
AstNode* parse_expr(Parser*);
AstNode* parse_type(Parser*);
AstNode* parse_program(Parser*);

#endif
