#ifndef FU_LANG_PARSER_H
#define FU_LANG_PARSER_H

#include "fu/lang/token.h"
#include "fu/core/log.h"

#define LOOK_AHEAD 2

typedef struct MemPool MemPool;
typedef struct Lexer Lexer;
typedef struct AstNode AstNode;

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
