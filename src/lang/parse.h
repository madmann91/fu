#ifndef FU_LANG_PARSE_H
#define FU_LANG_PARSE_H

struct ast;
struct lexer;
struct mem_pool;

struct ast* parse_ast(struct mem_pool*, struct lexer*);

#endif
