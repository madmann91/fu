#ifndef FU_LANG_CHECK_H
#define FU_LANG_CHECK_H

struct ast;
struct log;
struct type_table;

void check_ast(struct ast*, struct type_table*, struct log*);

#endif
