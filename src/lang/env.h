#ifndef FU_LANG_ENV_H
#define FU_LANG_ENV_H

#include <stdbool.h>

struct ast;
struct env;

void free_env(struct env*);
void push_env(struct env**);
void pop_env(struct env**);
bool add_symbol(struct env*, const char*, struct ast*);
struct ast* find_symbol(struct env*, const char*);

#endif
