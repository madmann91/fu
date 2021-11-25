#ifndef FU_IR_PRINT_H
#define FU_IR_PRINT_H

#include <stddef.h>

struct ir_node;
struct var_set;
struct format_state;

void print_ir_node(struct format_state*, const struct ir_node*, size_t);
void print_var_set(struct format_state*, const struct var_set*);
void dump_ir_node(const struct ir_node*);
void dump_var_set(const struct var_set*);

#endif
