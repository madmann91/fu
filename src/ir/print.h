#ifndef FU_IR_PRINT_H
#define FU_IR_PRINT_H

#include <stddef.h>

struct ir_node;
struct ir_var_set;
struct format_state;

void print_ir(struct format_state*, const struct ir_node*, size_t);
void print_var_set(struct format_state*, const struct ir_var_set*);
void dump_ir(const struct ir_node*);
void dump_var_set(const struct ir_var_set*);

#endif
