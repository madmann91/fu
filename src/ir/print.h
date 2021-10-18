#ifndef FU_IR_PRINT_H
#define FU_IR_PRINT_H

#include <stddef.h>

struct ir_node;
struct format_state;

void print_ir(struct format_state*, const struct ir_node*, size_t);
void dump_ir(const struct ir_node*);

#endif
