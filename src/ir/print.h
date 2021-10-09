#ifndef FU_IR_PRINT_H
#define FU_IR_PRINT_H

struct ir_node;
struct format_state;

void print_ir(struct format_state* state, const struct ir_node*);
void dump_ir(const struct ir_node*);

#endif
