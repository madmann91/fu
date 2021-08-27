#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

struct ir_module;

struct ir_module* new_ir_module(void);
void insert_ir_node(struct ir_module*, const struct ir_node*);

#endif
