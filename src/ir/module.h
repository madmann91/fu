#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

struct ir_module;
struct ir_node;
struct file_loc;

/*
 * A module is just a hashed collection of nodes.
 * Nodes are hash-consed: Two nodes that compare equal are mapped to the same object,
 * allowing perfect re-use, and automatic common-expression elimination.
 */

struct ir_module* new_ir_module(void);
void free_ir_module(struct ir_module*);

const struct debug_info* make_debug_info(struct ir_module*, const struct file_loc*, const char* name);
const struct ir_node* make_var(struct ir_module*, size_t var_index, const struct debug_info*);

#endif
