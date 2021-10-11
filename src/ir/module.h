#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

#include <stddef.h>
#include <stdint.h>

#include "ir/node.h"

struct ir_module;

/*
 * A module is just a hashed collection of nodes.
 * Nodes are hash-consed: Two nodes that compare equal are mapped to the same object,
 * allowing perfect re-use, and automatic common-expression elimination.
 */

struct ir_module* new_ir_module(void);
void free_ir_module(struct ir_module*);

ir_node_t make_node(
    struct ir_module*,
    unsigned,
    ir_type_t, const ir_node_t*, size_t,
    const union ir_node_data*,
    const struct debug_info*);

ir_node_t rebuild_node(struct ir_module*, ir_node_t, ir_type_t, const ir_node_t*, const struct debug_info*);

ir_kind_t make_star(struct ir_module*);
ir_kind_t make_nat(struct ir_module*);

ir_type_t make_int_type(struct ir_module*, ir_type_t);
ir_type_t make_intn_type(struct ir_module*, ir_uint_t);
ir_type_t make_float_type(struct ir_module*, ir_type_t);
ir_type_t make_floatn_type(struct ir_module*, ir_uint_t);
ir_type_t make_tuple_type(struct ir_module*, const ir_type_t*, size_t, const struct debug_info*);
ir_type_t make_option_type(struct ir_module*, const ir_type_t*, size_t, const struct debug_info*);

ir_node_t make_undef(struct ir_module*, ir_type_t);
ir_node_t make_const(struct ir_module*, ir_node_t, const union ir_node_data*, size_t);
ir_type_t make_nat_const(struct ir_module*, ir_uint_t);
ir_node_t make_int_const(struct ir_module*, ir_type_t, ir_uint_t);
ir_node_t make_float_const(struct ir_module*, ir_type_t, ir_float_t);
ir_node_t make_var(struct ir_module*, ir_type_t, size_t, const struct debug_info*);
ir_node_t make_tied_var(struct ir_module*, ir_type_t, size_t, ir_node_t, const struct debug_info*);
ir_node_t make_let(struct ir_module*, const ir_node_t*, size_t, ir_node_t, const struct debug_info*);
ir_node_t make_tuple(struct ir_module*, const ir_node_t*, size_t, const struct debug_info*);
ir_node_t make_option(struct ir_module*, ir_type_t, ir_type_t, ir_node_t, const struct debug_info*);
ir_node_t make_extract(struct ir_module*, ir_node_t, ir_node_t, const struct debug_info*);
ir_node_t make_insert(struct ir_module*, ir_node_t, ir_node_t, ir_node_t, const struct debug_info*);

#endif
