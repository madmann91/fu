#include "ir/module.h"
#include "ir/node.h"
#include "core/alloc.h"

#include <assert.h>
#include <string.h>

static inline ir_node_t see_thru(ir_node_t node) {
    return is_tied_var(node) ? get_tied_val(node) : node;
}

static inline ir_node_t find_insert_with_index(ir_node_t node, ir_node_t index) {
    while (node->tag == IR_NODE_INSERT) {
        ir_node_t other_index = see_thru(get_extract_or_insert_index(node));
        if (other_index == index)
            return node;
        if (other_index->tag != IR_NODE_CONST || index->tag != IR_NODE_CONST)
            break;
        node = see_thru(get_extract_or_insert_val(node));
    }
    return NULL;
}

static inline ir_node_t remove_insert_with_index(struct ir_module* module, ir_node_t node, ir_node_t index) {
    if (see_thru(get_extract_or_insert_index(node)) == index)
        return get_extract_or_insert_val(node);
    return rebuild_node(module, node, node->type, (ir_node_t[]) {
        remove_insert_with_index(module, see_thru(get_extract_or_insert_val(node)), index),
        get_extract_or_insert_index(node),
        get_insert_elem(node)
    }, node->debug);
}

static inline ir_node_t simplify_insert(struct ir_module* module, ir_node_t node) {
    ir_node_t val   = see_thru(get_extract_or_insert_val(node));
    ir_node_t index = see_thru(get_extract_or_insert_index(node));

    if (find_insert_with_index(val, index))
        val = remove_insert_with_index(module, val, index);

    if (val->tag == IR_NODE_TUPLE) {
        ir_node_t* ops = malloc_or_die(sizeof(ir_node_t) * val->op_count);
        memcpy(ops, val->ops, sizeof(ir_node_t) * val->op_count);
        ops[get_int_or_nat_const_val(index)] = get_insert_elem(node);
        return make_tuple(module, ops, val->op_count, node->debug);
    }

    return node;
}

static inline ir_node_t simplify_extract(ir_node_t node) {
    ir_node_t val   = see_thru(get_extract_or_insert_val(node));
    ir_node_t index = see_thru(get_extract_or_insert_index(node));

    ir_node_t insert = find_insert_with_index(val, index);
    if (insert)
        return get_insert_elem(insert);

    if (val->tag == IR_NODE_TUPLE)
        return val->ops[get_int_or_nat_const_val(index)];

    return node;
}

ir_node_t simplify_ir_node(struct ir_module* module, ir_node_t node) {
    switch (node->tag) {
        case IR_NODE_INSERT:  return simplify_insert(module, node);
        case IR_NODE_EXTRACT: return simplify_extract(node);
        default: return node;
    }
}
