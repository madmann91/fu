#include "ir/module.h"
#include "ir/node.h"

static inline ir_node_t see_thru(ir_node_t node) {
    return is_tied_var(node) ? get_tied_val(node) : node;
}

static inline ir_node_t find_same_insert_index(ir_node_t node) {
    ir_node_t elem  = see_thru(get_insert_elem(node));
    ir_node_t index = see_thru(get_extract_or_insert_index(node));
    while (elem->tag == IR_NODE_INSERT) {
        ir_node_t other_index = see_thru(get_extract_or_insert_index(elem));
        if (other_index == index)
            return index;
        if (other_index->tag != IR_NODE_CONST || index->tag != IR_NODE_CONST)
            break;
        elem = see_thru(get_extract_or_insert_val(elem));
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
    ir_node_t same_index = find_same_insert_index(node);
    if (same_index)
        return remove_insert_with_index(module, node, same_index);
    return node;
}

static inline ir_node_t simplify_extract(struct ir_module* module, ir_node_t node) {
    return node;
}

ir_node_t simplify_ir_node(struct ir_module* module, ir_node_t node) {
    switch (node->tag) {
        case IR_NODE_INSERT:  return simplify_insert(module, node);
        case IR_NODE_EXTRACT: return simplify_extract(module, node);
        default: return node;
    }
}
