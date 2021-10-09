#include "core/hash.h"
#include "ir/node.h"

#include <assert.h>

ir_type_t to_type(ir_node_t node) {
    assert(is_type(node));
    return node;
}

ir_kind_t to_kind(ir_node_t node) {
    assert(is_kind(node));
    return node;
}

bool is_type(ir_node_t node) {
    return node->type->tag == IR_KIND_STAR;
}

bool is_kind(ir_node_t node) {
    return node->type == NULL;
}

bool is_valid_pattern(ir_node_t node) {
    switch (node->tag) {
        case IR_NODE_VAR:
        case IR_NODE_TUPLE:
        case IR_NODE_OPTION:
            for (size_t i = 0, n = node->op_count; i < n; ++i) {
                if (!is_valid_pattern(node->ops[i]))
                    return false;
            }
            return true;
        default:
            return false;
    }
}

bool is_tied_var(ir_node_t node) {
    return node->tag == IR_NODE_VAR && node->op_count == 1;
}

bool is_untied_var(ir_node_t node) {
    return node->tag == IR_NODE_VAR && node->op_count == 0;
}

ir_node_t get_tied_val(ir_node_t node) {
    assert(is_tied_var(node));
    return node->ops[0];
}

ir_node_t get_extract_or_insert_val(ir_node_t node) {
    assert(node->tag == IR_NODE_INSERT || node->tag == IR_NODE_EXTRACT);
    return node->ops[0];
}

ir_node_t get_extract_or_insert_index(ir_node_t node) {
    assert(node->tag == IR_NODE_INSERT || node->tag == IR_NODE_EXTRACT);
    return node->ops[1];
}

ir_node_t get_insert_elem(ir_node_t node) {
    assert(node->tag == IR_NODE_INSERT);
    return node->ops[2];
}
