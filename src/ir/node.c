#include "core/hash.h"
#include "ir/node.h"

uint32_t hash_ir_node(const struct ir_node* node) {
    uint32_t h = hash_init();
    h = hash_uint32(h, node->tag);
    h = hash_raw_bytes(h, node->data, node->data_size);
    for (size_t i = 0; i < node->op_count; ++i)
        h = hash_pointer(h, node->ops[i]);
    return h;
}

bool are_ir_nodes_equal(const struct ir_node* left, const struct ir_node* right) {
    if (left->tag != right->tag ||
        left->data_size != right->data_size ||
        left->op_count != right->op_count)
        return false;
    for (size_t i = 0, n = left->op_count; i < n; ++i) {
        if (left->ops[i] != right->ops[i])
            return false;
    }
    return true;
}

bool is_valid_pattern(const struct ir_node* node) {
    switch (node->tag) {
        case IR_NODE_VAR:
        case IR_NODE_TUP:
        case IR_NODE_OPT:
            for (size_t i = 0, n = node->op_count; i < n; ++i) {
                if (!is_valid_pattern(node->ops[i]))
                    return false;
            }
            return true;
        default:
            return false;
    }
}
