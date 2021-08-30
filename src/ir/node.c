#include <string.h>

#include "core/hash.h"
#include "ir/node.h"

uint32_t hash_ir_node(const struct ir_node* node) {
    uint32_t h = hash_init();
    h = hash_uint32(h, node->tag);
    h = hash_raw_bytes(h, &node->data, node->data_size);
    for (size_t i = 0; i < node->op_count; ++i)
        h = hash_pointer(h, node->ops[i]);
    return h;
}

bool is_same_node(const struct ir_node* left, const struct ir_node* right) {
    if (left->tag != right->tag ||
        left->data_size != right->data_size ||
        left->op_count != right->op_count)
        return false;
    if (memcmp(&left->data, &right->data, left->data_size))
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

size_t let_bindings_count(const struct ir_node* let) {
    return let->op_count / 2;
}

struct let_binding let_binding(const struct ir_node* let, size_t i) {
    return (struct let_binding) {
        .var = let->ops[i * 2 + 0],
        .val = let->ops[i * 2 + 1]
    };
}

const struct ir_node* let_body(const struct ir_node* let) {
    return let->ops[let->op_count - 1];
}
