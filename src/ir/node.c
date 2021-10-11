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
            for (size_t i = 0, n = node->op_count; i < n; ++i) {
                if (!is_valid_pattern(node->ops[i]))
                    return false;
            }
            return true;
        case IR_NODE_INSERT:
            return
                get_extract_or_insert_index(node)->type->tag == IR_KIND_NAT &&
                is_valid_pattern(get_extract_or_insert_val(node)) &&
                is_valid_pattern(get_insert_elem(node));
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

bool is_int_const(ir_node_t node) {
    return node->tag == IR_NODE_CONST && node->type->tag == IR_TYPE_INT;
}

bool is_nat_const(ir_type_t type) {
    return type->tag == IR_NODE_CONST && type->type->tag == IR_KIND_NAT;
}

ir_uint_t get_nat_const_val(ir_type_t type) {
    assert(is_nat_const(type));
    return type->data.int_val;
}

ir_uint_t get_int_const_val(ir_node_t node) {
    assert(is_int_const(node));
    return node->data.int_val;
}

ir_uint_t get_int_or_nat_const_val(ir_node_t node) {
    assert(is_int_const(node) || is_nat_const(node));
    return node->data.int_val;
}

ir_float_t get_float_const_val(ir_node_t node) {
    assert(node->tag == IR_NODE_CONST && node->type->tag == IR_TYPE_FLOAT);
    return node->data.float_val;
}

ir_node_t get_tied_val(ir_node_t node) {
    assert(is_tied_var(node));
    return node->ops[0];
}

ir_type_t get_tuple_type_elem(ir_type_t type, size_t i) {
    assert(type->tag == IR_TYPE_TUPLE);
    assert(i < type->op_count);
    return type->ops[i];
}

ir_type_t get_option_type_elem(ir_type_t type, size_t i) {
    assert(type->tag == IR_TYPE_OPTION);
    assert(i < type->op_count);
    return type->ops[i];
}

ir_type_t get_array_type_elem(ir_type_t type) {
    assert(type->tag == IR_TYPE_ARRAY);
    return type->ops[0];
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
