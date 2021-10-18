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

bool is_int_or_nat_const(ir_node_t node) {
    return is_nat_const(node) || is_int_const(node);
}

bool is_float_const(ir_node_t node) {
    return node->tag == IR_NODE_CONST && node->type->tag == IR_TYPE_FLOAT;
}

bool is_sized_array_type(ir_type_t type) {
    return type->tag == IR_TYPE_ARRAY && type->op_count == 1;
}

bool is_unit_tuple_type(ir_type_t type) {
    return type->tag == IR_TYPE_TUPLE && type->op_count == 0;
}

bool is_unit_tuple(ir_node_t node) {
    return node->tag == IR_NODE_TUPLE && node->op_count == 0;
}

bool is_vec_op(enum ir_node_tag tag) {
    switch (tag) {
#define S(t, s)
#define V(t, s) case IR_NODE_V##t:
#define node(t, s, v) v(t, s)
        IR_NODE_LIST(node)
            return true;
#undef node
#undef V
#undef S
        default:
            return false;
    }
}

#define MAKE_LIST_PREDICATE(name, list) \
    bool name(enum ir_node_tag tag) { \
        switch (tag) { \
            list(node) \
                return true; \
            default: \
                return false; \
        } \
    }

#define V(t) case IR_NODE_V##t:
#define node(t, s, v) case IR_NODE_##t: v(t)

MAKE_LIST_PREDICATE(is_arith_op,       IR_ARITH_OP_LIST)
MAKE_LIST_PREDICATE(is_int_arith_op,   IR_INT_ARITH_OP_LIST)
MAKE_LIST_PREDICATE(is_float_arith_op, IR_FLOAT_ARITH_OP_LIST)
MAKE_LIST_PREDICATE(is_cmp_op,         IR_CMP_OP_LIST)
MAKE_LIST_PREDICATE(is_int_cmp_op,     IR_INT_CMP_OP_LIST)
MAKE_LIST_PREDICATE(is_float_cmp_op,   IR_FLOAT_CMP_OP_LIST)
MAKE_LIST_PREDICATE(is_bit_op,         IR_BIT_OP_LIST)

#undef MAKE_LIST_PREDICATE
#undef node
#undef V

bool has_fp_math_mode(enum ir_node_tag tag) {
    return is_float_cmp_op(tag) || is_float_arith_op(tag);
}

const char* get_node_name(enum ir_node_tag tag) {
    switch (tag)
    {
#define S(t, s)
#define V(t, s) case IR_NODE_V##t: return "v" s;
#define node(t, s, v) case IR_NODE_##t: return s; v(t, s)
#define type(t, s) case IR_TYPE_##t: return s;
#define kind(t, s) case IR_KIND_##t: return s;
        IR_NODE_LIST(node)
        IR_TYPE_LIST(type)
        IR_KIND_LIST(kind)
#undef kind
#undef type
#undef node
#undef V
#undef S
        default:
            assert(false && "invalid node");
            return "";
    }
}

enum ir_node_tag to_vec_tag(enum ir_node_tag tag) {
    switch (tag) {
#define S(t, s)
#define V(t, s) case IR_NODE_##t: return IR_NODE_V##t;
#define node(t, s, v) v(t, s)
        IR_NODE_LIST(node)
#undef node
#undef V
#undef S
        default: return tag;
    }
}

enum ir_node_tag to_scalar_tag(enum ir_node_tag tag) {
    switch (tag) {
#define S(t, s)
#define V(t, s) case IR_NODE_V##t: return IR_NODE_##t;
#define node(t, s, v) v(t, s)
        IR_NODE_LIST(node)
#undef node
#undef V
#undef S
        default: return tag;
    }
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
    assert(is_int_or_nat_const(node));
    return node->data.int_val;
}

ir_float_t get_float_const_val(ir_node_t node) {
    assert(is_float_const(node));
    return node->data.float_val;
}

ir_node_t get_tied_val(ir_node_t node) {
    assert(is_tied_var(node));
    return node->ops[0];
}

ir_node_t get_vec_op_mask(ir_node_t node) {
    assert(is_vec_op(node->tag));
    return node->ops[0];
}

size_t get_tuple_type_elem_count(ir_type_t type) {
    assert(type->tag == IR_TYPE_TUPLE);
    return type->op_count;
}

size_t get_option_type_elem_count(ir_type_t type) {
    assert(type->tag == IR_TYPE_OPTION);
    return type->op_count;
}

size_t get_sized_array_type_elem_count(ir_type_t type) {
    assert(is_sized_array_type(type));
    return get_nat_const_val(type->ops[0]);
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

ir_node_t get_tuple_elem(ir_node_t node, size_t i) {
    assert(node->tag == IR_NODE_TUPLE);
    return node->ops[i];
}

ir_node_t get_extract_or_insert_val(ir_node_t node) {
    assert(
        node->tag == IR_NODE_INSERT || node->tag == IR_NODE_EXTRACT ||
        node->tag == IR_NODE_VINSERT || node->tag == IR_NODE_VEXTRACT);
    return node->ops[is_vec_op(node->tag) ? 1 : 0];
}

ir_node_t get_extract_or_insert_index(ir_node_t node) {
    assert(
        node->tag == IR_NODE_INSERT || node->tag == IR_NODE_EXTRACT ||
        node->tag == IR_NODE_VINSERT || node->tag == IR_NODE_VEXTRACT);
    return node->ops[is_vec_op(node->tag) ? 2 : 1];
}

ir_node_t get_insert_elem(ir_node_t node) {
    assert(node->tag == IR_NODE_INSERT || node->tag == IR_NODE_VINSERT);
    return node->ops[is_vec_op(node->tag) ? 3 : 2];
}
