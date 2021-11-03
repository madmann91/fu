#include "core/hash.h"
#include "ir/node.h"

#include <assert.h>

ir_type_t to_type(ir_node_t node) {
    assert(is_type(node));
    return (ir_type_t)node;
}

ir_kind_t to_kind(ir_node_t node) {
    assert(is_kind(node));
    return (ir_kind_t)node;
}

ir_val_t to_val(ir_node_t node) {
    assert(is_val(node));
    return (ir_val_t)node;
}

bool is_kind(ir_node_t node) {
    return node->type == NULL || node->tag == IR_ERROR;
}

bool is_type(ir_node_t node) {
    return (node->type && is_kind(node->type)) || node->tag == IR_ERROR;
}

bool is_val(ir_node_t node) {
    return (node->type && is_type(node->type)) || node->tag == IR_ERROR;
}

bool is_pattern(ir_val_t val) {
    switch (val->tag) {
        case IR_VAR:
            return true;
        case IR_VAL_TUPLE:
            for (size_t i = 0, n = val->op_count; i < n; ++i) {
                if (!is_pattern(to_val(val->ops[i])))
                    return false;
            }
            return true;
        case IR_VAL_INSERT:
            return
                get_extract_or_insert_index(val)->type->tag == IR_KIND_NAT &&
                is_pattern(get_extract_or_insert_val(val)) &&
                is_pattern(get_insert_elem(val));
        default:
            return false;
    }
}

bool is_tied_var(ir_node_t node) {
    return node->tag == IR_VAR && node->op_count == 1;
}

bool is_untied_var(ir_node_t node) {
    return node->tag == IR_VAR && node->op_count == 0;
}

bool is_int_const(ir_node_t node) {
    return node->tag == IR_CONST && node->type->tag == IR_TYPE_INT;
}

bool is_nat_const(ir_node_t node) {
    return node->tag == IR_CONST && node->type->tag == IR_KIND_NAT;
}

bool is_int_or_nat_const(ir_node_t node) {
    return is_nat_const(node) || is_int_const(node);
}

bool is_float_const(ir_node_t node) {
    return node->tag == IR_CONST && node->type->tag == IR_TYPE_FLOAT;
}

bool is_sized_array_type(ir_type_t type) {
    return type->tag == IR_TYPE_ARRAY && type->op_count == 1;
}

bool is_unit_tuple_type(ir_type_t type) {
    return type->tag == IR_TYPE_TUPLE && type->op_count == 0;
}

bool is_unit_tuple(ir_val_t val) {
    return val->tag == IR_VAL_TUPLE && val->op_count == 0;
}

bool is_extract(ir_val_t val) {
    return val->tag == IR_VAL_EXTRACT || val->tag == IR_VAL_VEXTRACT;
}

bool is_insert(ir_val_t val) {
    return val->tag == IR_VAL_INSERT || val->tag == IR_VAL_VINSERT;
}

bool is_extract_or_insert(ir_val_t val) {
    return is_extract(val) || is_insert(val);
}

#define MAKE_PREDICATE(name, ...) \
    bool name(enum ir_node_tag tag) { \
        switch (tag) { \
            __VA_ARGS__ return true; \
            default: return false; \
        } \
    }

#define vec_op(tag, str, n) case IR_VAL_V##tag:
#define other_op(tag, str, n) case IR_VAL_##tag: case IR_VAL_V##tag:
MAKE_PREDICATE(is_vec_op,         IR_VEC_OP_LIST(vec_op))
MAKE_PREDICATE(is_arith_op,       IR_ARITH_OP_LIST(other_op))
MAKE_PREDICATE(is_int_op,         IR_INT_OP_LIST(other_op))
MAKE_PREDICATE(is_float_op,       IR_FLOAT_OP_LIST(other_op))
MAKE_PREDICATE(is_cmp_op,         IR_CMP_OP_LIST(other_op))
MAKE_PREDICATE(is_bit_op,         IR_BIT_OP_LIST(other_op))
MAKE_PREDICATE(is_int_arith_op,   IR_INT_ARITH_OP_LIST(other_op))
MAKE_PREDICATE(is_float_arith_op, IR_FLOAT_ARITH_OP_LIST(other_op))
MAKE_PREDICATE(is_int_cmp_op,     IR_INT_CMP_OP_LIST(other_op))
MAKE_PREDICATE(is_float_cmp_op,   IR_FLOAT_CMP_OP_LIST(other_op))
#undef MAKE_PREDICATE
#undef vec_op
#undef other_op

const char* get_node_name(enum ir_node_tag tag) {
    switch (tag) {
#define scalar_op(tag, str, n) case IR_VAL_##tag: return str;
#define vec_op(tag, str, n)    scalar_op(tag, str, n) case IR_VAL_V##tag: return "v" str;
#define type(tag, str, n)      case IR_TYPE_##tag: return str;
#define kind(tag, str, n)      case IR_KIND_##tag: return str;
        case IR_ERROR: return "error";
        case IR_CONST: return "const";
        case IR_VAR:   return "var";
        IR_KIND_LIST(kind)
        IR_TYPE_LIST(type)
        IR_SCALAR_OP_LIST(scalar_op)
        IR_VEC_OP_LIST(vec_op)
#undef scalar_op
#undef vec_op
#undef type
#undef kind
        default:
            assert(false && "invalid node");
            return "";
    }
}

size_t get_expected_op_count(enum ir_node_tag tag) {
    switch (tag) {
#define scalar_op(tag, str, n) case IR_VAL_##tag: return n;
#define vec_op(tag, str, n) case IR_VAL_V##tag: return n; scalar_op(tag, str, n)
#define type(tag, str, n) case IR_TYPE_##tag: return n;
#define kind(tag, str, n) case IR_KIND_##tag: return n;
#define N SIZE_MAX
        case IR_ERROR: return 0;
        case IR_CONST: return 0;
        case IR_VAR:   return SIZE_MAX;
        IR_KIND_LIST(kind)
        IR_TYPE_LIST(type)
        IR_SCALAR_OP_LIST(scalar_op)
        IR_VEC_OP_LIST(vec_op)
#undef scalar_op
#undef vec_op
#undef type
#undef kind
        default: return SIZE_MAX;
    }
}

enum ir_node_tag to_vec_tag(enum ir_node_tag tag) {
    switch (tag) {
#define vec_op(tag, str, n) case IR_VAL_##tag: return IR_VAL_V##tag;
        IR_VEC_OP_LIST(vec_op)
#undef vec_op
        default: return tag;
    }
}

enum ir_node_tag to_scalar_tag(enum ir_node_tag tag) {
    switch (tag) {
#define vec_op(tag, str, n) case IR_VAL_V##tag: return IR_VAL_##tag;
        IR_VEC_OP_LIST(vec_op)
#undef vec_op
        default: return tag;
    }
}

bool has_fp_math_mode(enum ir_node_tag tag) {
    return is_float_op(tag) && (is_cmp_op(tag) || is_arith_op(tag));
}

ir_uint_t get_nat_const_val(ir_type_t type) {
    assert(is_nat_const(as_node(type)));
    return type->data.int_val;
}

ir_uint_t get_int_const_val(ir_val_t val) {
    assert(is_int_const(as_node(val)));
    return val->data.int_val;
}

ir_uint_t get_int_or_nat_const_val(ir_node_t node) {
    assert(is_int_or_nat_const(node));
    return node->data.int_val;
}

ir_float_t get_float_const_val(ir_val_t val) {
    assert(is_float_const(as_node(val)));
    return val->data.float_val;
}

ir_node_t get_tied_val(ir_node_t node) {
    assert(is_tied_var(node));
    return node->ops[0];
}

ir_val_t get_vec_op_mask(ir_val_t val) {
    assert(is_vec_op(val->tag));
    return to_val(val->ops[0]);
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
    return get_nat_const_val(to_type(type->ops[0]));
}

ir_type_t get_tuple_type_elem(ir_type_t type, size_t i) {
    assert(type->tag == IR_TYPE_TUPLE);
    assert(i < type->op_count);
    return to_type(type->ops[i]);
}

ir_type_t get_option_type_elem(ir_type_t type, size_t i) {
    assert(type->tag == IR_TYPE_OPTION);
    assert(i < type->op_count);
    return to_type(type->ops[i]);
}

ir_type_t get_array_type_elem(ir_type_t type) {
    assert(type->tag == IR_TYPE_ARRAY);
    return to_type(type->ops[0]);
}

ir_val_t get_tuple_elem(ir_val_t val, size_t i) {
    assert(val->tag == IR_VAL_TUPLE);
    return to_val(val->ops[i]);
}

ir_val_t get_extract_or_insert_val(ir_val_t val) {
    assert(is_extract_or_insert(val));
    return to_val(val->ops[is_vec_op(val->tag) ? 1 : 0]);
}

ir_val_t get_extract_or_insert_index(ir_val_t val) {
    assert(is_extract_or_insert(val));
    return to_val(val->ops[is_vec_op(val->tag) ? 2 : 1]);
}

ir_val_t get_insert_elem(ir_val_t val) {
    assert(is_insert(val));
    return to_val(val->ops[is_vec_op(val->tag) ? 3 : 2]);
}
