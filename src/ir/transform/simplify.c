#include "ir/module.h"
#include "ir/node.h"
#include "core/alloc.h"

#include <assert.h>
#include <string.h>

static inline ir_val_t see_thru(ir_val_t val) {
    return is_tied_var(as_node(val)) ? to_val(get_tied_val(as_node(val))) : val;
}

static inline ir_val_t find_insert_with_index(ir_val_t val, ir_val_t index, ir_val_t mask) {
    while (is_insert(val)) {
        if (is_vec_op(val->tag) && get_vec_op_mask(val) != mask)
            break;
        ir_val_t other_index = see_thru(get_extract_or_insert_index(val));
        if (other_index == index)
            return val;
        if (other_index->tag != IR_CONST || index->tag != IR_CONST)
            break;
        val = see_thru(get_extract_or_insert_val(val));
    }
    return NULL;
}

static inline ir_val_t remove_insert_with_index(struct ir_module* module, ir_val_t val, ir_val_t index) {
    if (see_thru(get_extract_or_insert_index(val)) == index)
        return get_extract_or_insert_val(val);
    return rebuild_val(module, val, to_type(val->type), (ir_val_t[]) {
        remove_insert_with_index(module, see_thru(get_extract_or_insert_val(val)), index),
        get_extract_or_insert_index(val),
        get_insert_elem(val)
    }, val->debug);
}

static inline ir_val_t simplify_insert(struct ir_module* module, ir_val_t insert) {
    ir_val_t mask  = is_vec_op(insert->tag) ? see_thru(get_vec_op_mask(insert)) : NULL;
    ir_val_t val   = see_thru(get_extract_or_insert_val(insert));
    ir_val_t index = see_thru(get_extract_or_insert_index(insert));

    if (val->type->tag == IR_TYPE_OPTION)
        val = make_undef(module, to_type(val->type));

    if (find_insert_with_index(val, index, mask))
        val = remove_insert_with_index(module, val, index);

    if (val->tag == IR_VAL_TUPLE) {
        ir_val_t* ops = malloc_or_die(sizeof(ir_val_t) * val->op_count);
        memcpy(ops, val->ops, sizeof(ir_val_t) * val->op_count);
        ops[get_int_const_val(index)] = get_insert_elem(insert);
        return make_tuple(module, ops, val->op_count, insert->debug);
    }

    return insert;
}

static inline ir_val_t simplify_extract(struct ir_module* module, ir_val_t extract) {
    ir_val_t mask  = is_vec_op(extract->tag) ? see_thru(get_vec_op_mask(extract)) : NULL;
    ir_val_t val   = see_thru(get_extract_or_insert_val(extract));
    ir_val_t index = see_thru(get_extract_or_insert_index(extract));

    ir_val_t other_insert = find_insert_with_index(val, index, mask);
    if (other_insert)
        return get_insert_elem(other_insert);

    if (val->tag == IR_VAL_TUPLE)
        return get_tuple_elem(val, get_int_const_val(index));

    if (val->tag == IR_VAL_INSERT && val->type->tag == IR_TYPE_OPTION)
        return make_undef(module, get_option_type_elem(to_type(val->type), get_int_const_val(index)));

    return extract;
}

static inline ir_val_t simplify_undef(struct ir_module* module, ir_val_t undef) {
    if (is_unit_tuple_type(to_type(undef->type)))
        return make_tuple(module, NULL, 0, undef->debug);
    return undef;
}

static inline ir_val_t simplify_let(struct ir_module* module, ir_val_t let) {
    // TODO
    return let;
}

static inline ir_val_t simplify_int_arith_op(struct ir_module* module, ir_val_t int_arith_op) {
    ir_val_t left  = see_thru(get_left_operand(int_arith_op));
    ir_val_t right = see_thru(get_right_operand(int_arith_op));
    if (left->tag == IR_CONST && right->tag == IR_CONST) {
        switch (int_arith_op->tag) {
            case IR_VAL_IADD: return make_int_const(module, to_type(left->type), left->data.int_val + right->data.int_val);
            case IR_VAL_ISUB: return make_int_const(module, to_type(left->type), left->data.int_val - right->data.int_val);
            case IR_VAL_IMUL: return make_int_const(module, to_type(left->type), left->data.int_val * right->data.int_val);
            case IR_VAL_UDIV: return make_int_const(module, to_type(left->type), left->data.int_val / right->data.int_val);
            case IR_VAL_SDIV: return make_int_const(module, to_type(left->type), ((intmax_t)left->data.int_val) / ((intmax_t)right->data.int_val));
            case IR_VAL_UREM: return make_int_const(module, to_type(left->type), left->data.int_val % right->data.int_val);
            case IR_VAL_SREM: return make_int_const(module, to_type(left->type), ((intmax_t)left->data.int_val) % ((intmax_t)right->data.int_val));
            default:
                assert(false && "invalid integer operation");
                break;
        }
    }
    return int_arith_op;
}

ir_node_t simplify_ir_node(struct ir_module* module, ir_node_t node) {
    if (node->tag == IR_VAL_LET)
        return as_node(simplify_let(module, to_val(node)));
    if (is_int_arith_op(node->tag))
        return as_node(simplify_int_arith_op(module, to_val(node)));
    if (node->tag == IR_VAL_UNDEF)
        return as_node(simplify_undef(module, to_val(node)));
    if (node->tag == IR_VAL_INSERT)
        return as_node(simplify_insert(module, to_val(node)));
    if (node->tag == IR_VAL_EXTRACT)
        return as_node(simplify_extract(module, to_val(node)));
    return node;
}
