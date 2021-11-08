#include "ir/module.h"
#include "ir/node.h"
#include "core/alloc.h"

#include <assert.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>

static inline ir_val_t see_thru(ir_val_t val) {
    return is_tied_var(as_node(val)) ? to_val(get_tied_val(as_node(val))) : val;
}

static inline ir_val_t simplify_let(struct ir_module* module, ir_val_t let) {
    ir_val_t body = see_thru(get_let_body(let));
    size_t var_count = get_let_var_count(let);

    // Remove variables that are not used in the body
    ir_val_t* used_vars = malloc_or_die(sizeof(ir_val_t) * var_count);
    size_t used_var_count = 0;
    for (size_t i = 0; i < var_count; ++i) {
        ir_node_t untied_var = untie_var(module, as_node(get_let_var(let, i)));
        if (contains_var(body->free_vars, untied_var))
            used_vars[used_var_count++] = get_let_var(let, i);
    }

    ir_val_t result = let;
    if (used_var_count == 0)
        result = body;
    else if (used_var_count != var_count)
        result = make_let(module, used_vars, used_var_count, body, let->debug);
    free(used_vars);
    return result;
}

static inline ir_val_t find_insert_with_index(ir_val_t val, ir_val_t index, ir_val_t mask) {
    while (is_insert(val->tag)) {
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
        val = make_bot(module, to_type(val->type));

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
        return make_bot(module, get_option_type_elem(to_type(val->type), get_int_const_val(index)));

    return extract;
}

static inline ir_val_t simplify_bot(struct ir_module* module, ir_val_t bot) {
    if (is_unit_tuple_type(to_type(bot->type)))
        return make_tuple(module, NULL, 0, bot->debug);
    return bot;
}

static inline ir_val_t simplify_int_arith_op(struct ir_module* module, ir_val_t int_arith_op) {
    ir_val_t left  = see_thru(get_left_operand(int_arith_op));
    ir_val_t right = see_thru(get_right_operand(int_arith_op));
    if (left->tag == IR_CONST && right->tag == IR_CONST) {
        ir_val_t result = NULL;
        switch (int_arith_op->tag) {
            case IR_VAL_IADD: return make_int_const(module, to_type(left->type), left->data.int_val + right->data.int_val);
            case IR_VAL_ISUB: return make_int_const(module, to_type(left->type), left->data.int_val - right->data.int_val);
            case IR_VAL_IMUL: return make_int_const(module, to_type(left->type), left->data.int_val * right->data.int_val);
            case IR_VAL_UDIV: result = make_int_const(module, to_type(left->type), left->data.int_val / right->data.int_val); break;
            case IR_VAL_SDIV: result = make_int_const(module, to_type(left->type), ((intmax_t)left->data.int_val) / ((intmax_t)right->data.int_val)); break;
            case IR_VAL_UREM: result = make_int_const(module, to_type(left->type), left->data.int_val % right->data.int_val); break;
            case IR_VAL_SREM: result = make_int_const(module, to_type(left->type), ((intmax_t)left->data.int_val) % ((intmax_t)right->data.int_val)); break;
            default:
                assert(false && "invalid integer operation");
                return int_arith_op;
        }
        return make_tuple(module, (ir_val_t[]) { get_err(int_arith_op), result }, 2, int_arith_op->debug);
    }
    return int_arith_op;
}

static ir_float_t fadd_32(ir_float_t x, ir_float_t y) { return ((float)x) + ((float)y); }
static ir_float_t fsub_32(ir_float_t x, ir_float_t y) { return ((float)x) - ((float)y); }
static ir_float_t fmul_32(ir_float_t x, ir_float_t y) { return ((float)x) * ((float)y); }
static ir_float_t fdiv_32(ir_float_t x, ir_float_t y) { return ((float)x) / ((float)y); }
static ir_float_t frem_32(ir_float_t x, ir_float_t y) { return fmodf(x, y); }

static ir_float_t fadd_64(ir_float_t x, ir_float_t y) { return x + y; }
static ir_float_t fsub_64(ir_float_t x, ir_float_t y) { return x - y; }
static ir_float_t fmul_64(ir_float_t x, ir_float_t y) { return x * y; }
static ir_float_t fdiv_64(ir_float_t x, ir_float_t y) { return x / y; }
static ir_float_t frem_64(ir_float_t x, ir_float_t y) { return fmod(x, y); }

typedef ir_float_t (*float_op_t) (ir_float_t, ir_float_t);

static inline ir_float_t apply_float_op(ir_val_t left, ir_val_t right, float_op_t op_32, float_op_t op_64) {
    switch (get_int_or_float_type_bitwidth_as_int(to_type(left->type))) {
        case 32: return op_32(left->data.float_val, right->data.float_val);
        case 64: return op_64(left->data.float_val, right->data.float_val);
        default:
            assert(false && "unsupported float type bitwidth");
            return 0;
    }
}

static inline ir_val_t simplify_float_arith_op(struct ir_module* module, ir_val_t float_arith_op) {
    ir_val_t left  = see_thru(get_left_operand(float_arith_op));
    ir_val_t right = see_thru(get_right_operand(float_arith_op));
    if (left->tag == IR_CONST && right->tag == IR_CONST) {
        ir_val_t result = NULL;
        switch (float_arith_op->tag) {
            case IR_VAL_FADD: return make_float_const(module, to_type(left->type), apply_float_op(left, right, fadd_32, fadd_64));
            case IR_VAL_FSUB: return make_float_const(module, to_type(left->type), apply_float_op(left, right, fsub_32, fsub_64));
            case IR_VAL_FMUL: return make_float_const(module, to_type(left->type), apply_float_op(left, right, fmul_32, fmul_64));
            case IR_VAL_FDIV: result = make_float_const(module, to_type(left->type), apply_float_op(left, right, fdiv_32, fdiv_64)); break;
            case IR_VAL_FREM: result = make_float_const(module, to_type(left->type), apply_float_op(left, right, frem_32, frem_64)); break;
            default:
                assert(false && "invalid integer operation");
                return float_arith_op;
        }
        return make_tuple(module, (ir_val_t[]) { get_err(float_arith_op), result }, 2, float_arith_op->debug);
    }
    return float_arith_op;
}

ir_node_t simplify_node(struct ir_module* module, ir_node_t node) {
    if (node->tag == IR_VAL_LET)
        return as_node(simplify_let(module, to_val(node)));
    if (is_int_arith_op(node->tag))
        return as_node(simplify_int_arith_op(module, to_val(node)));
    if (is_float_arith_op(node->tag))
        return as_node(simplify_float_arith_op(module, to_val(node)));
    if (node->tag == IR_VAL_BOT)
        return as_node(simplify_bot(module, to_val(node)));
    if (node->tag == IR_VAL_INSERT)
        return as_node(simplify_insert(module, to_val(node)));
    if (node->tag == IR_VAL_EXTRACT)
        return as_node(simplify_extract(module, to_val(node)));
    return node;
}
