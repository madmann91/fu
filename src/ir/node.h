#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "core/log.h"
#include "ir/node_list.h"

typedef uint64_t ir_uint_t;
typedef double   ir_float_t;

struct debug_info {
    struct file_loc loc;
    const char* name;
};

enum fp_math_mode {
    FP_NO_NANS     = 0x01,
    FP_ONLY_FINITE = 0x01,
    FP_COMMUTE     = 0x02,
    FP_DISTRIBUTE  = 0x04,

    FP_FAST = FP_NO_NANS | FP_ONLY_FINITE | FP_COMMUTE | FP_DISTRIBUTE,
    FP_STRICT = 0
};

enum ir_node_tag {
#define scalar_op(tag, str, n)  IR_VAL_##tag,
#define vec_op(tag, str, n) IR_VAL_V##tag, scalar_op(tag, str, n)
#define type(tag, str, n) IR_TYPE_##tag,
#define kind(tag, str, n) IR_KIND_##tag,
    IR_ERROR,
    IR_CONST,
    IR_VAR,
    IR_KIND_LIST(kind)
    IR_TYPE_LIST(type)
    IR_VEC_OP_LIST(vec_op)
    IR_SCALAR_OP_LIST(scalar_op)
    IR_NODE_COUNT
#undef scalar_op
#undef vec_op
#undef type
#undef kind
};

union ir_node_data {
    ir_uint_t int_val;
    ir_float_t float_val;
    size_t var_index;
    unsigned fp_math;
};

#define IR_NODE_FIELDS \
    enum ir_node_tag tag : 32; \
    uint32_t data_size; \
    union ir_node_data data; \
    size_t op_count; \
    const struct ir_node* type; \
    const struct debug_info* debug;

struct ir_node {
    IR_NODE_FIELDS
    const struct ir_node* ops[];
};

typedef const struct ir_node* ir_node_t;

// These types prevent mixing different node sorts together.
typedef const struct { IR_NODE_FIELDS const struct ir_node* ops[]; }* ir_kind_t;
typedef const struct { IR_NODE_FIELDS const struct ir_node* ops[]; }* ir_type_t;
typedef const struct { IR_NODE_FIELDS const struct ir_node* ops[]; }* ir_val_t;

bool is_kind(ir_node_t);
bool is_type(ir_node_t);
bool is_val(ir_node_t);

ir_kind_t to_kind(ir_node_t);
ir_type_t to_type(ir_node_t);
ir_val_t  to_val(ir_node_t);

#define as_node(n) ((ir_node_t)(n))

bool is_pattern(ir_val_t);
bool is_tied_var(ir_node_t);
bool is_untied_var(ir_node_t);
bool is_int_const(ir_node_t);
bool is_nat_const(ir_node_t);
bool is_int_or_nat_const(ir_node_t);
bool is_float_const(ir_node_t);
bool is_sized_array_type(ir_type_t);
bool is_unit_tuple_type(ir_type_t);
bool is_unit_tuple(ir_val_t);

bool is_vec_op(enum ir_node_tag);
bool is_int_op(enum ir_node_tag);
bool is_float_op(enum ir_node_tag);
bool is_arith_op(enum ir_node_tag);
bool is_int_arith_op(enum ir_node_tag);
bool is_float_arith_op(enum ir_node_tag);
bool is_cmp_op(enum ir_node_tag);
bool is_int_cmp_op(enum ir_node_tag);
bool is_float_cmp_op(enum ir_node_tag);
bool is_bit_op(enum ir_node_tag);

bool is_insert(enum ir_node_tag);
bool is_extract(enum ir_node_tag);
bool is_extract_or_insert(enum ir_node_tag);

bool has_fp_math_mode(enum ir_node_tag);
bool has_err(enum ir_node_tag);
bool has_mem(enum ir_node_tag);

const char* get_unique_name(ir_node_t);
const char* get_node_name(enum ir_node_tag);
size_t get_expected_op_count(enum ir_node_tag);

enum ir_node_tag to_vec_tag(enum ir_node_tag);
enum ir_node_tag to_scalar_tag(enum ir_node_tag);

ir_type_t get_int_or_float_type_bitwidth(ir_type_t);
size_t get_int_or_float_type_bitwidth_as_int(ir_type_t);
ir_uint_t get_int_type_bitmask(ir_type_t);

ir_uint_t get_nat_const_val(ir_type_t);
ir_uint_t get_int_const_val(ir_val_t);
ir_uint_t get_int_or_nat_const_val(ir_node_t);
ir_float_t get_float_const_val(ir_val_t);

size_t get_tuple_type_elem_count(ir_type_t);
size_t get_option_type_elem_count(ir_type_t);
size_t get_sized_array_type_elem_count(ir_type_t);
ir_type_t get_tuple_type_elem(ir_type_t, size_t);
ir_type_t get_option_type_elem(ir_type_t, size_t);
ir_type_t get_array_type_elem(ir_type_t);

ir_val_t get_tuple_elem(ir_val_t, size_t);

ir_node_t get_tied_val(ir_node_t);
ir_val_t get_vec_op_mask(ir_val_t);
ir_val_t get_extract_or_insert_val(ir_val_t);
ir_val_t get_extract_or_insert_index(ir_val_t);
ir_val_t get_insert_elem(ir_val_t);

ir_val_t get_err(ir_val_t);
ir_val_t get_mem(ir_val_t);
ir_val_t get_left_operand(ir_val_t);
ir_val_t get_right_operand(ir_val_t);

#endif
