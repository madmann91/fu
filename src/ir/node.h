#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "core/log.h"

#define IR_KIND_LIST(f) \
    f(STAR) \
    f(NAT)

#define IR_TYPE_LIST(f) \
    f(VEC) \
    f(PTR) \
    f(MEM) \
    f(BOOL) \
    f(ARRAY) \
    f(TUPLE) \
    f(OPTION) \
    f(FUNC) \
    f(INT) \
    f(FLOAT)

#define IR_NODE_LIST(f) \
    f(VAR) \
    f(UNDEF) \
    f(CONST) \
    f(TUPLE) \
    f(EXTRACT) \
    f(INSERT) \
    f(FUNC) \
    f(LET)

struct debug_info {
    struct file_loc loc;
    const char* name;
};

enum ir_node_tag {
#define f(t) IR_NODE_##t,
#define g(t) IR_TYPE_##t,
#define h(t) IR_KIND_##t,
    IR_KIND_LIST(h)
    IR_TYPE_LIST(g)
    IR_NODE_LIST(f)
#undef h
#undef g
#undef f
};

enum fp_math_mode {
    FP_NO_NANS     = 0x01,
    FP_ONLY_FINITE = 0x01,
    FP_COMMUTE     = 0x02,
    FP_DISTRIBUTE  = 0x04,

    FP_FAST = FP_NO_NANS | FP_ONLY_FINITE | FP_COMMUTE | FP_DISTRIBUTE,
    FP_STRICT = 0
};

typedef uint64_t ir_uint_t;
typedef double   ir_float_t;

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
typedef ir_node_t ir_type_t;
typedef ir_node_t ir_kind_t;

ir_type_t to_type(ir_node_t);
ir_kind_t to_kind(ir_node_t);

bool is_type(ir_node_t);
bool is_kind(ir_node_t);
bool is_valid_pattern(ir_node_t);
bool is_tied_var(ir_node_t);
bool is_untied_var(ir_node_t);
bool is_int_const(ir_node_t);
bool is_nat_const(ir_type_t);

ir_uint_t get_nat_const_val(ir_type_t);
ir_uint_t get_int_const_val(ir_node_t);
ir_uint_t get_int_or_nat_const_val(ir_node_t);
ir_float_t get_float_const_val(ir_node_t);

ir_type_t get_tuple_type_elem(ir_type_t, size_t);
ir_type_t get_option_type_elem(ir_type_t, size_t);
ir_type_t get_array_type_elem(ir_type_t);

ir_node_t get_tied_val(ir_node_t);
ir_node_t get_extract_or_insert_val(ir_node_t);
ir_node_t get_extract_or_insert_index(ir_node_t);
ir_node_t get_insert_elem(ir_node_t);

#endif
