#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "core/log.h"

#define IR_KIND_LIST(f) \
    f(STAR, "star") \
    f(NAT,  "nat")

#define IR_TYPE_LIST(f) \
    f(VEC,    "vec") \
    f(PTR,    "ptr") \
    f(MEM,    "mem") \
    f(ARRAY,  "array") \
    f(TUPLE,  "tuple") \
    f(OPTION, "option") \
    f(FUNC,   "func") \
    f(INT,    "int") \
    f(FLOAT,  "float")

#define IR_INT_ARITH_OP_LIST(f) \
    f(IADD, "iadd", V) \
    f(ISUB, "isub", V) \
    f(IMUL, "imul", V) \
    f(IDIV, "idiv", V) \
    f(IREM, "irem", V)

#define IR_FLOAT_ARITH_OP_LIST(f) \
    f(FADD, "fadd", V) \
    f(FSUB, "fsub", V) \
    f(FMUL, "fmul", V) \
    f(FDIV, "fdiv", V) \
    f(FREM, "frem", V)

#define IR_INT_CMP_OP_LIST(f) \
    f(ICMPEQ,  "icmpeq", V) \
    f(ICMPNE,  "icmpne", V) \
    f(ICMPUGT, "icmpugt", V) \
    f(ICMPUGE, "icmpuge", V) \
    f(ICMPULT, "icmpult", V) \
    f(ICMPULE, "icmpule", V) \
    f(ICMPSGT, "icmpsgt", V) \
    f(ICMPSGE, "icmpsge", V) \
    f(ICMPSLT, "icmpslt", V) \
    f(ICMPSLE, "icmpsle", V)

#define IR_FLOAT_CMP_OP_LIST(f) \
    f(FCMPEQ,  "fcmpeq",  V) \
    f(FCMPNE,  "fcmpne",  V) \
    f(FCMPORD, "fcmpord", V) \
    f(FCMPUNO, "fcmpuno", V) \
    f(FCMPUGT, "fcmpugt", V) \
    f(FCMPUGE, "fcmpuge", V) \
    f(FCMPULT, "fcmpult", V) \
    f(FCMPULE, "fcmpule", V) \
    f(FCMPOGT, "fcmpogt", V) \
    f(FCMPOGE, "fcmpoge", V) \
    f(FCMPOLT, "fcmpolt", V) \
    f(FCMPOLE, "fcmpole", V)

#define IR_ARITH_OP_LIST(f) \
    IR_INT_ARITH_OP_LIST(f) \
    IR_FLOAT_ARITH_OP_LIST(f)

#define IR_CMP_OP_LIST(f) \
    IR_INT_CMP_OP_LIST(f) \
    IR_FLOAT_CMP_OP_LIST(f)

#define IR_BIT_OP_LIST(f) \
    f(AND, "and", V) \
    f(OR,  "or",  V) \
    f(XOR, "xor", V)

#define IR_CAST_OP_LIST(f) \
    f(ITOF,    "itof",    V) \
    f(UTOF,    "utof",    V) \
    f(FTOU,    "ftou",    V) \
    f(UTOF,    "utof",    V) \
    f(BITCAST, "bitcast", V)

#define IR_NODE_LIST(f) \
    IR_ARITH_OP_LIST(f) \
    IR_CMP_OP_LIST(f) \
    IR_BIT_OP_LIST(f) \
    f(ERROR,     "error",     S) \
    f(VAR,       "var",       S) \
    f(UNDEF,     "undef",     S) \
    f(CONST,     "const",     S) \
    f(TUPLE,     "tuple",     S) \
    f(EXTRACT,   "extract",   V) \
    f(INSERT,    "insert",    V) \
    f(FUNC,      "func",      S) \
    f(CALL,      "call",      S) \
    f(CALLC,     "callc",     S) \
    f(RETURN,    "return",    S) \
    f(ANY,       "any",       S) \
    f(ALL,       "all",       S) \
    f(BROADCAST, "broadcast", S) \
    f(LET,       "let",       S)

struct debug_info {
    struct file_loc loc;
    const char* name;
};

enum ir_node_tag {
#define S(t)
#define V(t) IR_NODE_V##t,
#define node(t, str, v) IR_NODE_##t, v(t)
#define type(t, ...) IR_TYPE_##t,
#define kind(t, ...) IR_KIND_##t,
    IR_KIND_LIST(kind)
    IR_TYPE_LIST(type)
    IR_NODE_LIST(node)
#undef node
#undef type
#undef kind
#undef S
#undef V
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
bool is_int_or_nat_const(ir_node_t);
bool is_float_const(ir_node_t);
bool is_sized_array_type(ir_type_t);
bool is_unit_tuple_type(ir_type_t);
bool is_unit_tuple(ir_node_t);

bool is_vec_op(enum ir_node_tag);
bool is_arith_op(enum ir_node_tag);
bool is_int_arith_op(enum ir_node_tag);
bool is_float_arith_op(enum ir_node_tag);
bool is_cmp_op(enum ir_node_tag);
bool is_int_cmp_op(enum ir_node_tag);
bool is_float_cmp_op(enum ir_node_tag);
bool is_bit_op(enum ir_node_tag);
bool has_fp_math_mode(enum ir_node_tag);

const char* get_unique_name(ir_node_t);
const char* get_node_name(enum ir_node_tag);

enum ir_node_tag to_vec_tag(enum ir_node_tag);
enum ir_node_tag to_scalar_tag(enum ir_node_tag);

ir_uint_t get_nat_const_val(ir_type_t);
ir_uint_t get_int_const_val(ir_node_t);
ir_uint_t get_int_or_nat_const_val(ir_node_t);
ir_float_t get_float_const_val(ir_node_t);

size_t get_tuple_type_elem_count(ir_type_t);
size_t get_option_type_elem_count(ir_type_t);
size_t get_sized_array_type_elem_count(ir_type_t);
ir_type_t get_tuple_type_elem(ir_type_t, size_t);
ir_type_t get_option_type_elem(ir_type_t, size_t);
ir_type_t get_array_type_elem(ir_type_t);

ir_node_t get_tuple_elem(ir_node_t, size_t);

ir_node_t get_tied_val(ir_node_t);
ir_node_t get_vec_op_mask(ir_node_t);
ir_node_t get_extract_or_insert_val(ir_node_t);
ir_node_t get_extract_or_insert_index(ir_node_t);
ir_node_t get_insert_elem(ir_node_t);

#endif
