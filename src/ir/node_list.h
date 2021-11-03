#ifndef FU_IR_NODE_LIST_H
#define FU_IR_NODE_LIST_H

#include "core/utils.h"

#define IR_KIND_LIST(f) \
    f(STAR, "star", 0) \
    f(NAT,  "nat",  0)

#define IR_TYPE_LIST(f) \
    f(VEC,    "vec",    1) \
    f(PTR,    "ptr",    0) \
    f(MEM,    "mem",    0) \
    f(ERR,    "err",    0) \
    f(ARRAY,  "array",  1) \
    f(TUPLE,  "tuple",  N) \
    f(OPTION, "option", N) \
    f(FUNC,   "func",   2) \
    f(INT,    "int",    1) \
    f(FLOAT,  "float",  1)

#define IR_INT_ARITH_OP_LIST(f) \
    f(IADD, "iadd", 2) \
    f(ISUB, "isub", 2) \
    f(IMUL, "imul", 2) \
    f(SDIV, "sdiv", 3) \
    f(SREM, "srem", 3) \
    f(UDIV, "udiv", 3) \
    f(UREM, "urem", 3)

#define IR_FLOAT_ARITH_OP_LIST(f) \
    f(FADD, "fadd", 2) \
    f(FSUB, "fsub", 2) \
    f(FMUL, "fmul", 2) \
    f(FDIV, "fdiv", 3) \
    f(FREM, "frem", 3)

#define IR_ARITH_OP_LIST(f) \
    IR_INT_ARITH_OP_LIST(f) \
    IR_FLOAT_ARITH_OP_LIST(f)

#define IR_INT_CMP_OP_LIST(f) \
    f(ICMPEQ,  "icmpeq",  2) \
    f(ICMPNE,  "icmpne",  2) \
    f(ICMPUGT, "icmpugt", 2) \
    f(ICMPUGE, "icmpuge", 2) \
    f(ICMPULT, "icmpult", 2) \
    f(ICMPULE, "icmpule", 2) \
    f(ICMPSGT, "icmpsgt", 2) \
    f(ICMPSGE, "icmpsge", 2) \
    f(ICMPSLT, "icmpslt", 2) \
    f(ICMPSLE, "icmpsle", 2)

#define IR_FLOAT_CMP_OP_LIST(f) \
    f(FCMPEQ,  "fcmpeq",  2) \
    f(FCMPNE,  "fcmpne",  2) \
    f(FCMPORD, "fcmpord", 2) \
    f(FCMPUNO, "fcmpuno", 2) \
    f(FCMPUGT, "fcmpugt", 2) \
    f(FCMPUGE, "fcmpuge", 2) \
    f(FCMPULT, "fcmpult", 2) \
    f(FCMPULE, "fcmpule", 2) \
    f(FCMPOGT, "fcmpogt", 2) \
    f(FCMPOGE, "fcmpoge", 2) \
    f(FCMPOLT, "fcmpolt", 2) \
    f(FCMPOLE, "fcmpole", 2)

#define IR_CMP_OP_LIST(f) \
    IR_INT_CMP_OP_LIST(f) \
    IR_FLOAT_CMP_OP_LIST(f)

#define IR_BIT_OP_LIST(f) \
    f(AND, "and", 2) \
    f(OR,  "or",  2) \
    f(XOR, "xor", 2)

#define IR_INT_CAST_OP_LIST(f) \
    f(ITOF, "itof", 1) \
    f(UTOF, "utof", 1)

#define IR_FLOAT_CAST_OP_LIST(f) \
    f(FTOU, "ftou", 1) \
    f(FTOI, "ftoi", 1)

#define IR_CAST_OP_LIST(f) \
    IR_INT_CAST_OP_LIST(f) \
    IR_FLOAT_CAST_OP_LIST(f) \
    f(BITCAST, "bitcast", 1)

#define IR_INT_OP_LIST(f) \
    IR_INT_ARITH_OP_LIST(f) \
    IR_INT_CMP_OP_LIST(f) \
    IR_INT_CAST_OP_LIST(f) \
    IR_BIT_OP_LIST(f)

#define IR_FLOAT_OP_LIST(f) \
    IR_FLOAT_ARITH_OP_LIST(f) \
    IR_FLOAT_CMP_OP_LIST(f) \
    IR_FLOAT_CAST_OP_LIST(f)

#define IR_VEC_OP_LIST(f) \
    f(LOAD,    "load",    2) \
    f(STORE,   "store",   3) \
    f(EXTRACT, "extract", 2) \
    f(INSERT,  "insert",  3) \
    IR_ARITH_OP_LIST(f) \
    IR_CMP_OP_LIST(f) \
    IR_CAST_OP_LIST(f) \
    IR_BIT_OP_LIST(f)

#define IR_FUNC_OP_LIST(f) \
    f(FUNC,   "func",   1) \
    f(CALL,   "call",   2) \
    f(CALLC,  "callc",  3) \
    f(RETURN, "return", 1) \
    f(PARAM,  "param",  1)

#define IR_SCALAR_OP_LIST(f) \
    f(UNDEF,     "undef",     0) \
    f(ANY,       "any",       1) \
    f(ALL,       "all",       1) \
    f(LET,       "let",       N) \
    f(BROADCAST, "broadcast", 1) \
    f(PTRTOVEC,  "ptrtovec",  1) \
    f(ALLOC,     "alloc",     2) \
    f(ARRAY,     "array",     N) \
    f(TUPLE,     "tuple",     N) \
    IR_FUNC_OP_LIST(f)

#define IR_VAL_LIST(f) \
    IR_SCALAR_OP_LIST(f) \
    IR_VEC_OP_LIST(f)

#define IR_NODE_LIST(f) \
    f(ERROR, "error", 0) \
    f(CONST, "const", 0) \
    f(VAR,   "var",   N) \
    IR_KIND_LIST(f) \
    IR_TYPE_LIST(f) \
    IR_VAL_LIST(f)

#endif
