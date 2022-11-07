#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdint.h>

#include "fu/core/log.h"

#define KIND_LIST(f) \
    f(STAR,   0, "star") \
    f(SINGLE, 1, "single") \
    f(ARROW,  2, "arrow")

#define TYPE_LIST(f) \
    f(INT,        1, "int") \
    f(FLOAT,      1, "float") \
    f(NAT,        0, "nat") \
    f(NORET,      0, "noret") \
    f(ERR,        0, "err") \
    f(MEM,        0, "mem") \
    f(PTR,        0, "ptr") \
    f(SAFE_PTR,   1, "safe-ptr") \
    f(ARRAY_TYPE, 1, "array-type") \
    f(SIGMA,      N, "sigma") \
    f(PI,         2, "pi")

#define VALUE_LIST(f) \
    f(PARAM,    1, "param") \
    f(ARRAY,    N, "array") \
    f(CONST,    0, "const") \
    f(TUPLE,    N, "tuple") \
    f(LAMBDA,   1, "lambda") \
    f(INSERT,   3, "insert") \
    f(EXTRACT,  2, "extract") \
    f(APP,      2, "app") \
    f(BITCAST,  1, "bitcast") \
    f(ZERO_EXT, 1, "zero-ext") \
    f(SIGN_EXT, 1, "sign-ext") \
    f(START,    1, "start")

#define NODE_LIST(f) \
    f(FREE_PARAMS, N, "free-params") \
    f(UNIVERSE,    0, "universe") \
    f(BOTTOM,      1, "bottom") \
    f(TOP,         1, "top") \
    KIND_LIST(f) \
    TYPE_LIST(f) \
    VALUE_LIST(f)

typedef struct Node Node;
typedef struct Module Module;

typedef struct Debug {
    const char* name;
    void* meta_data;
    FileLoc file_loc;
} Debug;

typedef struct User {
    const Node* node;
    size_t index;
    const struct User* next;
} User;

typedef enum NodeTag {
#define node(name, ...) NODE_##name,
    NODE_LIST(node)
#undef node
} NodeTag;

typedef double FloatVal;
typedef uintmax_t IntVal;

typedef enum FloatMode {
    FLOAT_MODE_EXACT_INVERSES  = 0x1,
    FLOAT_MODE_ASSOCIATIVE     = 0x2,
    FLOAT_MODE_COMMUTATIVE     = 0x4,
    FLOAT_MODE_NO_NANS         = 0x8,
    FLOAT_MODE_NO_SIGNED_ZEROS = 0x10,
    FLOAT_MODE_FINITE_ONLY     = 0x20,
    FLOAT_MODE_STRICT = 0,
    FLOAT_MODE_FAST = 
        FLOAT_MODE_EXACT_INVERSES |
        FLOAT_MODE_ASSOCIATIVE |
        FLOAT_MODE_COMMUTATIVE |
        FLOAT_MODE_NO_NANS |
        FLOAT_MODE_NO_SIGNED_ZEROS |
        FLOAT_MODE_FINITE_ONLY
} FloatMode;

struct Node {
    NodeTag tag;
    bool is_nominal : 1;
    const Node* free_params;
    union {
        FloatMode float_mode;
        struct{
            union {
                IntVal int_val;
                FloatVal float_val;
            };
        } const_;
        struct {
            Module* module;
        } universe;
    };
    uint64_t id;
    const Node* type;
    const Debug* debug;
    const User* users;
    size_t op_count;
    const Node* ops[];
};

Module* get_module(const Node*);
Node* cast_nominal_node(const Node*);

const char* get_node_tag_name(NodeTag);
bool is_kind_node_tag(NodeTag);
bool is_type_node_tag(NodeTag);
bool is_value_node_tag(NodeTag);

bool is_int_const(const Node*);
bool is_nat_const(const Node*);
bool is_int_or_nat_const(const Node*);
bool is_float_const(const Node*);
FloatVal get_float_const_value(const Node*);
IntVal get_int_or_nat_const_value(const Node*);

const Node* get_array_type_or_sigma_elem(const Node*, size_t);
const Node* get_tuple_or_array_elem(const Node*, size_t);

const Node* get_pi_dom(const Node*);
const Node* get_pi_codom(const Node*);

const Node* get_insert_or_extract_value(const Node*);
const Node* get_insert_or_extract_index(const Node*);
const Node* get_insert_elem(const Node*);

void print_node(FormatState*, const Node*);

#ifndef NDEBUG
void dump_node(const Node*);
#endif

#endif
