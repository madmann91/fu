#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdint.h>

#include "fu/core/file_loc.h"
#include "fu/core/format.h"

#define NODE_LIST(f) \
    f(FREE_PARAMS,  N, "free-params") \
    f(MERGE_PARAMS, 2, "merge-params") \
    f(LABEL,        0, "label") \
    f(UNIVERSE,     0, "universe") \
    f(STAR,         0, "star") \
    f(SINGLETON,    1, "singleton") \
    f(NAT,          0, "nat") \
    f(INT,          1, "int") \
    f(FLOAT,        1, "float") \
    f(CONST,        0, "const") \
    f(SIGMA,        N, "sigma") \
    f(VARIANT,      N, "variant") \
    f(PI,           2, "pi") \
    f(APP,          2, "app") \
    f(PROJ,         2, "proj") \
    f(LAMBDA,       1, "lambda") \
    f(TUPLE,        N, "tuple") \
    f(PARAM,        1, "param") \
    f(AXIOM,        0, "axiom")

typedef struct Node Node;
typedef struct Module Module;

typedef struct DebugInfo {
    const char* name;
    void* user_data;
    FileLoc file_loc;
} DebugInfo;

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
typedef size_t Uid;

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

typedef union {
    FloatMode float_mode;
    const char* label;
    IntVal int_val;
    FloatVal float_val;
    Module* module;
} NodeData;

#define NODE_CONTENTS \
    NodeTag tag; \
    bool is_nominal : 1; \
    bool is_dead : 1; \
    unsigned level : 3; \
    const Node* free_params; \
    NodeData data; \
    Uid id; \
    const Node* type; \
    const DebugInfo* debug_info; \
    const User* users; \
    size_t op_count;

struct Node {
    NODE_CONTENTS
    const Node* ops[];
};

Module* get_module(const Node*);
Node* cast_nominal_node(const Node*);

size_t get_max_op_count(NodeTag);
size_t get_min_op_count(NodeTag);
const char* get_op_name(NodeTag);

bool is_int_const(const Node*);
bool is_nat_const(const Node*);
bool is_int_or_nat_const(const Node*);
bool is_float_const(const Node*);
FloatVal get_float_const_value(const Node*);
IntVal get_int_or_nat_const_value(const Node*);
const char* get_label_name(const Node*);

size_t find_label_index(const Node**, size_t, const char*);

const Node* get_pi_dom(const Node*);
const Node* get_pi_codom(const Node*);
const Node* get_proj_type(const Node* sigma, size_t);
const Node* get_app_type(const Node* pi, const Node* arg);

void print_node(FormatState*, const Node*);

#ifndef NDEBUG
void dump_node(const Node*);
#endif

#endif
