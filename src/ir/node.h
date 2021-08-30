#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#include "core/log.h"

#define IR_NODE_TAGS(f) \
    f(VAR) \
    f(LITERAL) \
    f(LET) \
    f(APPLY) \
    f(FUN) \
    f(MATCH) \
    f(INSERT) \
    f(EXTRACT) \
    f(TUPLE) \
    f(OPTION) \
    f(POINTER) \
    f(INTEGER) \
    f(FLOAT) \
    f(PRODUCT) \
    f(SUM) \
    f(PI) \
    f(STAR) \
    f(NAT) \
    f(ERROR)

struct debug_info {
    struct file_loc loc;
    const char* name;
};

enum ir_node_tag {
#define f(t) IR_NODE_##t,   
    IR_NODE_TAGS(f)
#undef f
};

union ir_node_data {
    uint64_t int_val;
    double float_val;
    size_t var_index;
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

uint32_t hash_ir_node(const struct ir_node*);
bool is_same_node(const struct ir_node*, const struct ir_node*);
bool is_valid_pattern(const struct ir_node*);

struct let_binding {
    const struct ir_node* var;
    const struct ir_node* val;
};

size_t let_bindings_count(const struct ir_node*);
struct let_binding let_binding(const struct ir_node*, size_t);
const struct ir_node* let_body(const struct ir_node*);

#endif
