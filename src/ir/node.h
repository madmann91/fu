#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

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
    f(NAT)

struct position {
    uint32_t row, col;
    const char* data_ptr;
};

struct location {
    const char* file_name;
    struct position begin, end;
};

struct ir_node {
    enum ir_node_tag {
#define f(t) IR_NODE_##t,   
        IR_NODE_TAGS(f)
#undef f
    } tag : 32;
    uint32_t data_size;
    const struct ir_node* type;
    const struct ir_node** ops;
    size_t op_count;
    struct location loc;
    union ir_node_data {
        uint64_t int_val;
        double   float_val;
    } data[];
};

uint32_t hash_ir_node(const struct ir_node*);
bool are_ir_nodes_equal(const struct ir_node*, const struct ir_node*);
bool is_valid_pattern(const struct ir_node*);

#endif
