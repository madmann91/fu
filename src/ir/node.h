#ifndef FU_IR_NODE_H
#define FU_IR_NODE_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#define IR_NODE_TAGS(f) \
    f(VAR) \
    f(LIT) \
    f(LET) \
    f(APP) \
    f(ABS) \
    f(MATCH) \
    f(INS) \
    f(EXT) \
    f(TUP) \
    f(OPT) \
    f(PTR) \
    f(INT) \
    f(FLOAT) \
    f(PROD) \
    f(SUM) \
    f(PI) \
    f(STAR) \
    f(NAT)

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
    union ir_node_data {
        uint64_t int_val;
        double   float_val;
    } data[];
};

uint32_t hash_ir_node(const struct ir_node*);
bool are_ir_nodes_equal(const struct ir_node*, const struct ir_node*);
bool is_valid_pattern(const struct ir_node*);

#endif
