#ifndef FU_IR_PARSE_H
#define FU_IR_PARSE_H

struct ir_node;

struct ir_parser {
    const char* input_buf;
    size_t input_size;
    size_t input_pos;
};

struct ir_node* parse_ir(struct ir_parser*);

#endif
