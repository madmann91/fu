#ifndef FU_IR_PARSE_H
#define FU_IR_PARSE_H

#include <stddef.h>

struct ir_node;
struct log;

struct ir_node* parse_ir(
    struct log* log,
    const char* data_ptr,
    size_t data_size,
    const char* file_name);

#endif
