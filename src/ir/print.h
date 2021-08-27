#ifndef FU_IR_PRINT_H
#define FU_IR_PRINT_H

#include "core/format.h"

struct ir_node;

struct ir_printer {
    struct format_state format_state;
};

void print_ir(struct ir_printer*, const struct ir_node*);

#endif
