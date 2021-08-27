#include <assert.h>

#include "ir/print.h"
#include "ir/node.h"

void print_ir(struct ir_printer* printer, const struct ir_node* node) {
    switch (node->tag) {
        default:
            assert(false && "unsupported node tag");
            break;
    }
}
