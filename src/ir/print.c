#include <assert.h>

#include "ir/print.h"
#include "ir/node.h"
#include "core/format.h"

void print_ir(struct format_state* state, const struct ir_node* node) {
    // TODO
    (void)state;
    switch (node->tag) {
        default:
            assert(false && "unsupported node tag");
            break;
    }
}
