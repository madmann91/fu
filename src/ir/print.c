#include <assert.h>

#include "ir/print.h"
#include "ir/node.h"
#include "core/format.h"

void print_ir(struct format_state* state, const struct ir_node* node) {
    // TODO
    (void)state;
    switch (node->tag) {
        case IR_NODE_VAR:
            format(state, "{s}_{u64}", (union format_arg[]) {
                { .s = node->debug && node->debug->name ? node->debug->name : "" },
                { .u64 = node->data.var_index }
            });
            break;
        default:
            assert(false && "unsupported node tag");
            break;
    }
}
