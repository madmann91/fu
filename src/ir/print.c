#include "ir/print.h"
#include "ir/node.h"
#include "core/format.h"

#include <assert.h>

static const struct format_style keyword_style = { .style = STYLE_BOLD, .color = COLOR_BLUE };

static void print_var_name(struct format_state* state, ir_node_t node) {
    assert(node->tag == IR_NODE_VAR);
    format(state, "{s}#{u64}", (union format_arg[]) {
        { .s = node->debug && node->debug->name ? node->debug->name : "" },
        { .u64 = node->data.var_index }
    });
}

void print_ir(struct format_state* state, ir_node_t node) {
    switch (node->tag) {
        case IR_NODE_LET:
            format(state, "{$}let{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            for (size_t i = 0, n = node->op_count - 1; i < n; ++i) {
                print_var_name(state, node->ops[i]);
                format(state, " = ", NULL);
                print_ir(state, node->ops[i]->ops[0]);
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, " {$}in{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            print_ir(state, node->ops[node->op_count - 1]);
            break;
        case IR_NODE_VAR:
            print_var_name(state, node);
            break;
        default:
            assert(false && "unsupported node tag");
            break;
    }
}

void dump_ir(ir_node_t node) {
    struct format_state state = { .tab = "    " };
    print_ir(&state, node);
    print_format_bufs(state.first_buf, stdout);
    free_format_bufs(state.first_buf);
    printf("\n");
}
