#include <assert.h>

#include "ir/print.h"
#include "ir/node.h"
#include "core/format.h"

static const struct format_style error_style   = { .style = STYLE_BOLD, .color = COLOR_RED  };
static const struct format_style keyword_style = { .style = STYLE_BOLD, .color = COLOR_BLUE };

void print_ir(struct format_state* state, const struct ir_node* node) {
    // TODO
    (void)state;
    switch (node->tag) {
        case IR_NODE_LET:
            format(state, "{$}let{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            for (size_t i = 0, n = let_bindings_count(node); i < n; ++i) {
                struct let_binding binding = let_binding(node, i);
                print_ir(state, binding.var);
                format(state, " = ", NULL);
                print_ir(state, binding.val);
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, " {$}in{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            print_ir(state, let_body(node));
            break;
        case IR_NODE_VAR:
            format(state, "{s}_{u64}", (union format_arg[]) {
                { .s = node->debug && node->debug->name ? node->debug->name : "" },
                { .u64 = node->data.var_index }
            });
            break;
        case IR_NODE_ERROR:
            format(state, "{$}<error>{$}", (union format_arg[]) { { .style = error_style }, { .style = reset_style } });
            break;
        default:
            assert(false && "unsupported node tag");
            break;
    }
}
