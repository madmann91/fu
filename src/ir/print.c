#include "ir/print.h"
#include "ir/node.h"
#include "core/format.h"

#include <assert.h>

static inline size_t decrease_depth(size_t depth) {
    assert(depth > 0);
    return depth == SIZE_MAX ? SIZE_MAX : depth - 1;
}

static void print_var_name(struct format_state* state, ir_node_t node) {
    assert(node->tag == IR_VAR);
    format(state, "{s}#{u64}", (union format_arg[]) {
        { .s = node->debug && node->debug->name ? node->debug->name : "" },
        { .u64 = node->data.var_index }
    });
}

void print_ir(struct format_state* state, ir_node_t node, size_t depth) {
    if (depth == 0) {
        format(state, "{$}...{$}", (union format_arg[]) { { .style = ellipsis_style }, { .style = reset_style } });
        return;
    }

    switch (node->tag) {
        case IR_VAL_LET:
            format(state, "{$}let{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            for (size_t i = 0, n = node->op_count - 1; i < n; ++i) {
                print_var_name(state, node->ops[i]);
                format(state, " : ", NULL);
                print_ir(state, node->ops[i]->type, depth);
                format(state, " = ", NULL);
                print_ir(state, node->ops[i]->ops[0], decrease_depth(depth));
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, depth > 1 ? " {$}in{$}{>}\n" : " {$}in{$}{>} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            print_ir(state, node->ops[node->op_count - 1], decrease_depth(depth));
            state->indent--;
            break;
        case IR_VAR:
            print_var_name(state, node);
            break;
        case IR_CONST:
            format(state, "{$}const{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            if (is_float_const(node)) {
                format(state, "{$}{f64}{$}",
                    (union format_arg[]) {
                        { .style = number_style },
                        { .f64 = get_float_const_val(to_val(node)) },
                        { .style = reset_style }
                    });
            } else {
                format(state, "{$}{u64}{$}",
                    (union format_arg[]) {
                        { .style = number_style },
                        { .u64 = is_int_or_nat_const(node) ? get_int_or_nat_const_val(node) : 0 },
                        { .style = reset_style }
                    });
            }
            if (!is_nat_const(node)) {
                format(state, " : ", NULL);
                print_ir(state, node->type, depth);
            }
            break;
        case IR_ERROR:
            format(state, "{$}error{$}", (union format_arg[]) { { .style = error_style }, { .style = reset_style } });
            break;
        default:
            format(state, "{$}{s}{$}",
                (union format_arg[]) {
                    { .style = keyword_style },
                    { .s = get_node_name(node->tag) },
                    { .style = reset_style },
                });
            if (node->op_count == 0)
                break;
            format(state, is_type(node) ? "[" : "(", NULL);
            for (size_t i = 0, n = node->op_count; i < n; ++i) {
                print_ir(state, node->ops[i], decrease_depth(depth));
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, is_type(node) ? "]" : ")", NULL);
            break;
    }
}

void dump_ir(ir_node_t node) {
    struct format_state state = { .tab = "    " };
    print_ir(&state, node, SIZE_MAX);
    print_format_bufs(state.first_buf, stdout);
    free_format_bufs(state.first_buf);
    printf("\n");
}
