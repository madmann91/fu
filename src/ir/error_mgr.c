#include "ir/error_mgr.h"
#include "ir/print.h"

static struct log* get_log(struct error_mgr* error_mgr) {
    return ((struct default_error_mgr*)error_mgr)->log;
}

static void format_ir(struct format_state* state, const void* p) {
    print_ir(state, p, is_type(p) ? SIZE_MAX : 1);
}

static void invalid_type(struct error_mgr* error_mgr, ir_type_t type, ir_type_t expected, const struct debug_info* debug) {
    if (type->tag != IR_ERROR && expected->tag != IR_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected type '{n}', but got '{n}'",
            (union format_arg[]) { { .p = expected }, { .p = type } });
    }
}

static void invalid_kind(struct error_mgr* error_mgr, ir_kind_t kind, ir_kind_t expected, const struct debug_info* debug) {
    if (kind->tag != IR_ERROR && expected->tag != IR_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected kind '{n}', but got '{n}'",
            (union format_arg[]) { { .p = expected }, { .p = kind } });
    }
}

static void unexpected_node(struct error_mgr* error_mgr, ir_node_t node, const char* msg, const struct debug_info* debug) {
    if (node->tag != IR_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected {s}, but got '{n}'",
            (union format_arg[]) { { .s = msg }, { .p = node } });
    }
}

static void invalid_op_count(struct error_mgr* error_mgr, enum ir_node_tag tag, size_t op_count, size_t expected_op_count, const struct debug_info* debug) {
    if (tag != IR_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected {u64} operands for '{$}{s}{$}', but got {u64}",
            (union format_arg[]) {
                { .u64 = expected_op_count },
                { .style = keyword_style },
                { .s = get_node_name(tag) },
                { .style = reset_style },
                { .u64 = op_count }
            });
    }
}

struct default_error_mgr get_default_error_mgr(struct log* log) {
    log->state.custom_format['n'] = format_ir;
    return (struct default_error_mgr) {
        .error_mgr = {
            .invalid_type = invalid_type,
            .invalid_kind = invalid_kind,
            .unexpected_node = unexpected_node,
            .invalid_op_count = invalid_op_count
        },
        .log = log
    };
}
