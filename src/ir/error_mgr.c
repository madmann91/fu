#include "ir/error_mgr.h"
#include "ir/print.h"

static struct log* get_log(struct error_mgr* error_mgr) {
    return ((struct default_error_mgr*)error_mgr)->log;
}

static void format_ir(struct format_state* state, const void* p) {
    print_ir(state, p, 1);
}

static void invalid_type(struct error_mgr* error_mgr, ir_type_t type, ir_type_t expected, const struct debug_info* debug) {
    if (type->tag != IR_NODE_ERROR && expected->tag != IR_NODE_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected type '{n}', but got '{n}'",
            (union format_arg[]) { { .p = expected }, { .p = type } });
    }
}

static void unexpected_type(struct error_mgr* error_mgr, ir_type_t type, const char* msg, const struct debug_info* debug) {
    if (type->tag != IR_NODE_ERROR) {
        log_error(
            get_log(error_mgr), &debug->loc,
            "expected {s} type, but got '{n}'",
            (union format_arg[]) { { .s = msg }, { .p = type } });
    }
}

struct default_error_mgr get_default_error_mgr(struct log* log) {
    log->state.custom_format['n'] = format_ir;
    return (struct default_error_mgr) {
        .error_mgr = {
            .invalid_type = invalid_type,
            .unexpected_type = unexpected_type
        },
        .log = log
    };
}
