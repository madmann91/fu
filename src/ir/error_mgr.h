#ifndef FU_ERROR_MGR_H
#define FU_ERROR_MGR_H

#include "ir/node.h"

struct error_mgr {
    void (*invalid_type)(struct error_mgr*, ir_type_t, ir_type_t, const struct debug_info*);
    void (*unexpected_type)(struct error_mgr*, ir_type_t, const char*, const struct debug_info*);
};

struct default_error_mgr {
    struct error_mgr error_mgr;
    struct log* log;
};

struct default_error_mgr get_default_error_mgr(struct log*);

#endif
