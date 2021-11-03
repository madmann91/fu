#ifndef FU_ERROR_MGR_H
#define FU_ERROR_MGR_H

#include "ir/node.h"

struct error_mgr {
    void (*invalid_type)(struct error_mgr*, ir_type_t, ir_type_t, const struct debug_info*);
    void (*invalid_kind)(struct error_mgr*, ir_kind_t, ir_kind_t, const struct debug_info*);
    void (*unexpected_node)(struct error_mgr*, ir_node_t, const char*, const struct debug_info*);
    void (*invalid_op_count)(struct error_mgr*, enum ir_node_tag, size_t, size_t, const struct debug_info*);
};

struct default_error_mgr {
    struct error_mgr error_mgr;
    struct log* log;
};

struct default_error_mgr get_default_error_mgr(struct log*);

#endif
