#ifndef FU_CORE_LOG_H
#define FU_CORE_LOG_H

#include <stdint.h>

#include "core/format.h"

struct file_pos {
    uint32_t row, col;
    const char* data_ptr;
};

struct file_loc {
    const char* file_name;
    struct file_pos begin, end;
};

struct format_buf;
union format_arg;

struct log {
    struct format_state state;
    size_t error_count;
    size_t warning_count;
};

void log_error  (struct log*, const struct file_loc*, const char*, const union format_arg*);
void log_warning(struct log*, const struct file_loc*, const char*, const union format_arg*);
void log_note   (struct log*, const struct file_loc*, const char*, const union format_arg*);

#endif
