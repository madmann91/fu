#ifndef FU_CORE_FORMAT_H
#define FU_CORE_FORMAT_H

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

struct format_buf {
    size_t size;
    size_t capacity;
    struct format_buf* next;
    char data[];
};

struct format_style {
    enum {
        STYLE_NORMAL = 0,
        STYLE_BOLD,
        STYLE_DIM,
        STYLE_UNDERLINE,
        STYLE_ITALIC
    } style;
    enum {
        COLOR_NORMAL = 0,
        COLOR_RED,
        COLOR_GREEN,
        COLOR_BLUE,
        COLOR_CYAN,
        COLOR_MAGENTA,
        COLOR_YELLOW,
        COLOR_WHITE
    } color;
};

union format_arg {
    struct format_style style;
    bool b;
    const void* p;
    const char* s;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
};

struct format_state {
    struct format_buf* buf;
    bool ignore_style;
    size_t indent;
    const char* tab;
};

void format(struct format_state* state, const char* format_str, const union format_arg* args);

void print_format_bufs(const struct format_buf*, FILE*);
void free_format_bufs(struct format_buf*);

#endif
