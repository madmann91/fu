#ifndef FU_CORE_FORMAT_H
#define FU_CORE_FORMAT_H

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct FormatBuf {
    size_t size;
    size_t capacity;
    struct FormatBuf* next;
    char data[];
} FormatBuf;

typedef struct {
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
} FormatStyle;

typedef struct Type Type;

typedef union {
    FormatStyle style;
    const Type* t;
    bool b;
    const void* p;
    const char* s;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    uintmax_t um;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
    float f32;
    double f64;
    size_t len;
} FormatArg;

typedef struct {
    FormatBuf* cur_buf;
    FormatBuf* first_buf;
    bool ignore_style;
    size_t indent;
    const char* tab;
} FormatState;

static const FormatStyle reset_style    = { STYLE_NORMAL, COLOR_NORMAL };
static const FormatStyle error_style    = { STYLE_BOLD,   COLOR_RED };
static const FormatStyle literal_style  = { STYLE_NORMAL, COLOR_MAGENTA };
static const FormatStyle keyword_style  = { STYLE_BOLD,   COLOR_BLUE };
static const FormatStyle comment_style  = { STYLE_NORMAL, COLOR_GREEN };
static const FormatStyle ellipsis_style = { STYLE_NORMAL, COLOR_WHITE };
static const FormatStyle loc_style      = { STYLE_BOLD,   COLOR_WHITE };

FormatState new_format_state(const char* tab, bool ignore_style);
void free_format_state(FormatState*);

void format(FormatState*, const char* format_str, const FormatArg* args);

void print_with_style(FormatState*, const char*, FormatStyle);
void print_keyword(FormatState*, const char*);

void write_format_state(FormatState*, FILE*);

#endif
