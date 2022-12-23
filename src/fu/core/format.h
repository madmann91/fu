#ifndef FU_CORE_FORMAT_H
#define FU_CORE_FORMAT_H

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

/*
 * Custom formatting library that allows pretty-printing custom types.
 *
 * Format strings are not based on `printf()` strings, but instead represent arguments with braces.
 * Inside braces, either a type (e.g. `{i32}`) or an integer followed by a colon and a type is given
 * (e.g. `{0:i32}`). When the integer is present, it refers to the index of the argument in the
 * argument list. When the integer is not present, the formatting routine used an internal index
 * that is incremented every time an argument is encountered.
 *
 * Terminal colors and styles are supported by using the `$` sign as a type for an argument, and can
 * be turned on/off by changing the value of `ignore_style` in the `FormatState` structure. When
 * that member is set to `true`, style arguments are ignored.
 *
 * Finally, indentation is also supported, and can be changed from the formatting string by using
 * `{>}` and `{<}`, or by changing the value of `indent` in the `FormatState` structure directly.
 * Each indentation level is printed as `indent` times the string in `tab`.
 *
 * Custom formatting functions can be registered via `register_format_fn()`, and use the `p` member
 * of the `FormatArg` union.
 */

typedef struct FormatState FormatState;
typedef void (*FormatFn)(FormatState*, const void*);

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

typedef union {
    FormatStyle style;
    bool b;
    const void* p;
    const char* s;
    uint8_t u8;
    uint16_t u16;
    uint32_t u32;
    uint64_t u64;
    uintmax_t u;
    int8_t i8;
    int16_t i16;
    int32_t i32;
    int64_t i64;
    intmax_t i;
    float f32;
    double f64;
    size_t len;
} FormatArg;

struct FormatState {
    FormatBuf* cur_buf;
    FormatBuf* first_buf;
    bool ignore_style;
    size_t indent;
    const char* tab;
};

static const FormatStyle reset_style    = { STYLE_NORMAL, COLOR_NORMAL };
static const FormatStyle error_style    = { STYLE_BOLD,   COLOR_RED };
static const FormatStyle literal_style  = { STYLE_NORMAL, COLOR_MAGENTA };
static const FormatStyle keyword_style  = { STYLE_BOLD,   COLOR_BLUE };
static const FormatStyle comment_style  = { STYLE_NORMAL, COLOR_GREEN };
static const FormatStyle ellipsis_style = { STYLE_NORMAL, COLOR_WHITE };
static const FormatStyle loc_style      = { STYLE_BOLD,   COLOR_WHITE };

FormatState new_format_state(const char* tab, bool ignore_style);

void reset_format_state(FormatState*);
void free_format_state(FormatState*);

void format(FormatState*, const char* format_str, const FormatArg* args);

void print_with_style(FormatState*, const char*, FormatStyle);
void print_keyword(FormatState*, const char*);

void write_format_state(FormatState*, FILE*);

void register_format_fn(char c, FormatFn);

#endif
