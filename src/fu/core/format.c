#include "fu/core/alloc.h"
#include "fu/core/format.h"

#include <assert.h>
#include <stddef.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

#define MAX_FORMAT_CHARS 64
#define DEFAULT_BUF_CAPACITY 1024

static FormatBuf* alloc_buf(size_t capacity) {
    FormatBuf* buf = malloc_or_die(sizeof(FormatBuf) + capacity);
    buf->size = 0;
    buf->capacity = capacity;
    buf->next = NULL;
    return buf;
}

static char* reserve_buf(FormatState* state, size_t size) {
    if (state->cur_buf && state->cur_buf->size + size <= state->cur_buf->capacity)
        return state->cur_buf->data + state->cur_buf->size;
    size_t capacity = state->cur_buf ? state->cur_buf->size : DEFAULT_BUF_CAPACITY;
    if (capacity < size)
        capacity = size;
    FormatBuf* buf = alloc_buf(capacity);
    if (state->cur_buf)
        state->cur_buf->next = buf;
    else
        state->first_buf = buf;
    state->cur_buf = buf;
    return buf->data;
}

static void advance_buf(FormatBuf* buf, size_t inc) {
    assert(buf->capacity >= buf->size + inc);
    buf->size += inc;
}

static void write(FormatState* state, const char* ptr, size_t size) {
    memcpy(reserve_buf(state, size), ptr, size);
    advance_buf(state->cur_buf, size);
}

static void write_char(FormatState* state, char c) {
    write(state, &c, 1);
}

static void write_str(FormatState* state, const char* s) {
    write(state, s, strlen(s));
}

static const char* format_arg(FormatState* state, const char* ptr, size_t* index, const FormatArg* args) {
    const FormatArg* arg = &args[(*index)++];
    char* buf_ptr = reserve_buf(state, MAX_FORMAT_CHARS);
    size_t chars_printed = 0;
    char c = *(ptr++);
    switch (c) {
        case 'u':
            switch (*(ptr++)) {
                case '8': chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIu8, arg->u8);  break;
                case 'm': chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIuMAX, arg->um); break;
                case '1': assert(*ptr == '6'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIu16, arg->u16); break;
                case '3': assert(*ptr == '2'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIu32, arg->u32); break;
                case '6': assert(*ptr == '4'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIu64, arg->u64); break;
                default: assert(false && "invalid unsigned integer format string");
            }
            break;
        case 'i':
            switch (*(ptr++)) {
                case '8': chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIi8, arg->i8);  break;
                case '1': assert(*ptr == '6'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIi16, arg->i16); break;
                case '3': assert(*ptr == '2'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIi32, arg->i32); break;
                case '6': assert(*ptr == '4'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%"PRIi64, arg->i64); break;
                default: assert(false && "invalid signed integer format string");
            }
            break;
        case 'f':
            switch (*(ptr++)) {
                case '3': assert(*ptr == '2'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%f", arg->f32); break;
                case '6': assert(*ptr == '4'); ptr++; chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%g", arg->f64); break;
                default: assert(false && "invalid floating-point format string");
            }
            break;
        case 's':
            if (*ptr == 'l') {
                ptr++;
                write(state, arg->s, args[(*index)++].len);
            } else
                write_str(state, arg->s);
            break;
        case 'b':
            write_str(state, arg->b ? "true" : "false");
            break;
        case 'p':
            chars_printed = snprintf(buf_ptr, MAX_FORMAT_CHARS, "%p", arg->p);
            break;
        default:
            assert(false && "unknown formatting command");
            break;
    }
    assert(chars_printed < MAX_FORMAT_CHARS);
    advance_buf(state->cur_buf, chars_printed);
    return ptr;
}

static void apply_style(FormatState* state, const FormatArg* arg) {
    if (arg->style.style == STYLE_NORMAL || arg->style.color == COLOR_NORMAL) {
        write_str(state, "\33[0m");
        if (arg->style.style == STYLE_NORMAL && arg->style.color == COLOR_NORMAL)
            return;
    }

    write_str(state, "\33[");
    switch (arg->style.style) {
        case STYLE_BOLD:      write_char(state, '1'); break;
        case STYLE_DIM:       write_char(state, '2'); break;
        case STYLE_UNDERLINE: write_char(state, '4'); break;
        case STYLE_ITALIC:    write_char(state, '3'); break;
        default: break;
    }
    if (arg->style.style != STYLE_NORMAL)
        write_char(state, ';');
    switch (arg->style.color) {
        case COLOR_RED:     write_str(state, "31"); break;
        case COLOR_GREEN:   write_str(state, "32"); break;
        case COLOR_BLUE:    write_str(state, "34"); break;
        case COLOR_CYAN:    write_str(state, "36"); break;
        case COLOR_MAGENTA: write_str(state, "35"); break;
        case COLOR_YELLOW:  write_str(state, "33"); break;
        case COLOR_WHITE:   write_str(state, "37"); break;
        default: break;
    }
    write_char(state, 'm');
}

FormatState new_format_state(const char* tab, bool ignore_style) {
    return (FormatState) {
        .tab = tab,
        .ignore_style = ignore_style
    };
}

void free_format_state(FormatState* state) {
    FormatBuf* next = NULL;
    for (FormatBuf* buf = state->first_buf; buf; buf = next) {
        next = buf->next;
        free(buf);
    }
}

void format(FormatState* state, const char* format_str, const FormatArg* args) {
    const char* ptr = format_str;
    size_t index = 0;
    while (*ptr) {
        const char* prev = ptr;
        ptr += strcspn(ptr, "\n{");
        write(state, prev, ptr - prev);
        if (*ptr == '\n') {
            ptr++;
            write_char(state, '\n');
            for (size_t i = 0, n = state->indent; i < n; ++i)
                write_str(state, state->tab);
        } else if (*ptr == '{') {
            switch (*(++ptr)) {
                case '{':
                    write_char(state, '{');
                    ptr++;
                    continue;
                case '>':
                    state->indent++;
                    ptr++;
                    break;
                case '<':
                    assert(state->indent > 0);
                    state->indent--;
                    ptr++;
                    break;
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    index = strtoull(ptr, (char**)&ptr, 10);
                    assert(*ptr == ':' && "positional separator expected");
                    ptr++;
                    // fallthrough
                default:
                    if (*ptr == '$') {
                        if (!state->ignore_style)
                            apply_style(state, &args[index]);
                        index++, ptr++;
                    } else
                        ptr = format_arg(state, ptr, &index, args);
                    break;
            }
            assert(*ptr == '}' && "argument end marker expected");
            ptr++;
        }
    }
}

void write_format_state(FormatState* state, FILE* file) {
    for (FormatBuf* buf = state->first_buf; buf; buf = buf->next)
        fwrite(buf->data, 1, buf->size, file);
}
