#include <assert.h>
#include <stddef.h>
#include <inttypes.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "core/alloc.h"
#include "core/format.h"

#define MAX_CHARS 64
#define DEFAULT_BUF_CAPACITY 1024

static struct format_buf* alloc_buf(size_t capacity) {
    struct format_buf* buf = malloc_or_die(sizeof(struct format_buf) + capacity);
    buf->size = 0;
    buf->capacity = capacity;
    buf->next = NULL;
    return buf;
}

static char* reserve_buf(struct format_state* state, size_t size) {
    if (state->buf && state->buf->size + size <= state->buf->capacity)
        return state->buf->data + state->buf->size;
    size_t capacity = state->buf ? state->buf->size : DEFAULT_BUF_CAPACITY;
    if (capacity < size)
        capacity = size;
    struct format_buf* buf = alloc_buf(capacity);
    if (state->buf)
        state->buf->next = buf;
    state->buf = buf;
    return buf->data;
}

static void advance_buf(struct format_buf* buf, size_t inc) {
    assert(buf->capacity >= buf->size + inc);
    buf->size += inc;
}

static void write(struct format_state* state, const char* ptr, size_t size) {
    memcpy(reserve_buf(state, size), ptr, size);
    advance_buf(state->buf, size);
}

static void write_char(struct format_state* state, char c) {
    write(state, &c, 1);
}

static void write_string(struct format_state* state, const char* s) {
    write(state, s, strlen(s));
}

static const char* format_arg(struct format_state* state, const char* ptr, const union format_arg* arg) {
    char* buf_ptr = reserve_buf(state, MAX_CHARS);
    size_t chars_printed = 0;
    switch (*(ptr++)) {
        case 'u':
            switch (*(ptr++)) {
                case '8': chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIu8, arg->u8);  break;
                case '1': assert(*ptr == '6'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIu16, arg->u16); break;
                case '3': assert(*ptr == '2'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIu32, arg->u32); break;
                case '6': assert(*ptr == '4'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIu64, arg->u64); break;
            } 
            break;
        case 'i':
            switch (*(ptr++)) {
                case '8': chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIi8, arg->i8);  break;
                case '1': assert(*ptr == '6'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIi16, arg->i16); break;
                case '3': assert(*ptr == '2'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIi32, arg->i32); break;
                case '6': assert(*ptr == '4'); ptr++; chars_printed = snprintf(buf_ptr, MAX_CHARS, "%"PRIi64, arg->i64); break;
            } 
            break;
        case 's':
            write_string(state, arg->s);
            break;
        case 'b':
            write_string(state, arg->b ? "true" : "false");
            break;
        case 'p':
            chars_printed = snprintf(buf_ptr, MAX_CHARS, "%p", arg->p);
            break;
        default:
            assert(false && "unknown format argument type");
            break;
    }
    advance_buf(state->buf, chars_printed);
    return ptr;
}

static void apply_style(struct format_state* state, const union format_arg* arg) {
    if (arg->style.style == STYLE_NORMAL || arg->style.color == COLOR_NORMAL) {
        write_string(state, "\33[0m");
        if (arg->style.style == STYLE_NORMAL && arg->style.color == COLOR_NORMAL)
            return;
    }

    write_string(state, "\33[");
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
        case COLOR_RED:     write_string(state, "31"); break;
        case COLOR_GREEN:   write_string(state, "32"); break;
        case COLOR_BLUE:    write_string(state, "34"); break;
        case COLOR_CYAN:    write_string(state, "36"); break;
        case COLOR_MAGENTA: write_string(state, "35"); break;
        case COLOR_YELLOW:  write_string(state, "33"); break;
        case COLOR_WHITE:   write_string(state, "37"); break;
        default: break;
    }
    write_char(state, 'm');
}

void format(struct format_state* state, const char* format_str, const union format_arg* args) {
    const char* ptr = format_str;
    size_t index = 0;
    while (*ptr) {
        const char* prev = ptr;
        ptr += strcspn(ptr, "\n%$");
        write(state, prev, ptr - prev);
        if (*ptr == '\n') {
            ptr++;
            write_char(state, '\n');
            for (size_t i = 0, n = state->indent; i < n; ++i)
                write_string(state, state->tab);
        } else if (*ptr == '$') {
            switch (*(++ptr)) {
                case '$':
                    write_char(state, '$');
                    ptr++;
                    break;
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
                    // fallthrough
                default:
                    if (!state->ignore_style)
                        apply_style(state, &args[index++]);
                    break;
            }
        } else if (*ptr == '%') {
            switch (*(++ptr)) {
                case '%':
                    write_char(state, '%');
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
                    assert(*ptr == '$' && "positional separator expected");
                    ptr++;
                    // fallthrough
                default:
                    ptr = format_arg(state, ptr, &args[index++]);
                    break;
            }
        }
    }
}

void print_bufs(const struct format_buf* buf, FILE* out) {
    while (buf) {
        fwrite(buf->data, 1, buf->size, out);
        buf = buf->next;
    }
}

void free_bufs(struct format_buf* buf) {
    while (buf) {
        struct format_buf* next = buf->next;
        free(buf);
        buf = next;
    }
}
