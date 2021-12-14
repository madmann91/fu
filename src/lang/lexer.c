#include "lang/lexer.h"
#include "core/utf8.h"
#include "core/hash.h"
#include "core/utils.h"

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <ctype.h>

struct keyword {
    const char* name;
    unsigned len;
    unsigned tag;
};

static bool compare_keywords(const void* left, const void* right) {
    return
        ((struct keyword*)left)->len == ((struct keyword*)right)->len &&
        !memcmp(((struct keyword*)left)->name, ((struct keyword*)right)->name, ((struct keyword*)left)->len);
}

static void register_keywords(struct lexer* lexer) {
#define f(x, str) \
    must_succeed(insert_in_hash_table( \
        &lexer->keywords, \
        &(struct keyword) { .name = str, .len = strlen(str), .tag = TOKEN_##x }, \
        hash_string(hash_init(), str), \
        sizeof(struct keyword), \
        compare_keywords));
    KEYWORDS(f)
#undef f
}

struct lexer new_lexer(const char* file_name, const char* file_data, size_t data_size, struct log* log) {
    enum {
#define f(x, str) KEYWORD_##x,
        KEYWORDS(f)
#undef f
        KEYWORD_COUNT
    };
    struct lexer lexer = {
        .log = log,
        .cur_pos = { .row = 1, .col = 1, .byte_offset = 0 },
        .keywords = new_hash_table(KEYWORD_COUNT, sizeof(struct keyword)),
        .file_name = file_name,
        .data = file_data,
        .data_size = data_size
    };
    register_keywords(&lexer);
    return lexer;
}

void free_lexer(struct lexer* lexer) {
    free_hash_table(&lexer->keywords);
}

bool is_eof_reached(const struct lexer* lexer) {
    return lexer->cur_pos.byte_offset >= lexer->data_size;
}

static inline char get_next_char(const struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return lexer->data[lexer->cur_pos.byte_offset];
}

static inline void skip_char(struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    if (is_utf8_multibyte_begin(get_next_char(lexer))) {
        size_t n = eat_utf8_bytes(
            (const uint8_t*)(lexer->data + lexer->cur_pos.byte_offset),
            (const uint8_t*)(lexer->data + lexer->data_size));
        lexer->cur_pos.byte_offset += n;
        lexer->cur_pos.col++;
    } else if (get_next_char(lexer) == '\n') {
        lexer->cur_pos.byte_offset++;
        lexer->cur_pos.col = 1;
        lexer->cur_pos.row++;
    } else {
        lexer->cur_pos.byte_offset++;
        lexer->cur_pos.col++;
    }
}

static inline bool accept_char(struct lexer* lexer, char c) {
    if (get_next_char(lexer) == c) {
        skip_char(lexer);
        return true;
    }
    return false;
}

static inline void skip_spaces(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && isspace(get_next_char(lexer)))
        skip_char(lexer);
}

static inline void skip_single_line_comment(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && get_next_char(lexer) != '\n')
        skip_char(lexer);
}

static inline void skip_multiline_comment(struct lexer* lexer, const struct file_pos* begin) {
    while (true) {
        while (!is_eof_reached(lexer) && get_next_char(lexer) != '*')
            skip_char(lexer);
        if (is_eof_reached(lexer)) {
            struct file_loc loc = {
                .file_name = lexer->file_name,
                .begin = *begin,
                .end = lexer->cur_pos
            };
            log_error(lexer->log, &loc, "unterminated multiline comment", NULL);
            break;
        }
        if (accept_char(lexer, '*') && accept_char(lexer, '/'))
            break;
    }
}

static struct token make_token(const struct lexer* lexer, const struct file_pos* begin, enum token_tag tag) {
    return (struct token) {
        .tag = tag,
        .loc = (struct file_loc) {
            .file_name = lexer->file_name,
            .begin = *begin,
            .end = lexer->cur_pos
        }
    };
}

struct token advance_lexer(struct lexer* lexer) {
    while (true) {
        skip_spaces(lexer);
        if (is_eof_reached(lexer))
            return make_token(lexer, &lexer->cur_pos, TOKEN_EOF);

        struct file_pos begin = lexer->cur_pos;

        if (accept_char(lexer, '/')) {
            if (accept_char(lexer, '/')) skip_single_line_comment(lexer);
            else if (accept_char(lexer, '*')) skip_multiline_comment(lexer, &begin);
            else return make_token(lexer, &begin, TOKEN_SLASH);
            continue;
        }

        if (accept_char(lexer, '.')) return make_token(lexer, &begin, TOKEN_DOT);
        if (accept_char(lexer, ':')) return make_token(lexer, &begin, TOKEN_COLON);
        if (accept_char(lexer, ';')) return make_token(lexer, &begin, TOKEN_SEMICOLON);
        if (accept_char(lexer, '#')) return make_token(lexer, &begin, TOKEN_HASH);
        if (accept_char(lexer, ',')) return make_token(lexer, &begin, TOKEN_COMMA);
        if (accept_char(lexer, '[')) return make_token(lexer, &begin, TOKEN_L_BRACKET);
        if (accept_char(lexer, ']')) return make_token(lexer, &begin, TOKEN_R_BRACKET);
        if (accept_char(lexer, '(')) return make_token(lexer, &begin, TOKEN_L_PAREN);
        if (accept_char(lexer, ')')) return make_token(lexer, &begin, TOKEN_R_PAREN);
        if (accept_char(lexer, '{')) return make_token(lexer, &begin, TOKEN_L_BRACE);
        if (accept_char(lexer, '}')) return make_token(lexer, &begin, TOKEN_R_BRACE);

        if (accept_char(lexer, '-')) {
            if (accept_char(lexer, '-'))
                return make_token(lexer, &begin, TOKEN_DOUBLE_MINUS);
            if (accept_char(lexer, '>'))
                return make_token(lexer, &begin, TOKEN_THIN_ARROW);
            if (accept_char(lexer, '='))
                return make_token(lexer, &begin, TOKEN_MINUS_EQUAL);
            return make_token(lexer, &begin, TOKEN_MINUS);
        }

        if (accept_char(lexer, '=')) {
            if (accept_char(lexer, '>'))
                return make_token(lexer, &begin, TOKEN_FAT_ARROW);
            return make_token(lexer, &begin, TOKEN_EQUAL);
        }

        if (get_next_char(lexer) == '_' || isalpha(get_next_char(lexer))) {
            skip_char(lexer);
            while (isalnum(get_next_char(lexer)) || get_next_char(lexer) == '_')
                skip_char(lexer);
            size_t len = lexer->cur_pos.byte_offset - begin.byte_offset;
            struct keyword* keyword = find_in_hash_table(
                &lexer->keywords,
                &(struct keyword) { .name = lexer->data + begin.byte_offset, .len = len },
                hash_raw_bytes(hash_init(), lexer->data + begin.byte_offset, len),
                sizeof(struct keyword), compare_keywords);
            return make_token(lexer, &begin, keyword ? keyword->tag : TOKEN_IDENT);
        }

        if (isdigit(get_next_char(lexer))) {
            while (isdigit(get_next_char(lexer)))
                skip_char(lexer);
            bool has_dot = false;
            if (accept_char(lexer, '.')) {
                has_dot = true;
                while (isdigit(get_next_char(lexer)))
                    skip_char(lexer);
            }
            if (accept_char(lexer, 'e')) {
                if (!accept_char(lexer, '+'))
                    accept_char(lexer, '-');
                while (isdigit(get_next_char(lexer)))
                    skip_char(lexer);
            }
            struct token token = make_token(lexer, &begin, TOKEN_INVALID);
            if (has_dot) {
                token.tag = TOKEN_FLOAT_LITERAL;
                token.float_val = strtod(lexer->data + begin.byte_offset, NULL);
            } else {
                token.tag = TOKEN_INT_LITERAL;
                token.int_val = strtoumax(lexer->data + begin.byte_offset, NULL, 10);
            }
            return token;
        }

        skip_char(lexer);
        struct file_loc loc = {
            .file_name = lexer->file_name,
            .begin = begin,
            .end = lexer->cur_pos
        };
        log_error(lexer->log, &loc, "unknown token '{sl}'", (union format_arg[]) {
            { .s = lexer->data + begin.byte_offset },
            { .len = lexer->cur_pos.byte_offset - begin.byte_offset }
        });
        return make_token(lexer, &begin, TOKEN_INVALID);
    }
}
