#include "lang/lexer.h"
#include "lang/token.h"
#include "core/hash.h"
#include "core/utils.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <inttypes.h>

typedef struct keyword {
    const char* name;
    size_t len;
    TokenTag tag;
} Keyword;

static bool compare_keywords(const void* left, const void* right) {
    return
        ((Keyword*)left)->len == ((Keyword*)right)->len &&
        !memcmp(((Keyword*)left)->name, ((Keyword*)right)->name, ((Keyword*)left)->len);
}

static void register_keywords(HashTable* hash_table) {
#define f(name, str) \
    insert_in_hash_table( \
        hash_table, \
        &(Keyword) { str, strlen(str), TOKEN_##name }, \
        hash_str(hash_init(), str), \
        sizeof(Keyword), \
        compare_keywords);
    KEYWORD_LIST(f)
#undef f
}

Lexer new_lexer(const char* file_name, const char* file_data, size_t file_size, Log* log) {
    enum {
#define f(name, str) KEYWORD_##name,
        KEYWORD_LIST(f)
#undef f
        KEYWORD_COUNT
    };
    Lexer lexer = {
        .log = log,
        .file_name = file_name,
        .file_data = file_data,
        .file_size = file_size,
        .file_pos = { .row = 1, .col = 1 },
        .keywords = new_hash_table(KEYWORD_COUNT, sizeof(Keyword))
    };
    register_keywords(&lexer.keywords);
    return lexer;
}

void free_lexer(Lexer* lexer) {
    free_hash_table(&lexer->keywords);
}

static bool is_eof_reached(const Lexer* lexer) {
    return lexer->file_size == lexer->file_pos.byte_offset;
}

static const char* get_cur_ptr(const Lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return &lexer->file_data[lexer->file_pos.byte_offset];
}

static char get_cur_char(const Lexer* lexer) {
    return *get_cur_ptr(lexer);
}

static void skip_char(Lexer* lexer) {
    if (get_cur_char(lexer) == '\n') {
        lexer->file_pos.row++;
        lexer->file_pos.col = 1;
    } else
        lexer->file_pos.col++;
    lexer->file_pos.byte_offset++;
}

static bool accept_char(Lexer* lexer, char c) {
    assert(!is_eof_reached(lexer));
    if (get_cur_char(lexer) == c) {
        skip_char(lexer);
        return true;
    }
    return false;
}

static void skip_spaces(Lexer* lexer) {
    while (!is_eof_reached(lexer) && isspace(get_cur_char(lexer)))
        skip_char(lexer);
}

static void skip_single_line_comment(Lexer* lexer) {
    while (!is_eof_reached(lexer) && get_cur_char(lexer) != '\n')
        skip_char(lexer);
}

static void skip_multi_line_comment(Lexer* lexer) {
    while (!is_eof_reached(lexer)) {
        while (accept_char(lexer, '*')) {
            if (accept_char(lexer, '/'))
                return;
        }
        skip_char(lexer);
    }
}

static Token make_token(Lexer* lexer, const FilePos* begin, enum token_tag tag) {
    return (Token) {
        .tag = tag,
        .file_loc = {
            .file_name = lexer->file_name,
            .begin = *begin,
            .end = lexer->file_pos
        },
    };
}

static Token make_int_literal(Lexer* lexer, const FilePos* begin, uintmax_t int_val) {
    Token token = make_token(lexer, begin, TOKEN_INT_LITERAL);
    token.int_val = int_val;
    return token;
}

static Token make_float_literal(Lexer* lexer, const FilePos* begin, double float_val) {
    Token token = make_token(lexer, begin, TOKEN_FLOAT_LITERAL);
    token.float_val = float_val;
    return token;
}

static Token make_invalid_token(Lexer* lexer, const FilePos* begin, const char* err_msg) {
    Token token = make_token(lexer, begin, TOKEN_ERROR);
    log_error(lexer->log, &token.file_loc, err_msg, NULL);
    return token;
}

Token advance_lexer(Lexer* lexer) {
    while (true) {
        skip_spaces(lexer);
        FilePos begin = lexer->file_pos;

        if (is_eof_reached(lexer))
            return make_token(lexer, &begin, TOKEN_EOF);

        if (accept_char(lexer, '(')) return make_token(lexer, &begin, TOKEN_L_PAREN);
        if (accept_char(lexer, ')')) return make_token(lexer, &begin, TOKEN_R_PAREN);
        if (accept_char(lexer, '[')) return make_token(lexer, &begin, TOKEN_L_BRACKET);
        if (accept_char(lexer, ']')) return make_token(lexer, &begin, TOKEN_R_BRACKET);
        if (accept_char(lexer, '{')) return make_token(lexer, &begin, TOKEN_L_BRACE);
        if (accept_char(lexer, '}')) return make_token(lexer, &begin, TOKEN_R_BRACE);

        if (accept_char(lexer, '/')) {
            if (accept_char(lexer, '/')) {
                skip_single_line_comment(lexer);
                continue;
            } else if (accept_char(lexer, '*')) {
                skip_multi_line_comment(lexer);
                continue;
            }
            return make_token(lexer, &begin, TOKEN_SLASH);
        }

        if (accept_char(lexer, '\"')) {
            while (get_cur_char(lexer) != '\"') {
                if (accept_char(lexer, '\n')) {
                    // Backslash to continue string on another line
                    if (accept_char(lexer, '\\'))
                        continue;
                    break;
                }
                skip_char(lexer);
            }
            if (!accept_char(lexer, '\"'))
                return make_invalid_token(lexer, &begin, "unterminated string literal");
            return make_token(lexer, &begin, TOKEN_STRING_LITERAL);
        }

        if (accept_char(lexer, '\'')) {
            const char* ptr = get_cur_ptr(lexer);
            while (get_cur_char(lexer) != '\'' && get_cur_char(lexer) != '\n')
                skip_char(lexer);
            Token token = make_token(lexer, &begin, TOKEN_CHAR_LITERAL);
            if (!accept_char(lexer, '\'') || !convert_escape_seq(ptr, get_cur_ptr(lexer) - ptr, &token.char_val))
                return make_invalid_token(lexer, &begin, "invalid character literal");
            return token;
        }

        if (accept_char(lexer, '<')) {
            if (accept_char(lexer, '='))
                return make_token(lexer, &begin, TOKEN_LESS_EQUAL);
            return make_token(lexer, &begin, TOKEN_LESS);
        }

        if (get_cur_char(lexer) == '_' || isalpha(get_cur_char(lexer))) {
            skip_char(lexer);
            while (get_cur_char(lexer) == '_' || isalnum(get_cur_char(lexer)))
                skip_char(lexer);
            const char* name = lexer->file_data + begin.byte_offset;
            size_t len = lexer->file_pos.byte_offset - begin.byte_offset;
            Keyword* keyword = find_in_hash_table(
                &lexer->keywords,
                &(Keyword) { .name = name, .len = len },
                hash_raw_bytes(hash_init(), name, len),
                sizeof(Keyword),
                compare_keywords);
            return keyword
                ? make_token(lexer, &begin, keyword->tag)
                : make_token(lexer, &begin, TOKEN_IDENT);
        }

        if (isdigit(get_cur_char(lexer))) {
            bool was_zero = get_cur_char(lexer) == '0';
            const char* ptr = get_cur_ptr(lexer);

            skip_char(lexer);
            if (was_zero) {
                if (accept_char(lexer, 'b')) {
                    // Binary literal
                    ptr = get_cur_ptr(lexer);
                    while (get_cur_char(lexer) == '0' || get_cur_char(lexer) == '1')
                        skip_char(lexer);
                    return make_int_literal(lexer, &begin, strtoumax(ptr, NULL, 2));
                } else if (accept_char(lexer, 'x')) {
                    // Hexadecimal literal
                    ptr = get_cur_ptr(lexer);
                    while (isxdigit(get_cur_char(lexer)))
                        skip_char(lexer);
                    return make_int_literal(lexer, &begin, strtoumax(ptr, NULL, 16));
                } else {
                    // Octal literal
                    ptr = get_cur_ptr(lexer);
                    while (get_cur_char(lexer) >= '0' && get_cur_char(lexer) <= '7')
                        skip_char(lexer);
                    return make_int_literal(lexer, &begin, strtoumax(ptr, NULL, 8));
                }
            }

            // Parse integral part
            while (isdigit(get_cur_char(lexer)))
                skip_char(lexer);

            bool has_dot = false;
            if (accept_char(lexer, '.')) {
                has_dot = true;
                // Parse fractional part
                while (isdigit(get_cur_char(lexer)))
                    skip_char(lexer);
                // Parse exponent
                if (accept_char(lexer, 'e')) {
                    // Accept `+`/`-` signs
                    if (!accept_char(lexer, '+'))
                        accept_char(lexer, '-');
                    while (isdigit(get_cur_char(lexer)))
                        skip_char(lexer);
                }
            }

            return has_dot
                ? make_float_literal(lexer, &begin, strtod(ptr, NULL))
                : make_int_literal(lexer, &begin, strtoumax(ptr, NULL, 10));
        }

        skip_char(lexer);
        return make_invalid_token(lexer, &begin, "invalid token");
    }
}
