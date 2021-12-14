#include "lang/lexer.h"
#include "lang/token.h"
#include "core/hash.h"

#include <assert.h>
#include <ctype.h>
#include <string.h>

struct keyword {
    const char* name;
    size_t len;
    enum token_tag tag;
};

static bool compare_keywords(const void* left, const void* right) {
    return
        ((struct keyword*)left)->len == ((struct keyword*)right)->len &&
        !memcmp(((struct keyword*)left)->name, ((struct keyword*)right)->name, ((struct keyword*)left)->len);
}

static void register_keywords(struct hash_table* hash_table) {
#define f(name, str) \
    insert_in_hash_table( \
        hash_table, \
        &(struct keyword) { str, strlen(str), TOKEN_##name }, \
        hash_string(hash_init(), str), \
        sizeof(struct keyword), \
        compare_keywords);
    KEYWORD_LIST(f)
#undef f
}

struct lexer new_lexer(const char* file_name, const char* file_data, size_t file_size, struct log* log) {
    enum {
#define f(name, str) KEYWORD_##name,
        KEYWORD_LIST(f)
#undef f
        KEYWORD_COUNT
    };
    struct lexer lexer = {
        .log = log,
        .file_name = file_name,
        .file_data = file_data,
        .file_size = file_size,
        .file_pos = { .row = 1, .col = 1 },
        .keywords = new_hash_table(KEYWORD_COUNT, sizeof(struct keyword))
    };
    register_keywords(&lexer.keywords);
    return lexer;
}

void free_lexer(struct lexer* lexer) {
    free_hash_table(&lexer->keywords);
}

static bool is_eof_reached(const struct lexer* lexer) {
    return lexer->file_size == lexer->file_pos.byte_offset;
}

static char get_current_char(const struct lexer* lexer) {
    assert(!is_eof_reached(lexer));
    return lexer->file_data[lexer->file_pos.byte_offset];
}

static void skip_char(struct lexer* lexer) {
    if (get_current_char(lexer) == '\n') {
        lexer->file_pos.row++;
        lexer->file_pos.col = 1;
    } else
        lexer->file_pos.col++;
    lexer->file_pos.byte_offset++;
}

static bool accept_char(struct lexer* lexer, char c) {
    assert(!is_eof_reached(lexer));
    if (get_current_char(lexer) == c) {
        skip_char(lexer);
        return true;
    }
    return false;
}

static void skip_spaces(struct lexer* lexer) {
    while (!is_eof_reached(lexer) && isspace(get_current_char(lexer)))
        skip_char(lexer);
}

static struct token make_token(struct lexer* lexer, struct file_pos* begin, enum token_tag tag) {
    return (struct token) {
        .tag = tag,
        .file_loc = {
            .file_name = lexer->file_name,
            .begin = *begin,
            .end = lexer->file_pos
        },
    };
}

struct token advance_lexer(struct lexer* lexer) {
    skip_spaces(lexer);

    struct file_pos begin = lexer->file_pos;
    if (is_eof_reached(lexer))
        return make_token(lexer, &begin, TOKEN_EOF);

    if (accept_char(lexer, '(')) return make_token(lexer, &begin, TOKEN_L_PAREN);
    if (accept_char(lexer, ')')) return make_token(lexer, &begin, TOKEN_R_PAREN);
    if (accept_char(lexer, '<')) {
        if (accept_char(lexer, '='))
            return make_token(lexer, &begin, TOKEN_LESS_EQUAL);
        return make_token(lexer, &begin, TOKEN_LESS);
    }

    if (get_current_char(lexer) == '_' || isalpha(get_current_char(lexer))) {
        skip_char(lexer);
        while (get_current_char(lexer) == '_' || isalnum(get_current_char(lexer)))
            skip_char(lexer);
        const char* name = lexer->file_data + begin.byte_offset;
        size_t len = lexer->file_pos.byte_offset - begin.byte_offset;
        struct keyword* keyword = find_in_hash_table(
            &lexer->keywords,
            &(struct keyword) { .name = name, .len = len },
            hash_raw_bytes(hash_init(), name, len),
            sizeof(struct keyword),
            compare_keywords);
        if (keyword)
            printf("%s\n", token_to_string(keyword->tag));
        return keyword
            ? make_token(lexer, &begin, keyword->tag)
            : make_token(lexer, &begin, TOKEN_IDENT);
    }

    skip_char(lexer);
    struct token error_token = make_token(lexer, &begin, TOKEN_ERROR);
    log_error(lexer->log, &error_token.file_loc, "unknown token", NULL);
    return error_token;
}
