#include "fu/lang/parser.h"

#include <string.h>
#include <assert.h>

Parser make_parser(Lexer* lexer, MemPool* mem_pool) {
    Parser parser = {
        .lexer = lexer,
        .mem_pool = mem_pool,
        .prev_end = { .row = 1, .col = 1 }
    };
    for (size_t i = 0; i < LOOK_AHEAD; ++i)
        parser.ahead[i] = advance_lexer(lexer);
    return parser;
}

static inline void skip_token(Parser* parser) {
    for (size_t i = 0; i < LOOK_AHEAD - 1; ++i)
        parser->ahead[i] = parser->ahead[i + 1];
    parser->ahead[LOOK_AHEAD - 1] = advance_lexer(parser->lexer);
}

static inline void eat_token(Parser* parser, TokenTag tag) {
    assert(parser->ahead->tag == tag);
    skip_token(parser);
    (void)tag;
}

static inline bool accept_token(Parser* parser, TokenTag tag) {
    if (parser->ahead->tag == tag) {
        skip_token(parser);
        return true;
    }
    return false;
}

static AstNode* parse_many(Parser* parser, TokenTag end, TokenTag sep, AstNode* (*parse_one)(Parser*)) {
    AstNode* first = NULL, *last = NULL;
    while (true) {
        if (end != TOKEN_ERROR && parser->ahead->tag == end)
            break;
        AstNode* node = parse_one(parser);
        if (!first)
            last = first = node;
        else
            last->next = node;
        if (sep != TOKEN_ERROR && !accept_token(parser, sep))
            break;
    }
    return first;
}

static inline FileLoc make_file_loc(Parser* parser, FilePos* begin) {
    return (FileLoc) {
        .file_name = parser->lexer->file_name,
        .begin = *begin,
        .end = parser->prev_end
    };
}

static inline AstNode* make_ast_node(Parser* parser, FilePos* begin, const AstNode* node) {
    AstNode* copy = alloc_from_mem_pool(parser->mem_pool, sizeof(AstNode));
    memcpy(copy, node, sizeof(AstNode));
    copy->file_loc = make_file_loc(parser, begin);
    return copy;
}

static inline AstNode* parse_error(Parser* parser, const char* msg) {
    FilePos begin = parser->ahead->file_loc.begin;
    TokenTag token_tag = parser->ahead->tag;
    skip_token(parser);
    FileLoc file_loc = make_file_loc(parser, &begin);
    log_error(parser->lexer->log, &file_loc,
        is_special_token(token_tag) ? "expected {s}, but got {s}" : "expected {s}, but got '{s}'",
        (FormatArg[]) { { .s = msg }, { .s = token_tag_to_str(token_tag) } });
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_ERROR });
}

static inline AstNode* parse_fun(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    eat_token(parser, TOKEN_FUN);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_FUN_DECL });
}

AstNode* parse_decl(Parser* parser) {
    switch (parser->ahead->tag) {
        case TOKEN_FUN:
            return parse_fun(parser);
        default:
            return parse_error(parser, "declaration");
    }
}

AstNode* parse_program(Parser* parser) {
    FilePos begin = parser->ahead->file_loc.begin;
    AstNode* decls = parse_many(parser, TOKEN_EOF, TOKEN_ERROR, parse_decl);
    return make_ast_node(parser, &begin, &(AstNode) { .tag = AST_PROGRAM, .program.decls = decls });
}
