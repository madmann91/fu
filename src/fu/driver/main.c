#include "fu/core/utils.h"
#include "fu/core/alloc.h"
#include "fu/core/mem_pool.h"
#include "fu/lang/ast.h"
#include "fu/lang/lexer.h"
#include "fu/lang/parser.h"
#include "fu/lang/bind.h"

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef struct {
    bool print_ast;
    bool no_color;
} Options;

static void usage() {
    printf(
        "Fu -- a FUnctional language\n"
        "usage: fu [options] files...\n"
        "options:\n"
        "  -h    --help       Shows this message\n"
        "        --print-ast  Prints the AST on the standard output\n"
        "        --no-color   Disables colored output\n");
}

static bool parse_options(int argc, char** argv, Options* options, Log* log) {
    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] != '-')
            continue;
        if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
            usage();
            return false;
        } else if (!strcmp(argv[i], "--no-color"))
            options->no_color = true;
        else if (!strcmp(argv[i], "--print-ast"))
            options->print_ast = true;
        else {
            log_error(log, NULL, "invalid option '{s}'", (FormatArg[]) { { .s = argv[i] } });
            return false;
        }
    }
    return options;
}

static AstNode* parse_file(const char* file_name, MemPool* mem_pool, Log* log) {
    size_t file_size = 0;
    char* file_data = read_file(file_name, &file_size);
    if (!file_data) {
        log_error(log, NULL, "cannot open file '{s}'", (FormatArg[]) { { .s = file_name } });
        return NULL;
    }
    Lexer lexer = new_lexer(file_name, file_data, file_size, log);
    Parser parser = make_parser(&lexer, mem_pool);
    AstNode* program = parse_program(&parser);
    free_lexer(&lexer);
    free(file_data);
    return program;
}

static bool compile_file(const char* file_name, const Options* options, Log* log) {
    MemPool mem_pool = new_mem_pool();
    AstNode* program = parse_file(file_name, &mem_pool, log);
    if (!program)
        return false;
    if (options->print_ast) {
        FormatState state = {
            .tab = "    ",
            .ignore_style = options->no_color || !is_color_supported(stdout)
        };
        print_ast(&state, program);
        print_format_bufs(state.first_buf, stdout);
        free_format_bufs(state.first_buf);
        printf("\n");
    }
    if (log->error_count == 0) {
        Env env = new_env(log);
        bind_program(&env, program);
        free_env(&env);
        free_mem_pool(&mem_pool);
    }
    return log->error_count == 0;
}

int main(int argc, char** argv) {
    Log log = { .state = { .tab = "    ", .ignore_style = !is_color_supported(stderr) } };
    bool status = true;

    Options options = { 0 };
    if (!parse_options(argc, argv, &options, &log)) {
        status = false;
        goto exit;
    }

    log.state.ignore_style = options.no_color;

    for (int i = 1; i < argc && status; ++i) {
        if (argv[i][0] == '-')
            continue;
        status &= compile_file(argv[i], &options, &log);
    }

exit:
    print_format_bufs(log.state.first_buf, stderr);
    free_format_bufs(log.state.first_buf);
    return status ? EXIT_SUCCESS : EXIT_FAILURE;
}
