#include "fu/driver/options.h"
#include "fu/core/log.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

static void usage() {
    printf(
        "Fu -- a FUnctional language (ver. %s)\n"
        "usage: fu [options] files...\n"
        "options:\n"
        "  -h    --help           Shows this message\n"
        "        --print-ast      Prints the AST on the standard output\n"
        "        --no-type-check  Disables type checking\n"
        "        --no-color       Disables colored output\n"
        "        --max-errors     Sets the maximum number of errors\n",
        FU_VERSION);
}

static inline bool check_option_arg(int i, int argc, char** argv, Log* log) {
    if (i + 1 >= argc) {
        log_error(log, NULL, "missing argument for option '{s}'", (FormatArg[]) { { .s = argv[i] } });
        return false;
    }
    return true;
}

bool parse_options(int* argc, char** argv, Options* options, Log* log) {
    bool status = true;
    int file_count = 0;
    for (int i = 1, n = *argc; i < n; ++i) {
        if (argv[i][0] != '-') {
            argv[++file_count] = argv[i];
            continue;
        }
        if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
            usage();
            goto error;
        } else if (!strcmp(argv[i], "--no-color"))
            log->state->ignore_style = true;
        else if (!strcmp(argv[i], "--no-type-check"))
            options->no_type_check = true;
        else if (!strcmp(argv[i], "--print-ast"))
            options->print_ast = true;
        else if (!strcmp(argv[i], "--max-errors")) {
            if (!check_option_arg(i, n, argv, log))
                goto error;
            log->max_errors = strtoull(argv[++i], NULL, 10);
        } else {
            log_error(log, NULL, "invalid option '{s}'", (FormatArg[]) { { .s = argv[i] } });
            goto error;
        }
    }
    if (file_count == 0) {
        log_error(log, NULL, "no input file, run with '--help' to display usage", NULL);
        goto error;
    }
    goto exit;

error:
    status = false;
exit:
    *argc = file_count + 1;
    return status;
}
