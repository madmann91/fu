#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "core/log.h"

struct options {
    unsigned opt_level;
    size_t file_count;
};

static struct log global_log = { .state = { .tab = "    " } };

static void usage() {
    printf(
        "Usage: fu [options] files...\n"
        "Available options:\n"
        "   -h    --help           Shows this message and exits the program\n"
        "   -On   --opt-level <n>  Sets the optimization level (where n = 0, 1, 2, or 3)\n");
}

static bool check_option(int i, int argc, char** argv) {
    if (i + 1 >= argc) {
        log_error(&global_log, NULL, "missing argument for option '%s'", (union format_arg[]) { { .s = argv[i] } });
        return false;
    }
    return true;
}

static bool parse_options(int argc, char** argv, struct options* options) {
    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-') {
            if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "--help")) {
                usage();
                return false;
            } else if (!strcmp(argv[i], "-O0")) {
                options->opt_level = 0;
            } else if (!strcmp(argv[i], "-O1")) {
                options->opt_level = 1;
            } else if (!strcmp(argv[i], "-O2")) {
                options->opt_level = 2;
            } else if (!strcmp(argv[i], "-O3")) {
                options->opt_level = 3;
            } else if (!strcmp(argv[i], "--opt-level")) {
                if (!check_option(i, argc, argv))
                    return false;
                options->opt_level = strtoul(argv[++i], NULL, 10);
                if (options->opt_level > 3) {
                    log_error(&global_log, NULL, "invalid optimization level '%s'", (union format_arg[]) { { .s = argv[i] } });
                    return false;
                }
            } else {
                log_error(&global_log, NULL, "unknown option '%s'", (union format_arg[]) { { .s = argv[i] } });
                return false;
            }
        } else {
            options->file_count++;
        }
    }
    if (options->file_count == 0) {
        log_error(&global_log, NULL, "no input files", NULL);
        return false;
    }
    return true;
}

static bool compile_file(const char* file_name, const struct options* options) {
    // TODO
    (void)file_name, (void)options;
    return true;
}

int main(int argc, char** argv) {
    int status = EXIT_SUCCESS;

    struct options options = {
        .file_count = 0,
        .opt_level  = 0    
    };

    if (!parse_options(argc, argv, &options))
        goto failure;

    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-')
            continue;
        if (!compile_file(argv[i], &options))
            goto failure;
    }
    goto success;

failure:
    status = EXIT_FAILURE;
success:
    print_format_bufs(global_log.state.first_buf, stdout);
    free_format_bufs(global_log.state.first_buf);
    return status;
}
