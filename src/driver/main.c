#include "core/log.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/string_pool.h"
#include "ir/parse.h"
#include "ir/print.h"
#include "ir/module.h"

#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

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
        log_error(&global_log, NULL, "missing argument for option '{s}'", (union format_arg[]) { { .s = argv[i] } });
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
                    log_error(&global_log, NULL, "invalid optimization level '{s}'", (union format_arg[]) { { .s = argv[i] } });
                    return false;
                }
            } else {
                log_error(&global_log, NULL, "unknown option '{s}'", (union format_arg[]) { { .s = argv[i] } });
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

static char* read_file(const char* file_name, size_t* file_size) {
    FILE* file = fopen(file_name, "rb");
    if (!file)
        return NULL;

    size_t chunk_size = 4096;
    char* file_data = NULL;
    *file_size = 0;
    while (true) {
        if (ferror(file)) {
            fclose(file);
            free(file_data);
            return NULL;
        }
        file_data = realloc_or_die(file_data, *file_size + chunk_size);
        size_t read_count = fread(file_data + *file_size, 1, chunk_size, file);
        *file_size += read_count;
        if (read_count < chunk_size)
            break;
        chunk_size *= 2;
    }
    fclose(file);

    // Add terminator
    file_data = realloc_or_die(file_data, *file_size + 1);
    file_data[*file_size] = 0;
    return file_data;
}

static bool compile_file(struct ir_module* module, const char* file_name, const struct options* options) {
    size_t file_size = 0;
    char* file_data = read_file(file_name, &file_size);
    if (!file_data) {
        log_error(&global_log, NULL, "cannot read file '{s}'", (union format_arg[]) { { .s = file_name } });
        return false;
    }

    // TODO
    (void)options;
    struct mem_pool mem_pool = new_mem_pool();
    ir_node_t node = parse_ir(&global_log, module, &mem_pool, file_data, file_size, file_name);
    if (node)
        print_ir(&global_log.state, node);
    format(&global_log.state, "\n", NULL);
    free_mem_pool(&mem_pool);
    free(file_data);
    return true;
}

int main(int argc, char** argv) {
    int status = EXIT_SUCCESS;

    struct options options = {
        .file_count = 0,
        .opt_level  = 0
    };

    struct ir_module* module = new_ir_module();

    if (!parse_options(argc, argv, &options))
        goto failure;

    for (int i = 1; i < argc; ++i) {
        if (argv[i][0] == '-')
            continue;
        if (!compile_file(module, argv[i], &options))
            goto failure;
    }
    goto success;

failure:
    status = EXIT_FAILURE;
success:
    free_ir_module(module);
    print_format_bufs(global_log.state.first_buf, stdout);
    free_format_bufs(global_log.state.first_buf);
    return status;
}
