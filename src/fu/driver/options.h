#ifndef FU_DRIVER_OPTIONS_H
#define FU_DRIVER_OPTIONS_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct Log Log;

typedef struct Options {
    bool print_ast;
    bool no_color;
    bool no_type_check;
    size_t max_errors;
} Options;

static const Options default_options = {
    .print_ast     = false,
    .no_color      = false,
    .no_type_check = false,
    .max_errors    = SIZE_MAX
};

bool parse_options(int* argc, char** argv, Options* options, Log* log);

#endif
