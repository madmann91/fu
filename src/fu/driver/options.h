#ifndef FU_DRIVER_OPTIONS_H
#define FU_DRIVER_OPTIONS_H

#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

typedef struct Log Log;

typedef struct Options {
    bool print_ast;
    bool no_type_check;
} Options;

static const Options default_options = {
    .print_ast     = false,
    .no_type_check = false
};

/// Parse command-line options, and remove those parsed options from the
/// argument list. After parsing, `argc` and `argv` are modified to only
/// contain the arguments that were not parsed. 
bool parse_options(int* argc, char** argv, Options* options, Log* log);

#endif
