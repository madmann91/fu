#ifndef FU_DRIVER_DRIVER_H
#define FU_DRIVER_DRIVER_H

#include <stdbool.h>

typedef struct Log Log;
typedef struct Options Options;

bool compile_file(const char* file_name, const Options* options, Log* log);

#endif
