#ifndef FU_CORE_LOG_H
#define FU_CORE_LOG_H

#include <stdint.h>

#include "fu/core/format.h"

typedef struct {
    uint32_t row, col;
    size_t byte_offset;
} FilePos;

typedef struct {
    const char* file_name;
    FilePos begin, end;
} FileLoc;

typedef struct Log {
    FormatState state;
    size_t error_count;
    size_t warning_count;
} Log;

void log_error(Log*, const FileLoc*, const char*, const FormatArg*);
void log_warning(Log*, const FileLoc*, const char*, const FormatArg*);
void log_note(Log*, const FileLoc*, const char*, const FormatArg*);

#endif
