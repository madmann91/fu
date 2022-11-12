#ifndef FU_CORE_LOG_H
#define FU_CORE_LOG_H

#include <stdint.h>

#include "fu/core/format.h"
#include "fu/core/hash_table.h"

/*
 * The log object is used to report messages from various passes of the compiler.
 * It also caches source files, so as to print error diagnostics efficiently.
 */

typedef struct {
    uint32_t row, col;
    size_t byte_offset;
} FilePos;

typedef struct {
    const char* file_name;
    FilePos begin, end;
} FileLoc;

typedef struct Log {
    HashTable file_cache;
    FormatState* state;
    size_t error_count;
    size_t warning_count;
    size_t max_errors;
    bool show_diagnostics;
} Log;

HashCode hash_file_pos(HashCode, const FilePos*);
HashCode hash_file_loc(HashCode, const FileLoc*);

Log new_log(FormatState*);
void free_log(Log*);

void log_error(Log*, const FileLoc*, const char*, const FormatArg*);
void log_warning(Log*, const FileLoc*, const char*, const FormatArg*);
void log_note(Log*, const FileLoc*, const char*, const FormatArg*);

#endif
