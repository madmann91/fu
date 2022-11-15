#ifndef FU_CORE_FILE_LOC_H
#define FU_CORE_FILE_LOC_H

#include "fu/core/hash.h"

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

typedef struct {
    uint32_t row, col;
    size_t byte_offset;
} FilePos;

typedef struct FileLoc {
    const char* file_name;
    FilePos begin, end;
} FileLoc;

HashCode hash_file_pos(HashCode, const FilePos*);
HashCode hash_file_loc(HashCode, const FileLoc*);
bool compare_file_pos(const FilePos*, const FilePos*);
bool compare_file_loc(const FileLoc*, const FileLoc*);

#endif
