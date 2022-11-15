#include "fu/core/file_loc.h"

#include <string.h>

HashCode hash_file_pos(HashCode hash, const FilePos* file_pos) {
    hash = hash_uint64(hash, file_pos->row);
    hash = hash_uint64(hash, file_pos->col);
    hash = hash_uint64(hash, file_pos->byte_offset);
    return hash;
}

HashCode hash_file_loc(HashCode hash, const FileLoc* file_loc) {
    hash = hash_str(hash, file_loc->file_name);
    hash = hash_file_pos(hash, &file_loc->begin);
    hash = hash_file_pos(hash, &file_loc->end);
    return hash;
}

bool compare_file_pos(const FilePos* left, const FilePos* right) {
    return
        left->row == right->row &&
        left->col == right->col &&
        left->byte_offset == right->byte_offset;
}

bool compare_file_loc(const FileLoc* left, const FileLoc* right) {
    return
        !strcmp(left->file_name, right->file_name) &&
        compare_file_pos(&left->begin, &right->begin) &&
        compare_file_pos(&left->end, &right->end);
}
