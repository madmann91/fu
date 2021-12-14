#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "core/alloc.h"
#include "lang/lexer.h"

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

static bool compile_file(const char* file_name) {
    size_t file_size = 0;
    char* file_data = read_file(file_name, &file_size);
    struct log log = { .state = { .tab = "    " } };
    struct lexer lexer = new_lexer(file_name, file_data, file_size, &log);
    while (advance_lexer(&lexer).tag != TOKEN_EOF) ;
    free_lexer(&lexer);
    print_format_bufs(log.state.first_buf, stdout);
    free_format_bufs(log.state.first_buf);
    return log.error_count == 0;
}

int main(int argc, char** argv) {
    return compile_file(argv[1]) ? 0 : 1;
}
