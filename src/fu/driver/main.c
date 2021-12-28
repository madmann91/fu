#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "fu/core/alloc.h"
#include "fu/core/mem_pool.h"
#include "fu/lang/lexer.h"
#include "fu/lang/parser.h"

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
    Log log = { .state = { .tab = "    " } };
    MemPool mem_pool = new_mem_pool();
    Lexer lexer = new_lexer(file_name, file_data, file_size, &log);
    Parser parser = make_parser(&lexer, &mem_pool);
    AstNode* program = parse_program(&parser);
    if (program) {
        dump_ast(program);
        free_lexer(&lexer);
    }
    free_mem_pool(&mem_pool);
    print_format_bufs(log.state.first_buf, stderr);
    free_format_bufs(log.state.first_buf);
    return log.error_count == 0;
}

int main(int argc, char** argv) {
    return compile_file(argv[1]) ? 0 : 1;
}
