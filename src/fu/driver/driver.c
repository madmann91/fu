#include "fu/driver/driver.h"
#include "fu/driver/options.h"
#include "fu/lang/ast.h"
#include "fu/lang/lexer.h"
#include "fu/lang/parser.h"
#include "fu/lang/bind.h"
#include "fu/lang/check.h"
#include "fu/lang/types.h"
#include "fu/lang/type_table.h"
#include "fu/core/utils.h"
#include "fu/core/mem_pool.h"

static AstNode* parse_file(const char* file_name, MemPool* mem_pool, Log* log) {
    size_t file_size = 0;
    char* file_data = read_file(file_name, &file_size);
    if (!file_data) {
        log_error(log, NULL, "cannot open file '{s}'", (FormatArg[]) { { .s = file_name } });
        return NULL;
    }
    Lexer lexer = new_lexer(file_name, file_data, file_size, log);
    Parser parser = make_parser(&lexer, mem_pool);
    AstNode* program = parse_program(&parser);
    free_lexer(&lexer);
    free(file_data);
    return program;
}

bool compile_file(const char* file_name, const Options* options, Log* log) {
    MemPool mem_pool = new_mem_pool();
    AstNode* program = parse_file(file_name, &mem_pool, log);
    if (!program)
        return false;

    // Bind names to their declaration sites
    if (log->error_count == 0) {
        Env env = new_env(log);
        bind_program(&env, program);
        free_env(&env);
    }

    // Check types
    if (!options->no_type_check && log->error_count == 0) {
        TypeTable* type_table = new_type_table(&mem_pool);
        TypingContext context = new_typing_context(type_table, &mem_pool, log);
        infer_program(&context, program);
        free_typing_context(&context);
        free_type_table(type_table);
    }

    // Print the AST
    if (options->print_ast) {
        FormatState state = new_format_state("    ", options->no_color || !is_color_supported(stdout));
        print_ast(&state, program);
        write_format_state(&state, stdout);
        free_format_state(&state);
        printf("\n");
    }

    free_mem_pool(&mem_pool);
    return log->error_count == 0;
}

