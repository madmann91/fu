add_library(libfu
    core/alloc.c
    core/hash.c
    core/format.c
    core/hash_table.c
    core/mem_pool.c
    core/str_pool.c
    core/log.c
    core/utils.c
    lang/lexer.c
    lang/parser.c
    lang/ast.c)

add_executable(fu driver/main.c)

target_include_directories(libfu PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..)
set_target_properties(fu libfu PROPERTIES
    C_STANDARD 11
    C_EXTENSIONS OFF
    PREFIX ""
    INTERPROCEDURAL_OPTIMIZATION_RELEASE ON
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

if (NOT WIN32)
    # For `isatty()` and `fileno()`
    target_compile_definitions(libfu PRIVATE -D_POSIX_C_SOURCE)
endif ()

include(CheckLibraryExists)
check_library_exists(m sin "" HAS_LIBM)
if (HAS_LIBM)
    target_link_libraries(libfu PUBLIC m)
endif ()

target_link_libraries(fu PUBLIC libfu)
