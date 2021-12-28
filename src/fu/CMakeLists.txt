add_executable(fu
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
    lang/ast.c
    driver/main.c)

target_include_directories(fu PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..)
set_target_properties(fu PROPERTIES
    C_STANDARD 11
    C_EXTENSIONS OFF
    INTERPROCEDURAL_OPTIMIZATION_RELEASE ON
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

if (NOT WIN32)
    target_compile_definitions(fu PUBLIC -D_POSIX_C_SOURCE)
endif ()

include(CheckLibraryExists)
check_library_exists(m sin "" HAS_LIBM)
if (HAS_LIBM)
    target_link_libraries(fu m)
endif ()
