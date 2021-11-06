add_executable(fu
    core/alloc.c
    core/hash.c
    core/format.c
    core/hash_table.c
    core/mem_pool.c
    core/string_pool.c
    core/log.c
    ir/node.c
    ir/print.c
    ir/parse.c
    ir/module.c
    ir/error_mgr.c
    ir/transform/simplify.c
    driver/main.c)
set_target_properties(fu PROPERTIES C_STANDARD 11 C_EXTENSIONS OFF)
target_include_directories(fu PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(fu PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")

include(CheckLibraryExists)
check_library_exists(m sin "" HAS_LIBM)
if (HAS_LIBM)
    target_link_libraries(fu m)
endif ()
