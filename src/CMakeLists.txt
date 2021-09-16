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
    driver/main.c)
set_target_properties(fu PROPERTIES C_STANDARD 11)
target_include_directories(fu PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(fu PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
