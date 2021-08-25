add_executable(fu
    core/alloc.c
    core/hash.c
    core/format.c
    ir/node.c
    driver/main.c)
set_target_properties(fu PROPERTIES C_STANDARD 99)
target_include_directories(fu PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set_target_properties(fu PROPERTIES RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
