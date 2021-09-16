#include "core/hash_table.h"
#include "core/hash.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/string_pool.h"
#include "core/utils.h"
#include "ir/module.h"
#include "ir/node.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define DEFAULT_MODULE_CAPACITY 256

struct ir_module {
    struct hash_table nodes;
    struct hash_table debug;
    struct mem_pool mem_pool;
    struct string_pool* string_pool;
};

struct ir_module* new_ir_module(struct string_pool* string_pool) {
    struct ir_module* module = malloc_or_die(sizeof(struct ir_module));
    module->nodes    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct ir_node*));
    module->debug    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct debug_info*));
    module->mem_pool = new_mem_pool();
    module->string_pool = string_pool;
    return module;
}

void free_ir_module(struct ir_module* module) {
    free_hash_table(&module->nodes);
    free_hash_table(&module->debug);
    free_mem_pool(&module->mem_pool);
    free(module);
}

static bool compare_ir_nodes(const void* left, const void* right) {
    return is_same_node(*(struct ir_node**)left, *(struct ir_node**)right);
}

static const struct ir_node* insert_ir_node(struct ir_module* module, const struct ir_node* node) {
    assert(node);
    uint32_t hash = hash_ir_node(node);
    struct ir_node** node_ptr =
        find_in_hash_table(&module->nodes, &node, hash, sizeof(struct ir_node*), compare_ir_nodes);
    if (node_ptr)
        return *node_ptr;
    struct ir_node* new_node = alloc_from_mem_pool(&module->mem_pool, sizeof(struct ir_node) + sizeof(struct ir_node*) * node->op_count);
    memcpy(new_node, node, sizeof(struct ir_node) + sizeof(struct ir_node*) * node->op_count);
    must_succeed(insert_in_hash_table(&module->nodes, &new_node, hash, sizeof(struct ir_node*), compare_ir_nodes));
    return new_node;
}

static inline uint32_t hash_debug_info(const struct debug_info* debug_info) {
    uint32_t h = hash_init();
    h = hash_pointer(h, debug_info->name);
    if (debug_info->loc.file_name) {
        h = hash_pointer(h, debug_info->loc.file_name);
        h = hash_uint32(h, debug_info->loc.begin.row);
        h = hash_uint32(h, debug_info->loc.begin.col);
        h = hash_uint32(h, debug_info->loc.end.row);
        h = hash_uint32(h, debug_info->loc.end.col);
    }
    return h;
}

static bool compare_debug_info(const void* left, const void* right) {
    return !memcmp(*(void**)left, *(void**)right, sizeof(struct debug_info));
}

static const struct debug_info* insert_debug_info(struct ir_module* module, const struct debug_info* debug) {
    if (!debug)
        return NULL;
    uint32_t hash = hash_debug_info(debug);
    struct debug_info** debug_ptr =
        find_in_hash_table(&module->debug, &debug, hash, sizeof(struct debug_info*), compare_debug_info);
    if (debug_ptr)
        return *debug_ptr;
    struct debug_info* new_debug = alloc_from_mem_pool(&module->mem_pool, sizeof(struct debug_info));
    memcpy(new_debug, debug, sizeof(struct debug_info));
    must_succeed(insert_in_hash_table(&module->debug, &new_debug, hash, sizeof(struct debug_info*), compare_debug_info));
    return new_debug;
}

const struct debug_info* make_debug_info(struct ir_module* module, const struct file_loc* loc, const char* name) {
    return insert_debug_info(module, &(struct debug_info) {
        .loc = {
            .file_name = make_unique_string(module->string_pool, loc->file_name),
            .begin = loc->begin,
            .end = loc->end
        },
        .name = make_unique_string(module->string_pool, name)
    });    
}

const struct ir_node* make_var(struct ir_module* module, size_t var_index, const struct debug_info* debug) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_VAR,
        .debug = debug,
        .data_size = sizeof(size_t),
        .data.var_index = var_index
    });
}
