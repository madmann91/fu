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
    struct string_pool string_pool;

    ir_node_t nat, star;
};

struct ir_node_pair {
    ir_node_t fst;
    ir_node_t snd;
};

static ir_node_t insert_ir_node(struct ir_module*, ir_node_t);

struct ir_module* new_ir_module() {
    struct ir_module* module = malloc_or_die(sizeof(struct ir_module));
    module->nodes    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct ir_node_pair));
    module->debug    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct debug_info*));
    module->mem_pool = new_mem_pool();
    module->string_pool = new_string_pool();
    module->nat  = insert_ir_node(module, &(struct ir_node) { .tag = IR_KIND_NAT  });
    module->star = insert_ir_node(module, &(struct ir_node) { .tag = IR_KIND_STAR });
    return module;
}

void free_ir_module(struct ir_module* module) {
    free_hash_table(&module->nodes);
    free_hash_table(&module->debug);
    free_mem_pool(&module->mem_pool);
    free_string_pool(&module->string_pool);
    free(module);
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

static uint32_t hash_ir_node(ir_node_t node) {
    uint32_t h = hash_init();
    h = hash_uint32(h, node->tag);
    h = hash_raw_bytes(h, &node->data, node->data_size);
    for (size_t i = 0; i < node->op_count; ++i)
        h = hash_pointer(h, node->ops[i]);
    return h;
}

static bool is_same_ir_node(ir_node_t left, ir_node_t right) {
    if (left->tag != right->tag ||
        left->data_size != right->data_size ||
        left->op_count != right->op_count)
        return false;
    if (memcmp(&left->data, &right->data, left->data_size))
        return false;
    for (size_t i = 0, n = left->op_count; i < n; ++i) {
        if (left->ops[i] != right->ops[i])
            return false;
    }
    return true;
}

static bool compare_ir_nodes(const void* left, const void* right) {
    return is_same_ir_node(*(ir_node_t*)left, *(ir_node_t*)right);
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

static const struct debug_info* import_debug_info(struct ir_module* module, const struct debug_info* debug_info) {
    return debug_info ? insert_debug_info(module, &(struct debug_info) {
        .loc = {
            .file_name = make_string(&module->string_pool, debug_info->loc.file_name),
            .begin = debug_info->loc.begin,
            .end = debug_info->loc.end
        },
        .name = make_string(&module->string_pool, debug_info->name)
    }) : NULL;
}

// Impl. in `simplify.c`
ir_node_t simplify_ir_node(struct ir_module* module, ir_node_t node);

static ir_node_t insert_ir_node(struct ir_module* module, ir_node_t node) {
    assert(node);
    uint32_t hash = hash_ir_node(node);
    struct ir_node_pair* node_pair =
        find_in_hash_table(&module->nodes, &node, hash, sizeof(struct ir_node_pair), compare_ir_nodes);
    if (node_pair)
        return node_pair->snd;
    struct ir_node* new_node = alloc_from_mem_pool(&module->mem_pool, sizeof(struct ir_node) + sizeof(struct ir_node*) * node->op_count);
    memcpy(new_node, node, sizeof(struct ir_node) + sizeof(struct ir_node*) * node->op_count);
    new_node->debug = import_debug_info(module, node->debug);
    struct ir_node_pair new_node_pair = { .fst = new_node, .snd = simplify_ir_node(module, new_node) };
    must_succeed(insert_in_hash_table(&module->nodes, &new_node_pair, hash, sizeof(struct ir_node_pair), compare_ir_nodes));
    return new_node;
}

ir_node_t make_node(
    struct ir_module* module,
    unsigned tag,
    ir_type_t type,
    const ir_node_t* ops,
    size_t op_count,
    const union ir_node_data* data,
    const struct debug_info* debug)
{
    switch (tag) {
        case IR_NODE_VAR:
            return op_count == 1
                ? make_tied_var(module, type, data->var_index, ops[0], debug)
                : make_var(module, type, data->var_index, debug);
        case IR_NODE_LET:
            return make_let(module, ops, op_count - 1, ops[op_count - 1], debug);
        default:
            assert(false);
            return NULL;
    }
}

ir_node_t rebuild_node(struct ir_module* module, ir_node_t node, ir_type_t type, const ir_node_t* ops, const struct debug_info* debug) {
    return make_node(module, node->tag, type, ops, node->op_count, &node->data, debug);
}

#define IR_NODE_WITH_N_OPS(n) \
    (struct ir_node*) &(struct { IR_NODE_FIELDS ir_node_t ops[n]; })

ir_kind_t make_star(struct ir_module* module) {
    return module->star;
}

ir_kind_t make_nat(struct ir_module* module) {
    return module->nat;
}

ir_type_t make_int_type(struct ir_module* module, ir_type_t bitwidth) {
    assert(bitwidth->type->tag == IR_KIND_NAT);
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_TYPE_INT,
        .type = make_star(module),
        .ops = { bitwidth }
    });
}

ir_type_t make_intn_type(struct ir_module* module, ir_uint_t bitwidth) {
    return make_int_type(module, make_nat_const(module, bitwidth));
}

ir_type_t make_float_type(struct ir_module* module, ir_type_t bitwidth) {
    assert(bitwidth->type->tag == IR_KIND_NAT);
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_TYPE_FLOAT,
        .type = make_star(module),
        .ops = { bitwidth }
    });
}

ir_type_t make_floatn_type(struct ir_module* module, ir_uint_t bitwidth) {
    return make_float_type(module, make_nat_const(module, bitwidth));
}

ir_type_t make_tuple_type(struct ir_module* module, const ir_node_t* args, size_t arg_count, const struct debug_info* debug) {
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (arg_count + 1));
    memcpy(node->ops, args, sizeof(ir_node_t) * arg_count);
    node->tag = IR_TYPE_TUPLE;
    node->data_size = 0;
    node->type = make_star(module);
    node->op_count = arg_count;
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return inserted_node;
}

ir_node_t make_undef(struct ir_module* module, ir_type_t type) {
    return insert_ir_node(module, &(struct ir_node) { .tag = IR_NODE_CONST, .type = type });
}

ir_node_t make_const(struct ir_module* module, ir_node_t type, const union ir_node_data* data, size_t data_size) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_CONST,
        .type = type,
        .data = *data,
        .data_size = data_size
    });
}

ir_type_t make_nat_const(struct ir_module* module, ir_uint_t int_val) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_CONST,
        .type = make_nat(module),
        .data = { .int_val = int_val },
        .data_size = sizeof(ir_uint_t)
    });
}

ir_node_t make_int_const(struct ir_module* module, ir_type_t type, ir_uint_t int_val) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_CONST,
        .type = type,
        .data = { .int_val = int_val },
        .data_size = sizeof(ir_uint_t)
    });
}

ir_node_t make_float_const(struct ir_module* module, ir_type_t type, ir_float_t float_val) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_CONST,
        .type = type,
        .data = { .float_val = float_val },
        .data_size = sizeof(ir_float_t)
    });
}

ir_node_t make_var(struct ir_module* module, ir_type_t type, size_t var_index, const struct debug_info* debug) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_NODE_VAR,
        .debug = debug,
        .type = type,
        .data_size = sizeof(size_t),
        .data.var_index = var_index
    });
}

ir_node_t make_tied_var(struct ir_module* module, ir_type_t type, size_t var_index, ir_node_t value, const struct debug_info* debug) {
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_NODE_VAR,
        .debug = debug,
        .type = type,
        .data_size = sizeof(size_t),
        .data.var_index = var_index,
        .ops = { value },
        .op_count = 1
    });
}

ir_node_t make_let(struct ir_module* module, const ir_node_t* vars, size_t var_count, ir_node_t body, const struct debug_info* debug) {
#ifndef NDEBUG
    for (size_t i = 0; i < var_count; ++i)
        assert(is_tied_var(vars[i]));
#endif
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (var_count + 1));
    memcpy(node->ops, vars, sizeof(ir_node_t) * var_count);
    node->tag = IR_NODE_LET;
    node->data_size = 0;
    node->type = body->type;
    node->op_count = var_count + 1;
    node->ops[var_count] = body;
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return inserted_node;
}

static inline ir_type_t infer_tuple_type(struct ir_module* module, const ir_node_t* args, size_t arg_count, const struct debug_info* debug) {
    ir_node_t* arg_types = malloc_or_die(sizeof(ir_node_t) * arg_count);
    for (size_t i = 0; i < arg_count; ++i)
        arg_types[i] = args[i]->type;
    ir_node_t tuple_type = make_tuple_type(module, arg_types, arg_count, debug);
    free(arg_types);
    return tuple_type;
}

ir_node_t make_tuple(struct ir_module* module, const ir_node_t* args, size_t arg_count, const struct debug_info* debug) {
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (arg_count + 1));
    memcpy(node->ops, args, sizeof(ir_node_t) * arg_count);
    node->tag = IR_NODE_TUPLE;
    node->data_size = 0;
    node->type = infer_tuple_type(module, args, arg_count, debug);
    node->op_count = arg_count;
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return inserted_node;
}

ir_node_t make_option(struct ir_module* module, ir_type_t option_type, ir_type_t index, ir_node_t val, const struct debug_info* debug) {
    assert(option_type->tag == IR_TYPE_OPTION);
    assert(index->type->tag == IR_KIND_NAT);
    return make_insert(module, make_undef(module, option_type), index, val, debug);
}

static inline ir_type_t infer_extract_type(ir_node_t val, ir_node_t index) {
    if (val->type->tag == IR_TYPE_TUPLE)
        return get_tuple_type_elem(val, get_nat_const_val(index));
    else if (val->type->tag == IR_TYPE_OPTION)
        return get_option_type_elem(val, get_nat_const_val(index));
    else if (val->type->tag == IR_TYPE_ARRAY)
        return get_array_type_elem(val->type);
    else
        assert(false && "invalid extract value");
}

ir_node_t make_extract(struct ir_module* module, ir_node_t val, ir_node_t index, const struct debug_info* debug) {
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(2) {
        .tag = IR_NODE_EXTRACT,
        .debug = debug,
        .type = infer_extract_type(val, index),
        .ops = { val, index },
        .op_count = 2
    });
}

ir_node_t make_insert(struct ir_module* module, ir_node_t val, ir_node_t index, ir_node_t elem, const struct debug_info* debug) {
    assert(elem->type == infer_extract_type(val, index));
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(3) {
        .tag = IR_NODE_INSERT,
        .debug = debug,
        .type = val->type,
        .ops = { val, index, elem },
        .op_count = 3
    });
}
