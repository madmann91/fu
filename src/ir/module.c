#include "core/hash_table.h"
#include "core/hash.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/string_pool.h"
#include "core/utils.h"
#include "ir/module.h"
#include "ir/node.h"
#include "ir/error_mgr.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define DEFAULT_MODULE_CAPACITY 256

struct ir_module {
    struct hash_table nodes;
    struct hash_table debug;
    struct mem_pool mem_pool;
    struct string_pool string_pool;
    struct error_mgr* error_mgr;

    ir_node_t nat, star, error;
};

struct ir_node_pair {
    ir_node_t fst;
    ir_node_t snd;
};

static ir_node_t insert_ir_node(struct ir_module*, ir_node_t);

struct ir_module* new_ir_module(struct error_mgr* error_mgr) {
    struct ir_module* module = malloc_or_die(sizeof(struct ir_module));
    module->nodes    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct ir_node_pair));
    module->debug    = new_hash_table(DEFAULT_MODULE_CAPACITY, sizeof(struct debug_info*));
    module->mem_pool = new_mem_pool();
    module->string_pool = new_string_pool();
    module->error_mgr = error_mgr;
    module->nat   = insert_ir_node(module, &(struct ir_node) { .tag = IR_KIND_NAT  });
    module->star  = insert_ir_node(module, &(struct ir_node) { .tag = IR_KIND_STAR });
    module->error = insert_ir_node(module, &(struct ir_node) { .tag = IR_ERROR });
    ((struct ir_node*)module->error)->type = module->error;
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
        left->type != right->type ||
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
    assert(
        new_node_pair.snd->type == new_node->type &&
        "simplified nodes must have the same type as the original, non-simplified one");
    must_succeed(insert_in_hash_table(&module->nodes, &new_node_pair, hash, sizeof(struct ir_node_pair), compare_ir_nodes));
    return new_node_pair.snd;
}

ir_node_t make_node(
    struct ir_module* module,
    enum ir_node_tag tag,
    ir_node_t type,
    const ir_node_t* ops, size_t op_count,
    const union ir_node_data* data,
    const struct debug_info* debug)
{
    if (module->error_mgr) {
        size_t expected_op_count = get_expected_op_count(tag);
        if (expected_op_count != op_count && expected_op_count != SIZE_MAX) {
            module->error_mgr->invalid_op_count(module->error_mgr, tag, op_count, expected_op_count, debug);
            return make_error(module);
        }
        ir_node_t expected_type = as_node(infer_type(module, tag, (const ir_val_t*)ops, op_count, debug));
        if (expected_type != type && expected_type != NULL) {
            module->error_mgr->invalid_type(module->error_mgr, to_type(type), to_type(expected_type), debug);
            return make_error(module);
        }
    }

    switch (tag) {
        case IR_VAR:
            return op_count == 1
                ? make_tied_var(module, type, data->var_index, ops[0], debug)
                : make_var(module, type, data->var_index, debug);
        case IR_TYPE_INT:    return as_node(make_int_type(module, to_type(ops[0])));
        case IR_TYPE_FLOAT:  return as_node(make_float_type(module, to_type(ops[0])));
        case IR_VAL_LET:     return as_node(make_let(module, (const ir_val_t*)ops, op_count - 1, to_val(ops[op_count - 1]), debug));
        case IR_VAL_TUPLE:   return as_node(make_tuple(module, (const ir_val_t*)ops, op_count, debug));
        case IR_VAL_EXTRACT: return as_node(make_extract(module, to_val(ops[0]), to_val(ops[1]), debug));
        case IR_VAL_INSERT:  return as_node(make_insert(module, to_val(ops[0]), to_val(ops[1]), to_val(ops[2]), debug));
#define int_arith_op(tag, str, n) case IR_VAL_##tag:
        IR_INT_ARITH_OP_LIST(int_arith_op)
#undef int_arith_op
            return as_node(make_int_arith_op(module, tag, to_val(ops[0]), to_val(ops[1]), debug));
        default:
            assert(false);
            return NULL;
    }
}

ir_node_t rebuild_node(struct ir_module* module, ir_node_t node, ir_node_t type, const ir_node_t* ops, const struct debug_info* debug) {
    return make_node(module, node->tag, type, ops, node->op_count, &node->data, debug);
}

ir_val_t rebuild_val(struct ir_module* module, ir_val_t val,  ir_type_t type, const ir_val_t* ops, const struct debug_info* debug) {
    return to_val(make_node(module, val->tag, as_node(type), (const ir_node_t*)ops, val->op_count, &val->data, debug));
}

ir_type_t rebuild_type(struct ir_module* module, ir_type_t type, ir_kind_t kind, const ir_type_t* ops, const struct debug_info* debug) {
    return to_type(make_node(module, type->tag, as_node(kind), (const ir_node_t*)ops, type->op_count, &type->data, debug));
}

static inline ir_type_t infer_extract_type(ir_val_t val, ir_val_t index) {
    ir_type_t val_type = to_type(val->type);
    if (val_type->tag == IR_TYPE_TUPLE)
        return get_tuple_type_elem(val_type, get_int_const_val(index));
    else if (val_type->tag == IR_TYPE_OPTION)
        return get_option_type_elem(val_type, get_int_const_val(index));
    else if (val_type->tag == IR_TYPE_ARRAY)
        return get_array_type_elem(val_type);
    else
        return NULL;
}

static inline ir_type_t infer_tuple_type(struct ir_module* module, const ir_val_t* args, size_t arg_count, const struct debug_info* debug) {
    ir_type_t* arg_types = malloc_or_die(sizeof(ir_type_t) * arg_count);
    for (size_t i = 0; i < arg_count; ++i)
        arg_types[i] = to_type(args[i]->type);
    ir_type_t tuple_type = make_tuple_type(module, arg_types, arg_count, debug);
    free(arg_types);
    return tuple_type;
}

static inline ir_type_t infer_insert_type(ir_val_t val) { return to_type(val->type); }
static inline ir_type_t infer_let_type(ir_val_t body) { return to_type(body->type); }
static inline ir_type_t infer_int_arith_op_type(ir_val_t left) { return to_type(left->type); }

ir_type_t infer_type(struct ir_module* module, enum ir_node_tag tag, const ir_val_t* ops, size_t op_count, const struct debug_info* debug) {
    switch (tag) {
        case IR_VAL_LET:     return infer_let_type(ops[op_count - 1]);
        case IR_VAL_TUPLE:   return infer_tuple_type(module, ops, op_count, debug);
        case IR_VAL_EXTRACT: return infer_extract_type(ops[0], ops[1]);
        case IR_VAL_INSERT:  return infer_insert_type(ops[0]);
#define int_arith_op(tag, str, n) case IR_VAL_##tag:
        IR_INT_ARITH_OP_LIST(int_arith_op)
#undef int_arith_op
            return infer_int_arith_op_type(ops[0]);
        default: return NULL;
    }
}

#define IR_NODE_WITH_N_OPS(n) \
    (struct ir_node*) &(struct { IR_NODE_FIELDS ir_node_t ops[n]; })

static inline void check_type(struct ir_module* module, ir_type_t type, ir_type_t expected, const struct debug_info* debug) {
    if (module->error_mgr && type != expected)
        module->error_mgr->invalid_type(module->error_mgr, type, expected, debug);
}

static inline void check_kind(struct ir_module* module, ir_kind_t kind, ir_kind_t expected, const struct debug_info* debug) {
    if (module->error_mgr && kind != expected)
        module->error_mgr->invalid_kind(module->error_mgr, kind, expected, debug);
}

static inline void check_type_or_kind(struct ir_module* module, ir_node_t type_or_kind, ir_node_t expected, const struct debug_info* debug) {
    if (is_kind(expected))
        check_kind(module, (ir_kind_t)type_or_kind, to_kind(expected), debug);
    else
        check_type(module, (ir_type_t)type_or_kind, to_type(expected), debug);
}

ir_node_t make_error(struct ir_module* module) { return module->error; }
ir_kind_t make_star(struct ir_module* module) { return to_kind(module->star); }
ir_kind_t make_nat(struct ir_module* module) { return to_kind(module->nat); }

ir_type_t make_int_type(struct ir_module* module, ir_type_t bitwidth) {
    check_kind(module, to_kind(bitwidth->type), make_nat(module), bitwidth->debug);
    return to_type(insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_TYPE_INT,
        .type = as_node(make_star(module)),
        .ops = { as_node(bitwidth) },
        .op_count = 1
    }));
}

ir_type_t make_intn_type(struct ir_module* module, ir_uint_t bitwidth) {
    return make_int_type(module, make_nat_const(module, bitwidth));
}

ir_type_t make_float_type(struct ir_module* module, ir_type_t bitwidth) {
    check_kind(module, to_kind(bitwidth->type), make_nat(module), bitwidth->debug);
    return to_type(insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_TYPE_FLOAT,
        .type = as_node(make_star(module)),
        .ops = { as_node(bitwidth) },
        .op_count = 1
    }));
}

ir_type_t make_floatn_type(struct ir_module* module, ir_uint_t bitwidth) {
    return make_float_type(module, make_nat_const(module, bitwidth));
}

ir_type_t make_tuple_type(struct ir_module* module, const ir_type_t* elems, size_t elem_count, const struct debug_info* debug) {
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (elem_count + 1));
    memcpy(node->ops, elems, sizeof(ir_node_t) * elem_count);
    node->tag = IR_TYPE_TUPLE;
    node->data_size = 0;
    node->type = as_node(make_star(module));
    node->op_count = elem_count;
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return to_type(inserted_node);
}

ir_type_t make_func_type(struct ir_module* module, ir_type_t dom, ir_type_t codom, const struct debug_info* debug) {
    return to_type(insert_ir_node(module, IR_NODE_WITH_N_OPS(2) {
        .tag = IR_TYPE_FUNC,
        .debug = debug,
        .type = codom->type,
        .ops = { as_node(dom), as_node(codom) },
        .op_count = 2
    }));
}

ir_val_t make_undef(struct ir_module* module, ir_type_t type) {
    return to_val(insert_ir_node(module, &(struct ir_node) { .tag = IR_VAL_UNDEF, .type = as_node(type) }));
}

ir_node_t make_const(struct ir_module* module, ir_node_t type, const union ir_node_data* data, size_t data_size) {
    if (module->error_mgr && type->tag != IR_TYPE_INT && type->tag != IR_TYPE_FLOAT && type->tag != IR_KIND_NAT)
        module->error_mgr->unexpected_node(module->error_mgr, type, "int, float, or nat", type->debug);
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_CONST,
        .type = type,
        .data = *data,
        .data_size = data_size
    });
}

ir_type_t make_nat_const(struct ir_module* module, ir_uint_t int_val) {
    return to_type(make_const(module, as_node(make_nat(module)), &(union ir_node_data) { .int_val = int_val }, sizeof(ir_uint_t)));
}

ir_val_t make_int_const(struct ir_module* module, ir_type_t type, ir_uint_t int_val) {
    return to_val(make_const(module, as_node(type), &(union ir_node_data) { .int_val = int_val & get_int_type_bitmask(type) }, sizeof(ir_uint_t)));
}

ir_val_t make_float_const(struct ir_module* module, ir_type_t type, ir_float_t float_val) {
    return to_val(make_const(module, as_node(type), &(union ir_node_data) { .float_val = float_val }, sizeof(ir_float_t)));
}

ir_node_t make_var(struct ir_module* module, ir_node_t type, size_t var_index, const struct debug_info* debug) {
    return insert_ir_node(module, &(struct ir_node) {
        .tag = IR_VAR,
        .debug = debug,
        .type = type,
        .data_size = sizeof(size_t),
        .data.var_index = var_index
    });
}

ir_node_t make_tied_var(struct ir_module* module, ir_node_t type, size_t var_index, ir_node_t value, const struct debug_info* debug) {
    check_type_or_kind(module, value->type, type, debug);
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(1) {
        .tag = IR_VAR,
        .debug = debug,
        .type = type,
        .data_size = sizeof(size_t),
        .data.var_index = var_index,
        .ops = { value },
        .op_count = 1
    });
}

ir_node_t make_func(struct ir_module* module, ir_node_t var, ir_node_t body, const struct debug_info* debug) {
    assert(var->tag == IR_VAR);
    return insert_ir_node(module, IR_NODE_WITH_N_OPS(2) {
        .tag = IR_VAL_FUNC,
        .debug = debug,
        .type = as_node(make_func_type(module, to_type(var->type), to_type(body->type), debug)),
        .ops = { var, body },
        .op_count = 2
    });
}

ir_val_t make_let(struct ir_module* module, const ir_val_t* vars, size_t var_count, ir_val_t body, const struct debug_info* debug) {
#ifndef NDEBUG
    for (size_t i = 0; i < var_count; ++i)
        assert(is_tied_var(as_node(vars[i])));
#endif
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (var_count + 1));
    memcpy(node->ops, vars, sizeof(ir_node_t) * var_count);
    node->tag = IR_VAL_LET;
    node->data_size = 0;
    node->type = as_node(infer_let_type(body));
    node->op_count = var_count + 1;
    node->ops[var_count] = as_node(body);
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return to_val(inserted_node);
}

ir_val_t make_tuple(struct ir_module* module, const ir_val_t* args, size_t arg_count, const struct debug_info* debug) {
    struct ir_node* node = malloc_or_die(
        sizeof(struct ir_node) + sizeof(ir_node_t) * (arg_count + 1));
    memcpy(node->ops, args, sizeof(ir_node_t) * arg_count);
    node->tag = IR_VAL_TUPLE;
    node->data_size = 0;
    node->type = as_node(infer_tuple_type(module, args, arg_count, debug));
    node->op_count = arg_count;
    node->debug = debug;
    ir_node_t inserted_node = insert_ir_node(module, node);
    free(node);
    return to_val(inserted_node);
}

ir_val_t make_extract(struct ir_module* module, ir_val_t val, ir_val_t index, const struct debug_info* debug) {
    return to_val(insert_ir_node(module, IR_NODE_WITH_N_OPS(2) {
        .tag = IR_VAL_EXTRACT,
        .debug = debug,
        .type = as_node(infer_extract_type(val, index)),
        .ops = { as_node(val), as_node(index) },
        .op_count = 2
    }));
}

ir_val_t make_insert(struct ir_module* module, ir_val_t val, ir_val_t index, ir_val_t elem, const struct debug_info* debug) {
    check_type(module, to_type(elem->type), infer_extract_type(val, index), debug);
    return to_val(insert_ir_node(module, IR_NODE_WITH_N_OPS(3) {
        .tag = IR_VAL_INSERT,
        .debug = debug,
        .type = as_node(infer_insert_type(val)),
        .ops = { as_node(val), as_node(index), as_node(elem) },
        .op_count = 3
    }));
}

ir_val_t make_int_arith_op(struct ir_module* module, enum ir_node_tag tag, ir_val_t left, ir_val_t right, const struct debug_info* debug) {
    check_type(module, to_type(left->type), to_type(right->type), debug);
    return to_val(insert_ir_node(module, IR_NODE_WITH_N_OPS(2) {
        .tag = tag,
        .debug = debug,
        .type = as_node(infer_int_arith_op_type(left)),
        .ops = { as_node(left), as_node(right) },
        .op_count = 2
    }));
}
