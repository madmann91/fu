#include "fu/ir/module.h"
#include "fu/core/hash_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/str_pool.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <string.h>

#define MAX_NODE_LEVEL 3

#define NODE(n) union { Node node; struct { NODE_CONTENTS const Node* ops[n]; } alias; }
#define SMALL_NODE_SIZE 8
typedef NODE(SMALL_NODE_SIZE) SmallNode;
static_assert(
    sizeof(SmallNode) == sizeof(Node) + SMALL_NODE_SIZE * sizeof(Node*),
    "incorrect node type size -- cannot create nodes on the stack");

struct Module {
    HashTable nodes;
    HashTable debug_info;
    MemPool mem_pool;
    StrPool str_pool;
    size_t node_count;
    const Node* universe;
    const Node* star;
    const Node* empty_params;
};

typedef struct {
    const Node* from;
    const Node* to;
} NodePair;

// Defined in simplify.c
const Node* simplify_node(const Node*);

static Node* alloc_node(Module* module, size_t op_count) {
    (void)module;
    return malloc_or_die(sizeof(Node) + sizeof(Node*) * op_count);
}

static void free_node(Module* module, const Node* node) {
    (void)module;
    free((void*)node);
}

static inline HashCode hash_debug_info(HashCode hash, const DebugInfo* debug_info) {
    hash = hash_ptr(hash, debug_info->user_data);
    hash = hash_ptr(hash, debug_info->name);
    hash = hash_file_loc(hash, &debug_info->file_loc);
    return hash;
}

static inline HashCode hash_node(HashCode hash, const Node* node) {
    hash = hash_uint16(hash, node->tag);
    hash = hash_uint64(hash, node->op_count);
    hash = hash_uint64(hash, node->type->id);
    for (size_t i = 0; i < node->op_count; ++i)
        hash = hash_uint64(hash, node->ops[i]->id);
    return hash;
}

static inline bool compare_debug_info(const void* left, const void* right) {
    return !memcmp(left, right, sizeof(DebugInfo));
}

static bool compare_node_pairs(const void* left, const void* right) {
    const Node* left_node  = left;
    const Node* right_node = right;
    return
        left_node->tag      == right_node->tag &&
        left_node->op_count == right_node->op_count &&
        left_node->type     == right_node->type &&
        !memcmp(left_node->ops, right_node->ops, left_node->op_count * sizeof(Node*));
}

static const Node* get_or_insert_node(Module* module, const Node* node) {
    assert(!node->is_nominal);
    assert(!node->type || get_module(node->type) == module);
    assert(!node->type || node->type->level > 0);
    assert(node->op_count >= get_min_op_count(node->tag));
    assert(node->op_count <= get_max_op_count(node->tag));
#ifndef NDEBUG
    for (size_t i = 0; i < node->op_count; ++i)
        assert(get_module(node->ops[i]) == module);
#endif
    // Perform hash consing: return an existing node if there is one, otherwise create a new one
    HashCode hash = hash_node(hash_init(), node); 
    const NodePair* found_pair = find_in_hash_table(
        &module->nodes, &(NodePair) { .from = node }, hash, sizeof(NodePair), compare_node_pairs);
    if (found_pair) {
        // Add the debug information if the existing node does not have one
        if (node->debug_info && !found_pair->to->debug_info)
            ((Node*)found_pair->to)->debug_info = node->debug_info;
        return found_pair->to;
    }

    Node* new_node = alloc_node(module, node->op_count);
    memcpy(new_node, node, sizeof(Node) + node->op_count * sizeof(Node*));
    new_node->id = module->node_count++;
    new_node->level = node->type ? node->type->level - 1 : MAX_NODE_LEVEL;
    bool status = insert_in_hash_table(&module->nodes,
        &(NodePair) { .from = new_node, .to = simplify_node(new_node) },
        hash, sizeof(NodePair), compare_node_pairs);
    if (!status)
    {
        assert(false &&
            "error inserting node in the module"
            "hash function or comparison function might be incorrect");
    }
    return new_node;
}

static const Node* get_or_insert_node_from_args(
    Module* module,
    NodeTag node_tag,
    const Node* type,
    size_t op_count,
    const Node** ops,
    const DebugInfo* debug_info)
{
    SmallNode small_node;
    bool use_alloc = op_count > SMALL_NODE_SIZE;
    Node* node = use_alloc
        ? malloc_or_die(sizeof(Node) + op_count * sizeof(Node*))
        : &small_node.node;
    *node = (Node) {
        .tag = node_tag,
        .type = type,
        .op_count = op_count,
        .debug_info = debug_info
    };
    memcpy(node->ops, ops, sizeof(Node*) * op_count);
    const Node* inserted_node = get_or_insert_node(module, node);
    if (use_alloc)
        free(node);
    return inserted_node;
}

Module* new_module() {
    Module* module = malloc_or_die(sizeof(Module));
    module->nodes = new_hash_table(sizeof(NodePair));
    module->debug_info = new_hash_table(sizeof(DebugInfo));
    module->mem_pool = new_mem_pool();
    module->str_pool = new_str_pool(&module->mem_pool);
    module->universe = get_or_insert_node(module, &(Node) { .tag = NODE_UNIVERSE, .universe.module = module });
    module->star = get_or_insert_node(module, &(Node) { .tag = NODE_STAR, .type = module->universe });
    module->empty_params = get_or_insert_node(module, &(Node) { .tag = NODE_FREE_PARAMS, .type = module->universe });
    return module;
}

static void free_nodes(Module* module) {
    for (size_t i = 0; i < module->nodes.capacity; ++i) {
        if (!is_bucket_occupied(&module->nodes, i))
            continue;
        free_node(module, ((NodePair*)module->nodes.elems)[i].from);
    }
}

void free_module(Module* module) {
    free_str_pool(&module->str_pool);
    free_mem_pool(&module->mem_pool);
    free_nodes(module);
    free_hash_table(&module->nodes);
    free(module);
}

static const DebugInfo* get_or_insert_debug_info(Module* module, const DebugInfo* debug_info) {
    HashCode hash = hash_debug_info(hash_init(), debug_info); 
    const DebugInfo* found_debug_info = find_in_hash_table(
        &module->debug_info, debug_info, hash, sizeof(DebugInfo), compare_debug_info);
    if (found_debug_info)
        return found_debug_info;

    DebugInfo* new_debug_info = alloc_from_mem_pool(&module->mem_pool, sizeof(DebugInfo));
    memcpy(new_debug_info, debug_info, sizeof(DebugInfo));
    bool status = insert_in_hash_table(&module->debug_info, debug_info,
        hash, sizeof(DebugInfo), compare_debug_info);
    if (!status)
    {
        assert(false &&
            "error inserting debug information in the module"
            "hash function or comparison function might be incorrect");
    }
    return new_debug_info;
}

const DebugInfo* make_debug_info(
    Module* module,
    const char* name,
    void* user_data,
    const FileLoc* file_loc)
{
    return get_or_insert_debug_info(module, &(DebugInfo) {
        .name = make_str(&module->str_pool, name),
        .user_data = user_data,
        .file_loc = {
            .file_name = file_loc->file_name
                ? make_str(&module->str_pool, file_loc->file_name) : NULL,
            .begin = file_loc->begin,
            .end = file_loc->end
        }
    });
}

const Node* make_empty_free_params(Module* module) {
    return module->empty_params;
}

const Node* make_single_free_param(const Node* param) {
    return make_free_params(get_module(param), &param, 1);
}

static int compare_params(const void* left, const void* right) {
    Uid left_id  = (*(const Node**)left)->id;
    Uid right_id = (*(const Node**)right)->id;
    return left_id < right_id ? -1 : left_id > right_id ? 1 : 0;
}

const Node* make_free_params(Module* module, const Node** params, size_t param_count) {
#ifndef NDEBUG
    for (size_t i = 0; i < param_count; ++i)
         assert(params[i]->tag == NODE_PARAM);
#endif
    // Sort parameters so that a free param set always compares identical to another if the elements
    // are the same, regardless of the order.
    qsort(params, param_count, sizeof(const Node*), compare_params);
#ifndef NDEBUG
    for (size_t i = 1; i < param_count; ++i)
        assert(params[i - 1] != params[i]);
#endif
    return get_or_insert_node_from_args(module, NODE_FREE_PARAMS,
        module->universe, param_count, params, NULL);
}

const Node* merge_free_params(const Node* left, const Node* right) {
    // Note: Thanks to the design of the module, this operation is memoized. This means that if the
    // merge is requested a second time, the result is already available by in the module, and
    // nothing is recomputed. The actual merging operation is implemented as a simplification rule.
    assert(left->tag  == NODE_FREE_PARAMS);
    assert(right->tag == NODE_FREE_PARAMS);
    Module* module = get_module(left);
    return get_or_insert_node_from_args(module, NODE_MERGE_PARAMS,
        module->universe, 2, (const Node*[]) { left, right }, NULL);
}

static Node* make_nominal_node(NodeTag tag, const Node* type, size_t op_count) {
    Module* module = get_module(type);
    Node* node = alloc_node(module, op_count);
    node->tag = tag;
    node->is_nominal = true;
    node->level = type->level - 1;
    node->id = module->node_count++;
    node->op_count = op_count;
    memset(node->ops, 0, sizeof(Node*) * op_count);
    return node;
}

Node* make_nominal_pi(const Node* type) {
    return make_nominal_node(NODE_PI, type, 2);
}

Node* make_nominal_sigma(const Node* type, size_t op_count) {
    return make_nominal_node(NODE_SIGMA, type, op_count);
}

Node* make_lambda(const Node* type) {
    assert(type->tag == NODE_PI);
    return make_nominal_node(NODE_LAMBDA, type, 1);
}

const Node* make_param(Node* node, const DebugInfo* debug_info) {
    assert(node->is_nominal);
    const Node* type =
        node->tag == NODE_PI    ? get_pi_dom(node) :
        node->tag == NODE_SIGMA ? node :
        get_pi_dom(node->type);
    return get_or_insert_node_from_args(get_module(node),
        NODE_PARAM, type, 1, (const Node*[]) { node }, debug_info);
}

const Node* make_star (Module* module) {
    return module->star;
}

const Node* make_singleton(const Node* node) {
    return get_or_insert_node_from_args(get_module(node), NODE_SINGLETON,
        node->type, 1, &node, NULL);
}

const Node* make_nat_const(Module* module, IntVal int_val) {
    return get_or_insert_node(module, &(Node) {
        .tag = NODE_CONST,
        .type = make_nat(module),
        .const_.int_val = int_val
    });
}

const Node* make_int_const(const Node* type, IntVal int_val) {
    assert(type->tag == NODE_INT);
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_CONST,
        .type = type,
        .const_.int_val = int_val
    });
}

const Node* make_float_const(const Node* type, FloatVal float_val) {
    assert(type->tag == NODE_FLOAT);
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_CONST,
        .type = type,
        .const_.float_val = float_val
    });
}

const Node* make_int(Module* module, size_t bitwidth) {
    return get_or_insert_node_from_args(module, NODE_INT,
        make_star(module), 1, (const Node*[]) { make_nat_const(module, bitwidth) }, NULL);
}

const Node* make_float(Module* module, size_t bitwidth) {
    return get_or_insert_node_from_args(module, NODE_FLOAT,
        make_star(module), 1, (const Node*[]) { make_nat_const(module, bitwidth) }, NULL);
}

const Node* make_proj(const Node* tuple, size_t index, const DebugInfo* debug_info) {
    Module* module = get_module(tuple);
    return get_or_insert_node_from_args(module, NODE_PROJ,
        get_proj_type(tuple->type, index), 2,
        (const Node*[]) { tuple, make_nat_const(module, index) }, debug_info);
}

const Node* make_app(const Node* applied, const Node* arg, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(get_module(applied), NODE_APP,
        get_app_type(applied->type, arg), 2,
        (const Node*[]) { applied, arg }, debug_info);
}

const Node* make_pi(const Node* dom, const Node* codom, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(get_module(dom), NODE_PI,
        codom->type, 2, (const Node*[]) { dom, codom }, debug_info);
}

const Node* make_empty_sigma(Module* module, const Node* type, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(module, NODE_SIGMA, type, 0, NULL, debug_info);
}

const Node* make_sigma(const Node** elems, size_t elem_count, const DebugInfo* debug_info) {
    assert(elem_count > 0);
    const Node* type = elems[0]->type;
    for (size_t i = 1; i < elem_count; ++i) {
        if (type->level < elems[i]->type->level)
            type = elems[i]->type;
    }
    return get_or_insert_node_from_args(get_module(elems[0]), NODE_SIGMA,
        type, elem_count, elems, debug_info);
}

const Node* make_tuple(const Node* type, const Node** elems, size_t elem_count, const DebugInfo* debug_info) {
    assert(type->tag == NODE_SIGMA);
    return get_or_insert_node_from_args(get_module(type), NODE_TUPLE,
        type, elem_count, elems, debug_info);
}
