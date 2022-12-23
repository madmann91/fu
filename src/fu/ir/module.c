#include "fu/ir/module.h"
#include "fu/core/hash_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/str_pool.h"
#include "fu/core/alloc.h"
#include "fu/core/file_loc.h"

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
    User* free_users;
    const Node* universe;
    const Node* noret;
    const Node* star;
    const Node* nat;
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

static User* alloc_user(Module* module) {
    if (module->free_users) {
        User* user = module->free_users;
        module->free_users = (User*)user->next;
        return user;
    }
    return malloc_or_die(sizeof(User));
}

void record_user(Module* module, const Node* used, size_t index, const Node* node) {
    User* user = alloc_user(module);
    user->index = index;
    user->node = node;
    user->next = used->users;
    ((Node*)used)->users = user;
}

static User** find_prev_user(const Node* used, size_t index, const Node* node) {
    User** prev = (User**)&used->users;
    for (const User* user = (User*)used->users; user; prev = (User**)&user->next, user = user->next) {
        if (user->index == index && user->node == node)
            return prev;
    }
    return NULL;
}

void forget_user(Module* module, const Node* used, size_t index, const Node* node) {
    User** prev = find_prev_user(used, index, node);
    assert(prev);
    User* user = *prev;
    *prev = (User*)user->next;
    user->next = module->free_users;
    module->free_users = user;
}

static inline HashCode hash_debug_info(HashCode hash, const DebugInfo* debug_info) {
    hash = hash_ptr(hash, debug_info->user_data);
    hash = hash_str(hash, debug_info->name);
    hash = hash_file_loc(hash, &debug_info->file_loc);
    return hash;
}

static inline bool hash_node_data(HashCode hash, const Node* node) {
    if (is_int_or_nat_const(node))
        return hash_raw_bytes(hash, &node->data.int_val, sizeof(IntVal));
    else if (is_float_const(node))
        return hash_raw_bytes(hash, &node->data.float_val, sizeof(FloatVal));
    else if (node->tag == NODE_LABEL)
        return hash_str(hash, node->data.label);
    return hash;
}

static inline HashCode hash_node(HashCode hash, const Node* node) {
    hash = hash_uint16(hash, node->tag);
    hash = hash_uint64(hash, node->op_count);
    if (node->type)
        hash = hash_uint64(hash, node->type->id);
    hash = hash_node_data(hash, node);
    for (size_t i = 0; i < node->op_count; ++i)
        hash = hash_uint64(hash, node->ops[i]->id);
    return hash;
}

static inline HashCode hash_nominal_node(HashCode hash, const Node* node) {
    hash = hash_uint16(hash, node->tag);
    hash = hash_uint64(hash, node->id);
    return hash;
}

static inline bool compare_debug_info(const void* left, const void* right) {
    const DebugInfo* left_debug_info  = left;
    const DebugInfo* right_debug_info = right;
    return
        left_debug_info->name      == right_debug_info->name &&
        left_debug_info->user_data == right_debug_info->user_data &&
        compare_file_loc(&left_debug_info->file_loc, &right_debug_info->file_loc);
}

static inline bool compare_node_data(const Node* node, const NodeData* left, const NodeData* right) {
    if (is_int_or_nat_const(node))
        return left->int_val == right->int_val;
    else if (is_float_const(node))
        return left->float_val == right->float_val;
    else if (node->tag == NODE_LABEL)
        return left->label == right->label;
    return true;
}

static bool compare_node_pairs(const void* left, const void* right) {
    const Node* left_node  = ((const NodePair*)left)->from;
    const Node* right_node = ((const NodePair*)right)->from;
    if (left_node->is_nominal || right_node->is_nominal)
        return left == right;
    return
        left_node->tag      == right_node->tag &&
        left_node->op_count == right_node->op_count &&
        left_node->type     == right_node->type &&
        compare_node_data(left_node, &left_node->data, &right_node->data) &&
        !memcmp(left_node->ops, right_node->ops, left_node->op_count * sizeof(Node*));
}

static bool contains_error(const Node* node) {
    if (node->tag == NODE_ERROR)
        return true;
    if (node->type && node->type->contains_error)
        return true;
    for (size_t i = 0; i < node->op_count; ++i) {
        if (node->ops[i]->contains_error)
            return true;
    }
    return false;
}

static void mark_users(const Node* node) {
    Module* module = get_module(node);
    for (size_t i = 0; i < node->op_count; ++i)
        record_user(module, node->ops[i], i, node);
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
    new_node->id = module->nodes.size;
    new_node->level = node->type ? node->type->level - 1 : MAX_NODE_LEVEL;
    const Node* simplified_node = simplify_node(new_node);

    // Only compute expensive properties if the node is done being simplified.
    if (new_node == simplified_node) {
        new_node->contains_error = contains_error(new_node);
        mark_users(new_node);
    }

    bool status = insert_in_hash_table(&module->nodes,
        &(NodePair) { .from = new_node, .to = simplified_node },
        hash, sizeof(NodePair), compare_node_pairs);
    if (!status) {
        assert(false &&
            "error inserting node in the module"
            "hash function or comparison function might be incorrect");
    }
    return simplified_node;
}

static const Node* get_or_insert_node_from_args(
    Module* module,
    NodeTag node_tag,
    const Node* type,
    const Node*const* ops,
    size_t op_count,
    const NodeData* node_data,
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
    if (node_data)
        node->data = *node_data;
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
    module->universe = get_or_insert_node(module, &(Node) { .tag = NODE_UNIVERSE, .data.module = module });
    module->star  = get_or_insert_node(module, &(Node) { .tag = NODE_STAR,  .type = module->universe });
    module->noret = get_or_insert_node(module, &(Node) { .tag = NODE_NORET, .type = module->star });
    module->nat   = get_or_insert_node(module, &(Node) { .tag = NODE_NAT,   .type = module->star });
    module->free_users = NULL;
    return module;
}

static void free_nodes(Module* module) {
    for (size_t i = 0; i < module->nodes.capacity; ++i) {
        if (!is_bucket_occupied(&module->nodes, i))
            continue;
        free_node(module, ((NodePair*)module->nodes.elems)[i].from);
    }
}

static void free_users(Module* module) {
    for (User* user = module->free_users; user;) {
        User* next = (User*)user->next;
        free(user);
        user = next;
    }
}

void free_module(Module* module) {
    free_users(module);
    free_nodes(module);
    free_str_pool(&module->str_pool);
    free_mem_pool(&module->mem_pool);
    free_hash_table(&module->debug_info);
    free_hash_table(&module->nodes);
    free(module);
}

const Node* rebuild_node(
    NodeTag node_tag,
    const Node* type,
    const Node*const* ops, size_t op_count,
    const NodeData* node_data,
    const DebugInfo* debug_info)
{
    Module* module = get_module(type);
    return get_or_insert_node_from_args(module, node_tag, type, ops, op_count, node_data, debug_info);
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
    if (!status) {
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

const DebugInfo* import_debug_info(Module* module, const DebugInfo* debug_info) {
    return make_debug_info(module, debug_info->name, debug_info->user_data, &debug_info->file_loc);
}

static Node* make_nominal_node(NodeTag tag, const Node* type, size_t op_count) {
    Module* module = get_module(type);
    Node* node = alloc_node(module, op_count);
    node->tag = tag;
    node->type = type;
    node->is_nominal = true;
    node->level = type->level - 1;
    node->id = module->nodes.size;
    node->op_count = op_count;
    memset(node->ops, 0, sizeof(Node*) * op_count);
    bool status = insert_in_hash_table(&module->nodes,
        &(NodePair) { .from = node, .to = node },
        hash_nominal_node(hash_init(), node),
        sizeof(NodePair), compare_node_pairs);
    if (!status) {
        assert(false &&
            "error inserting nominal node in the module"
            "hash function or comparison function might be incorrect");
    }
    return node;
}

Node* make_nominal_pi(const Node* type) {
    return make_nominal_node(NODE_PI, type, 2);
}

Node* make_nominal_sigma(const Node* type, size_t op_count) {
    return make_nominal_node(NODE_SIGMA, type, op_count);
}

Node* make_nominal_variant(const Node* type, size_t op_count) {
    return make_nominal_node(NODE_VARIANT, type, op_count);
}

Node* make_lambda(const Node* type) {
    assert(type->tag == NODE_PI);
    return make_nominal_node(NODE_LAMBDA, type, 1);
}

Node* make_axiom(const Node* type) {
    return make_nominal_node(NODE_AXIOM, type, 0);
}

const Node* make_param(const Node* node, const DebugInfo* debug_info) {
    assert(node->is_nominal);
    const Node* type =
        node->tag == NODE_PI    ? get_pi_dom(node) :
        node->tag == NODE_SIGMA ? node :
        get_pi_dom(node->type);
    return get_or_insert_node_from_args(get_module(node),
        NODE_PARAM, type, (const Node*[]) { node }, 1, NULL, debug_info);
}

const Node* make_label(const Node* value, const char* label, const DebugInfo* debug_info) {
    Module* module = get_module(value);
    return get_or_insert_node_from_args(module, NODE_LABEL, value->type, &value, 1,
        &(NodeData) { .label = make_str(&module->str_pool, label) }, debug_info);
}

const Node* make_star(Module* module) {
    return module->star;
}

const Node* make_noret(Module* module) {
    return module->noret;
}

const Node* make_error(const Node* type) {
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_ERROR,
        .type = type
    });
}

const Node* make_top(const Node* type) {
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_TOP,
        .type = type
    });
}

const Node* make_bottom(const Node* type) {
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_BOTTOM,
        .type = type
    });
}

const Node* make_nat(Module* module) {
    return module->nat;
}

const Node* make_singleton(const Node* node) {
    return get_or_insert_node_from_args(get_module(node), NODE_SINGLETON,
        node->type, &node, 1, NULL, NULL);
}

const Node* make_nat_const(Module* module, IntVal int_val) {
    return get_or_insert_node(module, &(Node) {
        .tag = NODE_CONST,
        .type = make_nat(module),
        .data.int_val = int_val
    });
}

const Node* make_int_const(const Node* type, IntVal int_val) {
    assert(type->tag == NODE_INT);
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_CONST,
        .type = type,
        .data.int_val = int_val
    });
}

const Node* make_float_const(const Node* type, FloatVal float_val) {
    assert(type->tag == NODE_FLOAT);
    return get_or_insert_node(get_module(type), &(Node) {
        .tag = NODE_CONST,
        .type = type,
        .data.float_val = float_val
    });
}

const Node* make_int(Module* module, size_t bitwidth) {
    return get_or_insert_node_from_args(module, NODE_INT,
        make_star(module), (const Node*[]) { make_nat_const(module, bitwidth) }, 1, NULL, NULL);
}

const Node* make_float(Module* module, size_t bitwidth) {
    return get_or_insert_node_from_args(module, NODE_FLOAT,
        make_star(module), (const Node*[]) { make_nat_const(module, bitwidth) }, 1, NULL, NULL);
}

const Node* make_proj(const Node* tuple, size_t index, const DebugInfo* debug_info) {
    Module* module = get_module(tuple);
    return get_or_insert_node_from_args(module, NODE_PROJ,
        get_proj_type(tuple->type, tuple, index),
        (const Node*[]) { tuple, make_nat_const(module, index) }, 2,
        NULL, debug_info);
}

const Node* make_app(const Node* applied, const Node* arg, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(get_module(applied), NODE_APP,
        get_app_type(applied->type, arg),
        (const Node*[]) { applied, arg }, 2,
        NULL, debug_info);
}

const Node* make_pi(const Node* dom, const Node* codom, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(get_module(dom), NODE_PI,
        codom->type, (const Node*[]) { dom, codom }, 2, NULL, debug_info);
}

const Node* make_empty_sigma(const Node* type, const DebugInfo* debug_info) {
    return get_or_insert_node_from_args(get_module(type), NODE_SIGMA, type, NULL, 0, NULL, debug_info);
}

static const Node* make_sigma_or_variant(
    NodeTag node_tag,
    const Node** elems,
    size_t elem_count,
    const DebugInfo* debug_info)
{
    assert(elem_count > 0);
    const Node* type = elems[0]->type;
    for (size_t i = 1; i < elem_count; ++i) {
        if (type->level < elems[i]->type->level)
            type = elems[i]->type;
    }
    return get_or_insert_node_from_args(get_module(elems[0]), node_tag,
        type, elems, elem_count, NULL, debug_info);
}

const Node* make_sigma(const Node** elems, size_t elem_count, const DebugInfo* debug_info) {
    return make_sigma_or_variant(NODE_SIGMA, elems, elem_count, debug_info);
}

const Node* make_variant(const Node** options, size_t option_count, const DebugInfo* debug_info) {
    return make_sigma_or_variant(NODE_VARIANT, options, option_count, debug_info);
}

const Node* make_tuple(const Node* type, const Node** elems, size_t elem_count, const DebugInfo* debug_info) {
    assert(type->tag == NODE_SIGMA);
    return get_or_insert_node_from_args(get_module(type), NODE_TUPLE,
        type, elems, elem_count, NULL, debug_info);
}