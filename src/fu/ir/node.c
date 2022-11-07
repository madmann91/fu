#include "fu/ir/node.h"
#include "fu/ir/module.h"

#include <assert.h>

Module* get_module(const Node* node) {
    while (node->type)
        node = node->type;
    assert(node->tag == NODE_UNIVERSE);
    return node->universe.module;
}

Node* cast_nominal_node(const Node* node) {
    assert(node->is_nominal);
    return (Node*)node;
}

const char* get_op_name(NodeTag tag) {
    switch (tag) {
#define node(name, n, str) case NODE_##name: return str;
        NODE_LIST(node)
#undef node
        default:
            assert(false && "invalid node tag");
            return "";
    }
}

bool is_nominal_node(const Node* node) {
    return node->is_nominal;
}

#define node(name, ...) case NODE_##name:
#define MAKE_PREDICATE(predicate, list) \
    bool predicate(NodeTag tag) { \
        switch (tag) { \
            list(node) \
                return true; \
            default: \
                return false; \
        } \
    }

MAKE_PREDICATE(is_kind_node_tag, KIND_LIST)
MAKE_PREDICATE(is_type_node_tag, TYPE_LIST)
MAKE_PREDICATE(is_value_node_tag, VALUE_LIST)

#undef MAKE_PREDICATE
#undef node

bool is_int_const(const Node* node) {
    return node->tag == NODE_CONST && node->type->tag == NODE_INT;
}

bool is_nat_const(const Node* node) {
    return node->tag == NODE_CONST && node->type->tag == NODE_NAT;
}

bool is_int_or_nat_const(const Node* node) {
    return is_int_const(node) || is_nat_const(node);
}

bool is_float_const(const Node* node) {
    return node->tag == NODE_CONST && node->type->tag == NODE_FLOAT;
}

FloatVal get_float_const_value(const Node* node) {
    assert(is_float_const(node));
    return node->const_.float_val;
}

IntVal get_int_or_nat_const_value(const Node* node) {
    assert(is_int_or_nat_const(node));
    return node->const_.int_val;
}

const Node* get_array_type_or_sigma_elem(const Node* node, size_t i) {
    assert(node->tag == NODE_ARRAY_TYPE || node->tag == NODE_SIGMA);
    return node->ops[node->tag == NODE_ARRAY_TYPE ? 0 : i];
}

const Node* get_array_or_tuple_elem(const Node* node, size_t i) {
    assert(node->tag == NODE_ARRAY || node->tag == NODE_TUPLE);
    return node->ops[i];
}

const Node* get_pi_dom(const Node* node) {
    assert(node->tag == NODE_PI);
    return node->ops[0];
}

const Node* get_pi_codom(const Node* node) {
    assert(node->tag == NODE_PI);
    return node->ops[1];
}

const Node* get_insert_or_extract_value(const Node* node) {
    assert(node->tag == NODE_INSERT || node->tag == NODE_EXTRACT);
    return node->ops[0];
}

const Node* get_insert_or_extract_index(const Node* node) {
    assert(node->tag == NODE_INSERT || node->tag == NODE_EXTRACT);
    return node->ops[1];
}

const Node* get_insert_elem(const Node* node) {
    assert(node->tag == NODE_INSERT);
    return node->ops[2];
}

static void print_unique_node_name(FormatState* state, const Node* node) {
    format(state, "{s}~{u64}", (FormatArg[]) {
        { .s = get_node_tag_name(node->tag) },
        { .u64 = node->id }
    });
}

void print_node(FormatState* state, const Node* node) {
    print_keyword(state, get_op_name(node->tag));
    if (node->tag == NODE_CONST) {
        assert(is_int_const(node) || is_float_const(node));
        if (is_int_or_nat_const(node))
            format(state, "{u64}", (FormatArg[]) { { .u64 = get_int_or_nat_const_value(node) } });
        else
            format(state, "{f64}", (FormatArg[]) { { .f64 = get_float_const_value(node) } });
    } else {
        format(state, "(", NULL);
        for (size_t i = 0, n = node->op_count; i < n; ++i) {
            print_unique_node_name(state, node->ops[i]);
            if (i != n - 1)
                format(state, ", ", NULL);
        }
        format(state, ")", NULL);
    }
}

#ifndef NDEBUG // GCOV_EXCL_START
void dump_node(const Node* node) {
    FormatState state = new_format_state("    ", !is_color_supported(stdout));
    print_node(&state, node);
    write_format_state(&state, stdout);
    free_format_state(&state);
    printf("\n");
}
#endif // GCOV_EXCL_STOP
