#include "fu/ir/node.h"
#include "fu/ir/module.h"
#include "fu/ir/rewrite.h"
#include "fu/core/utils.h"

#include <assert.h>
#include <string.h>

Module* get_module(const Node* node) {
    while (node->type)
        node = node->type;
    assert(node->tag == NODE_UNIVERSE);
    return node->data.module;
}

Node* nominal_cast(const Node* node) {
    assert(node->is_nominal);
    return (Node*)node;
}

size_t get_min_op_count(NodeTag tag) {
    switch (tag) {
#define N 0
#define node(name, n, str) case NODE_##name: return n;
        NODE_LIST(node)
#undef node
#undef N
        default:
            assert(false && "invalid node tag");
            return 0;
    }
}

size_t get_max_op_count(NodeTag tag) {
    switch (tag) {
#define N SIZE_MAX
#define node(name, n, str) case NODE_##name: return n;
        NODE_LIST(node)
#undef node
#undef N
        default:
            assert(false && "invalid node tag");
            return SIZE_MAX;
    }
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
    return node->data.float_val;
}

IntVal get_int_or_nat_const_value(const Node* node) {
    assert(is_int_or_nat_const(node));
    return node->data.int_val;
}

const char* get_label_name(const Node* node) {
    assert(node->tag == NODE_LABEL);
    return node->data.label;
}

size_t find_label_index(const Node** labels, size_t count, const char* label) {
    for (size_t i = 0; i < count; ++i) {
        if (!strcmp(label, get_label_name(labels[i])))
            return i;
    }
    return SIZE_MAX;
}

// Defined in module.c
void record_user(Module*, const Node*, size_t, const Node*);
void forget_user(Module*, const Node*, size_t, const Node*);

void unset_op(Node* node, size_t index) {
    assert(node->ops[index]);
    forget_user(get_module(node), node->ops[index], index, node);
    node->ops[index] = NULL;
}

void set_op(Node* node, size_t index, const Node* op) {
    assert(!node->ops[index]);
    record_user(get_module(node), op, index, node);
    node->ops[index] = op;
}

const Node* get_singleton_value(const Node* node) {
    assert(node->tag == NODE_SINGLETON);
    return node->ops[0];
}

const Node* get_pi_dom(const Node* node) {
    assert(node->tag == NODE_PI);
    return node->ops[0];
}

const Node* get_pi_codom(const Node* node) {
    assert(node->tag == NODE_PI);
    return node->ops[1];
}

const Node* get_proj_type(const Node* sigma, const Node* value, size_t index) {
    assert(sigma->type->tag == NODE_SIGMA);
    assert(index < sigma->op_count);
    if (!sigma->is_nominal)
        return sigma->ops[index];
    return replace_param(sigma->ops[index], make_param(sigma, NULL), value);
}

const Node* get_app_type(const Node* pi, const Node* arg) {
    assert(arg->type == get_pi_dom(pi));
    if (!pi->is_nominal)
        return get_pi_codom(pi);
    return replace_param(get_pi_codom(pi), make_param(pi, NULL), arg);
}

Node* get_param_nominal(const Node* param) {
    assert(param->tag == NODE_PARAM);
    return nominal_cast(param->ops[0]);
}

static void print_unique_node_name(FormatState* state, const Node* node) {
    format(state, "{s}~{u64}", (FormatArg[]) {
        { .s = get_op_name(node->tag) },
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
    } else if (node->op_count > 0) {
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