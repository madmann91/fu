#include "fu/ir/node.h"
#include "fu/ir/module.h"
#include "fu/core/alloc.h"

static const Node* merge_free_params_once(const Node* left, const Node* right) {
    // Note: This is only run once for every merge operation
    size_t i = 0, j = 0, k = 0;
    const Node** params = malloc_or_die(sizeof(Node*) * (left->op_count + right->op_count));
    for (; i < left->op_count && j < right->op_count;) {
        if (left->ops[i]->id < right->ops[j]->id)
            params[k++] = left->ops[i++];
        else if (left->ops[i]->id > right->ops[j]->id)
            params[k++] = right->ops[j++];
        else
            params[k++] = left->ops[i++], j++;
    }
    for (; i < left->op_count; ++i)
        params[k++] = left->ops[i];
    for (; j < right->op_count; ++j)
        params[k++] = right->ops[j];
    const Node* free_params = make_free_params(get_module(left), params, k);
    free(params);
    return free_params;
}

const Node* simplify_node(const Node* node) {
    if (node->tag == NODE_MERGE_PARAMS)
        return merge_free_params_once(node->ops[0], node->ops[1]);
    return node;
}
