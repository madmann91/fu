#include "fu/ir/rewrite.h"
#include "fu/ir/module.h"
#include "fu/ir/node.h"
#include "fu/core/alloc.h"

#include <assert.h>

typedef struct {
    Rewriter rewriter;
    DynArray op_buf;
    const Node* param;
} ParamReplacer;

Rewriter new_rewriter(NodeMap* node_map, RewriteFn rewrite_fn) {
    return (Rewriter) {
        .rewritten_nodes = node_map,
        .rewrite_stack = new_dyn_array(sizeof(const Node*)),
        .op_buf = new_dyn_array(sizeof(const Node*)),
        .rewrite_fn = rewrite_fn
    };
}

void free_rewriter(Rewriter* rewriter) {
    free_dyn_array(&rewriter->rewrite_stack);
    free_dyn_array(&rewriter->op_buf);
}

const Node* rewrite_node(Rewriter* rewriter, const Node* first_node) {
    const Node* node = NULL;
    const Node* last_found = NULL;
    push_on_dyn_array(&rewriter->rewrite_stack, &first_node);
    while (rewriter->rewrite_stack.size > 0) {
    restart:
        node = ((const Node**)rewriter->rewrite_stack.elems)[rewriter->rewrite_stack.size - 1];
        if ((last_found = find_in_node_map(rewriter->rewritten_nodes, node))) {
            pop_from_dyn_array(&rewriter->rewrite_stack);
            continue;
        }
        const Node* rewritten_node = rewriter->rewrite_fn(rewriter, node);
        if (!rewritten_node)
            goto restart;
        pop_from_dyn_array(&rewriter->rewrite_stack);
        insert_in_node_map(rewriter->rewritten_nodes, node, (void*)rewritten_node);
    }
    return last_found;
}

static const Node* find_node_with_replaced_param(ParamReplacer* param_replacer, const Node* node) {
    if (!contains_free_param(node->free_params, param_replacer->param))
        return node;
    const Node** replaced_node = find_in_node_map(param_replacer->rewriter.rewritten_nodes, node);
    return replaced_node ? *replaced_node : NULL;
}

static const Node* rewrite_param(Rewriter* rewriter, const Node* node) {
    ParamReplacer* param_replacer = (ParamReplacer*)rewriter;
    const Node* replaced_type = NULL;
    if (node->type) {
        replaced_type = find_node_with_replaced_param(param_replacer, node->type);
        if (!replaced_type)
            return NULL;
    }
    clear_dyn_array(&rewriter->op_buf);
    for (size_t i = 0; i < node->op_count; ++i) {
        const Node* replaced_op = find_node_with_replaced_param(param_replacer, node->ops[i]);
        if (!replaced_op)
            return NULL;
        push_on_dyn_array(&rewriter->op_buf, replaced_op);
    }
    return rebuild_node(node->tag,
        replaced_type, rewriter->op_buf.elems, node->op_count, &node->data, node->debug_info);
}

const Node* replace_param(const Node* node, const Node* from, const Node* to) {
    assert(from->tag == NODE_PARAM);
    NodeMap node_map = new_node_map();
    insert_in_node_map(&node_map, from, (void*)to);
    ParamReplacer param_replacer = {
        .rewriter = new_rewriter(&node_map, rewrite_param),
        .param = from
    };
    const Node* replaced_node = rewrite_node(&param_replacer.rewriter, node);
    free_rewriter(&param_replacer.rewriter);
    free_node_map(&node_map);
    return replaced_node;
}

static const Node* manifest_node(Rewriter* rewriter, const Node* node) {
    if (node->tag == NODE_SINGLETON)
        return get_singleton_value(node);
    if (node->tag == NODE_SIGMA) {
        clear_dyn_array(&rewriter->op_buf);
        for (size_t i = 0; i < node->op_count; ++i) {
            const Node* manifested_op = find_in_node_map(rewriter->rewritten_nodes, node->ops[i]);
            push_on_dyn_array(&rewriter->op_buf, manifested_op);
            if (!manifested_op)
                return NULL;
        }
        return make_tuple(node, rewriter->op_buf.elems, node->op_count, NULL);
    }
    return make_error(node);
}

const Node* manifest_singletons(const Node* type) {
    NodeMap node_map = new_node_map();
    Rewriter rewriter = new_rewriter(&node_map, manifest_node);
    const Node* manifested_node = rewrite_node(&rewriter, type);
    free_node_map(&node_map);
    free_rewriter(&rewriter);
    return manifested_node;
}
