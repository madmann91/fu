#include "fu/ir/rewrite.h"
#include "fu/ir/scope.h"
#include "fu/ir/module.h"
#include "fu/ir/node.h"
#include "fu/core/alloc.h"

#include <assert.h>

typedef struct {
    Rewriter rewriter;
    const Node* param;
    const NodeSet* scope;
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

const Node* find_rewritten_node(Rewriter* rewriter, const Node* node) {
    const Node* rewritten_node = find_in_node_map(rewriter->rewritten_nodes, node);
    if (!rewritten_node) {
        push_on_dyn_array(&rewriter->rewrite_stack, node);
        return NULL;
    }
    return rewritten_node;
}

bool find_rewritten_nodes(
    Rewriter* rewriter,
    const Node*const* nodes,
    size_t node_count)
{
    clear_dyn_array(&rewriter->op_buf);
    for (size_t i = 0; i < node_count; ++i) {
        const Node* rewritten_node = find_rewritten_node(rewriter, nodes[i]);
        if (!rewritten_node)
            return false;
        push_on_dyn_array(&rewriter->op_buf, rewritten_node);
    }
    return true;
}

static const Node* rewrite_param(Rewriter* rewriter, const Node* node) {
    ParamReplacer* param_replacer = (ParamReplacer*)rewriter;
    if (!find_in_node_set(param_replacer->scope, node))
        return node;
    const Node* replaced_type = NULL;
    if (node->type) {
        replaced_type = find_rewritten_node(rewriter, node->type);
        if (!replaced_type)
            return NULL;
    }
    if (!find_rewritten_nodes(rewriter, node->ops, node->op_count))
        return NULL;
    return rebuild_node(node->tag,
        replaced_type, rewriter->op_buf.elems, node->op_count, &node->data, node->debug_info);
}

const Node* replace_param(const Node* node, const Node* from, const Node* to) {
    assert(from->tag == NODE_PARAM);
    NodeMap node_map = new_node_map();
    NodeSet scope = compute_scope(from);
    insert_in_node_map(&node_map, from, (void*)to);
    ParamReplacer param_replacer = {
        .rewriter = new_rewriter(&node_map, rewrite_param),
        .scope = &scope,
        .param = from
    };
    const Node* replaced_node = rewrite_node(&param_replacer.rewriter, node);
    free_rewriter(&param_replacer.rewriter);
    free_node_map(&node_map);
    free_node_set(&scope);
    return replaced_node;
}

static const Node* manifest_node(Rewriter* rewriter, const Node* node) {
    if (node->tag == NODE_SINGLETON)
        return get_singleton_value(node);
    if (node->tag == NODE_SIGMA) {
        if (!find_rewritten_nodes(rewriter, node->ops, node->op_count))
            return NULL;
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
