#include "fu/ir/import.h"
#include "fu/ir/node.h"
#include "fu/core/alloc.h"
#include "fu/core/dyn_array.h"

Importer new_importer(
    ImportPredFn import_pred_fn,
    PreTransformFn pre_transform_fn,
    RebuildFn rebuild_fn)
{
    Importer importer = {
        .import_pred_fn = import_pred_fn,
        .pre_transform_fn = pre_transform_fn,
        .rebuild_fn = rebuild_fn,
        .rebuild_map = new_node_map()
    };
    if (pre_transform_fn)
        importer.pre_transform_map = new_node_map();
    return importer;
}

void free_importer(Importer* importer) {
    if (importer->pre_transform_fn)
        free_node_map(&importer->pre_transform_map);
    free_node_map(&importer->rebuild_map);
}

static const Node* pre_transform_node(Importer* importer, const Node* first_node) {
    // This executes the pre-transform function until it reaches a fix point, using path halving to
    // reduce the number of calls to the pre-transform function.
    if (!importer->pre_transform_fn)
        return first_node;
    const Node* node = first_node;
    const Node* prev_node = NULL;
    while (true) {
        const Node* pre_transformed_node = find_in_node_map(&importer->pre_transform_map, node);
        if (!pre_transformed_node) {
            pre_transformed_node = importer->pre_transform_fn(node);
            insert_in_node_map(&importer->pre_transform_map, node, (void*)pre_transformed_node);
        }
        if (prev_node)
            replace_in_node_map(&importer->pre_transform_map, prev_node, (void*)pre_transformed_node);
        if (node == pre_transformed_node)
            break;
        prev_node = node;
        node = pre_transformed_node;
    }
    return node;
}

const Node* find_node(Importer* importer, const Node* node) {
    return find_in_node_map(&importer->rebuild_map, pre_transform_node(importer, node));
}

const Node* import_node(Importer* importer, const Node* first_node) {
    DynArray stack = new_dyn_array(sizeof(const Node*));
    push_on_dyn_array(&stack, &first_node);
    const Node* node = NULL;
    const Node* last_found = NULL;
    while (stack.size > 0) {
    restart:
        node = pre_transform_node(importer, ((const Node**)stack.elems)[stack.size - 1]);
        if ((last_found = find_in_node_map(&importer->rebuild_map, node))) {
            pop_from_dyn_array(&stack);
            continue;
        }
        if (importer->import_pred_fn(node->type) && !find_node(importer, node->type)) {
            push_on_dyn_array(&stack, &node->type);
            goto restart;
        }
        for (size_t i = 0; i < node->op_count; ++i) {
            if (importer->import_pred_fn(node->ops[i]) && !find_node(importer, node->ops[i])) {
                push_on_dyn_array(&stack, &node->type);
                goto restart;
            }
        }
        pop_from_dyn_array(&stack);
        const Node* rebuilt_node = importer->rebuild_fn(node);
        insert_in_node_map(&importer->rebuild_map, node, (void*)rebuilt_node);
    }
    free_dyn_array(&stack);
    return last_found;
}
