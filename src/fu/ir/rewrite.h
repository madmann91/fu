#ifndef FU_IR_IMPORT_H
#define FU_IR_IMPORT_H

#include "fu/ir/containers/node_map.h"
#include "fu/core/dyn_array.h"

/*
 * The importer is a general method to replace/inline/import nodes in a module. It consists of a map
 * from original nodes to the ones they should be replaced with, along with functions that control
 * how replacement is performed.
 */

typedef struct Node Node;
typedef struct Rewriter Rewriter;
typedef const Node* (*RewriteFn)(Rewriter*, const Node*);

struct Rewriter {
    RewriteFn rewrite_fn;
    DynArray rewrite_stack;
    DynArray op_buf;
    NodeMap* rewritten_nodes;
};

Rewriter new_rewriter(NodeMap*, RewriteFn);
void free_rewriter(Rewriter*);

const Node* rewrite_node(Rewriter*, const Node*);
const Node* find_rewritten_node(Rewriter*, const Node*);
bool find_rewritten_nodes(Rewriter*, const Node*const*, size_t, RewriteFn);

/// Replaces every occurence of the given parameter `from` in `node` to `to`.
/// Nominal nodes are re-created in the process, except for axioms which are kept the same.
const Node* replace_param(const Node* node, const Node* from, const Node* to);

/// From a type that may contain singletons, manifest a value. Parts of the type for which no value
/// exists get replaced with error values of the corresponding type.
const Node* manifest_singletons(const Node* type);

#endif
