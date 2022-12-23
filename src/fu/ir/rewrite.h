#ifndef FU_IR_REWRITE_H
#define FU_IR_REWRITE_H

#include "fu/ir/containers.h"
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
    NodeSet* scope;
};

Rewriter new_rewriter(NodeMap*, NodeSet*, RewriteFn);
void free_rewriter(Rewriter*);

const Node* rewrite_node(Rewriter*, const Node*);
const Node* find_rewritten_node(Rewriter*, const Node*);
bool find_rewritten_nodes(Rewriter*, const Node*const*, size_t);

/// General replacement method that replaces nodes in the map with their associated value.
/// If `scope` is not null, rewritting is restricted to the nodes that are inside that set.
const Node* replace_nodes(const Node* node, NodeMap* node_map, NodeSet* scope);

#endif
