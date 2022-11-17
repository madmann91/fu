#ifndef FU_IR_IMPORT_H
#define FU_IR_IMPORT_H

#include "fu/ir/containers/node_map.h"

/*
 * The importer is a general method to replace/inline/import nodes in a module. It consists of a map
 * from original nodes to the ones they should be replaced with, along with functions that control
 * how replacement is performed.
 */

typedef struct Node Node;
typedef struct Importer Importer;
typedef bool (*ImportPredFn)(Importer*, const Node*);
typedef const Node* (*PreTransformFn)(Importer*, const Node*);
typedef const Node* (*RebuildFn)(Importer*, const Node*);

struct Importer {
    ImportPredFn import_pred_fn;
    PreTransformFn pre_transform_fn;
    RebuildFn rebuild_fn;
    NodeMap pre_transform_map;
    NodeMap rebuild_map;
};

Importer new_importer(ImportPredFn, PreTransformFn, RebuildFn);
void free_importer(Importer*);

const Node* find_node(Importer*, const Node*);
const Node* import_node(Importer*, const Node*);

const Node* replace_param(const Node* node, const Node* from, const Node* to);

#endif
