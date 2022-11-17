#ifndef FU_IR_CONTAINERS_NODE_MAP_H
#define FU_IR_CONTAINERS_NODE_MAP_H

#include "fu/core/hash_table.h"

typedef struct Node Node;
typedef struct NodeMap { HashTable hash_table; } NodeMap;

NodeMap new_node_map();
void free_node_map(NodeMap*);
bool insert_in_node_map(NodeMap*, const Node*, void*);
bool replace_in_node_map(NodeMap*, const Node*, void*);
void* find_in_node_map(const NodeMap*, const Node*);

#endif
