#ifndef FU_IR_NODE_MAP_H
#define FU_IR_NODE_MAP_H

#include "fu/core/hash_table.h"

typedef struct Node Node;
typedef struct NodeSet { HashTable hash_table; } NodeSet;

NodeSet new_node_set();
void free_node_set(NodeSet*);
bool insert_in_node_set(NodeSet*, const Node*);
bool find_in_node_set(const NodeSet*, const Node*);

#endif
