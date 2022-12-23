#ifndef FU_IR_CONTAINERS_H
#define FU_IR_CONTAINERS_H

#include "fu/core/hash_table.h"

typedef struct Node Node;
typedef struct NodeMap { HashTable hash_table; } NodeMap;
typedef struct NodeSet { HashTable hash_table; } NodeSet;

NodeMap new_node_map();
void free_node_map(NodeMap*);
void clear_node_map(NodeMap*);
bool insert_in_node_map(NodeMap*, const Node*, void*);
bool replace_in_node_map(NodeMap*, const Node*, void*);
void* find_in_node_map(const NodeMap*, const Node*);

NodeSet new_node_set();
void free_node_set(NodeSet*);
void clear_node_set(NodeSet*);
bool insert_in_node_set(NodeSet*, const Node*);
bool find_in_node_set(const NodeSet*, const Node*);

#endif
