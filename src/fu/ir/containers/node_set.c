#include "fu/ir/containers/node_set.h"
#include "fu/ir/node.h"

NodeSet new_node_set() {
    return (NodeSet) { .hash_table = new_hash_table(sizeof(const Node*)) };
}

void free_node_set(NodeSet* node_set) {
    free_hash_table(&node_set->hash_table);
}

static bool compare_nodes(const void* left, const void* right) {
    return *(const Node**)left == *(const Node**)right;
}

bool insert_in_node_set(NodeSet* node_set, const Node* node) {
    return insert_in_hash_table(&node_set->hash_table, &node, node->id, sizeof(const Node*), compare_nodes);
}

bool find_in_node_set(const NodeSet* node_set, const Node* node) {
    return find_in_hash_table(&node_set->hash_table, &node, node->id, sizeof(const Node*), compare_nodes) != NULL;
}
