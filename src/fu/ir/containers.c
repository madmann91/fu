#include "fu/ir/containers.h"
#include "fu/ir/node.h"

typedef struct {
    const Node* from;
    void* to;
} NodeMapPair;

NodeMap new_node_map() {
    return (NodeMap) { .hash_table = new_hash_table(sizeof(NodeMapPair)) };
}

void free_node_map(NodeMap* node_map) {
    free_hash_table(&node_map->hash_table);
}

void clear_node_map(NodeMap* node_map) {
    clear_hash_table(&node_map->hash_table);
}

static bool compare_node_map_pairs(const void* left, const void* right) {
    return ((const NodeMapPair*)left)->from == ((const NodeMapPair*)right)->from;
}

bool insert_in_node_map(NodeMap* node_map, const Node* from, void* to) {
    return insert_in_hash_table(&node_map->hash_table,
        &(NodeMapPair) { .from = from, .to = to }, from->id,
        sizeof(NodeMapPair), compare_node_map_pairs);
}

bool replace_in_node_map(NodeMap* node_map, const Node* from, void* to) {
    return replace_in_hash_table(&node_map->hash_table,
        &(NodeMapPair) { .from = from, .to = to }, from->id,
        sizeof(NodeMapPair), compare_node_map_pairs);
}

void* find_in_node_map(const NodeMap* node_map, const Node* node) {
    NodeMapPair* elem = find_in_hash_table(&node_map->hash_table, &(NodeMapPair) { .from = node },
        node->id, sizeof(NodeMapPair), compare_node_map_pairs);
    return elem ? elem->to : NULL;
}

NodeSet new_node_set() {
    return (NodeSet) { .hash_table = new_hash_table(sizeof(const Node*)) };
}

void free_node_set(NodeSet* node_set) {
    free_hash_table(&node_set->hash_table);
}

void clear_node_set(NodeSet* node_set) {
    clear_hash_table(&node_set->hash_table);
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
