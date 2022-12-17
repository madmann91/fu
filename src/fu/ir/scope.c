#include "fu/ir/scope.h"
#include "fu/ir/node.h"
#include "fu/core/dyn_array.h"

NodeSet compute_scope(const Node* param) {
    NodeSet scope = new_node_set();
    DynArray stack = new_dyn_array(sizeof(Node*));
    push_on_dyn_array(&stack, &param);
    while (stack.size != 0) {
        const Node* top = pop_from_dyn_array(&stack);
        if (top == get_param_nominal(param) || !insert_in_node_set(&scope, top))
            continue;
        for (const User* user = top->users; user; user = user->next)
            push_on_dyn_array(&stack, &user->node);
    }
    free_dyn_array(&stack);
    return scope;
}
