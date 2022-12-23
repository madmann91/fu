#ifndef FU_IR_SCOPE_H
#define FU_IR_SCOPE_H

#include "fu/ir/containers.h"

/// Computes the scope of a parameter, which is the set of nodes that directly or indirectly depend
/// on it. The caller is responsible for destroying the resulting set.
NodeSet compute_scope(const Node* param);

#endif
