#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

#include "fu/ir/node.h"

typedef struct Module Module;

Module* new_module();
void free_module(Module*);

const Node* rebuild_node(
    NodeTag,
    const Node*,
    const Node*const*, size_t,
    const NodeData*,
    const DebugInfo*);

//==================================== DEBUG INFO =======================================

const DebugInfo* make_debug_info(Module*, const char*, void*, const FileLoc*);
const DebugInfo* import_debug_info(Module*, const DebugInfo*);

//================================== NOMINAL NODES ======================================

Node* make_nominal_pi(const Node*);
Node* make_nominal_sigma(const Node*, size_t);
Node* make_nominal_variant(const Node*, size_t);
Node* make_lambda(const Node*);
Node* make_axiom(const Node*);

//================================ STRUCTURAL NODES =====================================

const Node* make_param(const Node* nominal, const DebugInfo*);
const Node* make_label(const Node* value, const char* label, const DebugInfo*);

const Node* make_star(Module*);
const Node* make_noret(Module*);
const Node* make_error(const Node* type);
const Node* make_top(const Node* type);
const Node* make_bottom(const Node* type);
const Node* make_singleton(const Node*);
const Node* make_nat(Module*);
const Node* make_int(Module*, size_t bitwidth);
const Node* make_float(Module*, size_t bitwidth);

const Node* make_nat_const(Module*, IntVal);
const Node* make_int_const(const Node* type, IntVal);
const Node* make_float_const(const Node* type, FloatVal);

const Node* make_proj(const Node*, size_t, const DebugInfo*);
const Node* make_app(const Node*, const Node*, const DebugInfo*);

const Node* make_pi(const Node* dom, const Node* codom, const DebugInfo*);
const Node* make_empty_sigma(const Node*, const DebugInfo*);
const Node* make_sigma(const Node**, size_t, const DebugInfo*);
const Node* make_variant(const Node**, size_t, const DebugInfo*);

const Node* make_tuple(const Node* type, const Node**, size_t, const DebugInfo*);

#endif