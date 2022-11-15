#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

#include "fu/ir/node.h"

typedef struct Module Module;

Module* new_module();
void free_module(Module*);

const DebugInfo* make_debug_info(Module*, const char*, void*, const FileLoc*);

//=================================== FREE PARAMS =======================================

const Node* make_empty_free_params(Module*);
const Node* make_single_free_param(const Node*);
const Node* make_free_params(Module*, const Node**, size_t);
const Node* merge_free_params(const Node*, const Node*);

//================================== NOMINAL NODES ======================================

Node* make_nominal_pi(const Node*);
Node* make_nominal_sigma(const Node*, size_t);
Node* make_lambda(const Node*);

//================================ STRUCTURAL NODES =====================================

const Node* make_param(Node*, const DebugInfo*);
const Node* make_label(const Node*, const char*, const DebugInfo*);

const Node* make_star(Module*);
const Node* make_singleton(const Node*);
const Node* make_nat(Module*);
const Node* make_int(Module*, size_t);
const Node* make_float(Module*, size_t);

const Node* make_nat_const(Module*, IntVal);
const Node* make_int_const(const Node* type, IntVal);
const Node* make_float_const(const Node* type, FloatVal);

const Node* make_proj(const Node*, size_t, const DebugInfo*);
const Node* make_app(const Node*, const Node*, const DebugInfo*);

const Node* make_pi(const Node* dom, const Node* codom, const DebugInfo*);
const Node* make_empty_sigma(Module*, const Node*, const DebugInfo*);
const Node* make_sigma(const Node**, size_t, const DebugInfo*);

const Node* make_tuple(const Node*, const Node**, size_t, const DebugInfo*);

#endif
