#ifndef FU_IR_MODULE_H
#define FU_IR_MODULE_H

#include "fu/ir/node.h"

typedef struct Module Module;

Module* new_module();
void free_module();

//=================================== FREE PARAMS =======================================

const Node* make_empty_free_params(Module*);
const Node* make_single_free_param(const Node*);
const Node* make_free_params(Module*, const Node**, size_t);
const Node* merge_free_params(Module*, const Node*, const Node*);

//================================== NOMINAL NODES ======================================

Node* make_nominal_pi(const Node*, const Node*);
Node* make_nominal_sigma(size_t);
Node* make_lambda(const Node*);

//================================ STRUCTURAL NODES =====================================

const Node* make_param(Node*);

const Node* make_star(Module*);
const Node* make_singleton(Module*);

const Node* make_bot(Module*);
const Node* make_top(Module*);
const Node* make_noret(Module*);
const Node* make_mem(Module*);
const Node* make_err(Module*);
const Node* make_nat(Module*);
const Node* make_nat_const(IntVal);
const Node* make_int_const(const Node* type, IntVal);
const Node* make_float_const(const Node* type, FloatVal);

const Node* make_int(Module*, size_t);
const Node* make_float(Module*, size_t);
const Node* make_array_type(const Node*);

const Node* make_pi(const Node* dom, const Node* codom, const Debug*);
const Node* make_sigma(const Node**, size_t, const Debug*);
const Node* make_tuple(const Node**, size_t, const Debug*);
const Node* make_extract(const Node* value, const Node* index, const Debug*);
const Node* make_insert(const Node* value, const Node* index, const Node* elem, const Debug*);

const Node* make_array(const Node**, size_t, const Debug*);
const Node* make_app(const Node* callee, const Node* arg, const Debug*);
const Node* make_start(const Node* block);

const Node* make_cast_op(NodeTag, const Node* type, const Node* value, const Debug*);

const Node* make_int_arith_op(NodeTag, const Node*, const Node*, const Debug*);
const Node* make_int_bit_op(NodeTag, const Node*, const Node*, const Debug*);
const Node* make_int_cmp_op(NodeTag, const Node*, const Node*, const Debug*);
const Node* make_int_div_op(NodeTag, const Node* err, const Node*, const Node*, const Debug*);

const Node* make_float_arith_op(NodeTag, FloatMode, const Node*, const Node*, const Debug*);
const Node* make_float_div_op(NodeTag, FloatMode, const Node*, const Node*, const Node*, const Debug*);
const Node* make_float_cmp_op(NodeTag, const Node*, const Node*, const Debug*);

#endif
