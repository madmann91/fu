#ifndef FU_LANG_TYPE_TABLE_H
#define FU_LANG_TYPE_TABLE_H

#include "fu/lang/types.h"

/*
 * The type-table is a factory object for types. Types that can be structurally compared are
 * created uniquely, and other (i.e. nominal) types are created every time they are requested.
 */

typedef struct TypeTable TypeTable;
typedef struct MemPool MemPool;

TypeTable* new_type_table(MemPool*);
void free_type_table(TypeTable*);

//====================================== KINDS ===========================================

const Kind* make_nat_kind(TypeTable*);
const Kind* make_type_kind(TypeTable*);

const Kind* make_functor_kind(TypeTable*,
    const Type** type_params,
    size_t type_param_count,
    const Kind* body);

const Kind* make_signature_kind(
    TypeTable*,
    SignatureMember* members,
    size_t member_count);

//================================== NOMINAL TYPES =======================================

Type* make_struct_type(TypeTable*, const char* name);
Type* make_enum_type(TypeTable*, const char* name);
const Type* freeze_struct_type(TypeTable*, Type*);
const Type* freeze_enum_type(TypeTable*, Type*);

//================================= STRUCTURAL TYPES =====================================

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_error_type(TypeTable*);
const Type* make_noret_type(TypeTable*);
const Type* make_var_type(TypeTable*, const char* name, const Kind* kind);
const Type* make_tuple_type(TypeTable*, const Type** args, size_t arg_count);
const Type* make_unit_type(TypeTable*);
const Type* make_app_type(TypeTable*, const Type* applied_type, const Type** args, size_t arg_count);
const Type* make_array_type(TypeTable*, const Type* elem_type);
const Type* make_ptr_type(TypeTable*, bool is_const, const Type* pointee_type);
const Type* make_fun_type(TypeTable*, const Type* dom, const Type* codom);
const Type* make_module_type(TypeTable*, const char* name, const Type* signature);
const Type* make_proj_type(TypeTable*, const Type* projected_type, size_t index);

const Type* make_alias_type(
    TypeTable*,
    const char* name,
    const Type** type_params,
    size_t type_param_count,
    const Type* aliased_type);

const Type* make_poly_fun_type(
    TypeTable*,
    const Type** type_params,
    size_t type_param_count,
    const Type* dom,
    const Type* codom);

//================================== SUBSTITUTION ========================================

const Type* replace_types_with_map(TypeTable*, const Type*, TypeMap*);
const Type* replace_types(TypeTable*, const Type*, const Type**, const Type**, size_t);

#endif
