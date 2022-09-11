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

const Kind* make_star_kind(TypeTable*);
const Kind* make_arrow_kind(TypeTable*,
    const Kind** kind_params,
    size_t kind_param_count,
    const Kind* body);

const Kind* make_type_ctor_kind(TypeTable*,
    const Type** type_params,
    size_t type_param_count,
    const Kind* body);

//================================== NOMINAL TYPES =======================================

Type* make_var_type(TypeTable*, const char* name);
Type* make_var_type_with_kind(TypeTable*, const char* name, const Kind*);
Type* make_var_type_with_value(TypeTable*, const char* name, const Type*);
Type* make_struct_type(TypeTable*, const char* name);
Type* make_enum_type(TypeTable*, const char* name);
Type* make_signature_type(TypeTable*);
const Type* seal_struct_type(TypeTable*, Type*);
const Type* seal_enum_type(TypeTable*, Type*);
const Type* seal_signature_type(TypeTable*, Type*);

Type* copy_struct_type(TypeTable*, const Type*);
Type* copy_enum_type(TypeTable*, const Type*);
Type* copy_signature_type(TypeTable*, const Type*);

//================================= STRUCTURAL TYPES =====================================

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_error_type(TypeTable*);
const Type* make_noret_type(TypeTable*);
const Type* make_tuple_type(TypeTable*, const Type** args, size_t arg_count);
const Type* make_unit_type(TypeTable*);
const Type* make_app_type(TypeTable*, const Type* applied_type, const Type** args, size_t arg_count);
const Type* make_sized_array_type(TypeTable*, const Type* elem_type, const Type* size);
const Type* make_unsized_array_type(TypeTable*, const Type* elem_type);
const Type* make_ptr_type(TypeTable*, bool is_const, const Type* pointee_type);
const Type* make_fun_type(TypeTable*, const Type* dom, const Type* codom);
const Type* make_proj_type(TypeTable*, const Type* projected_type, size_t index);
const Type* make_mod_type(TypeTable*, const char*, Type* signature);

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

const Type* replace_types(
    TypeTable*,
    const Type* type,
    const Type** from,
    const Type** to,
    size_t mapped_types_count);

#endif
