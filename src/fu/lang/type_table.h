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

TypeMember make_type_member(TypeTable*, const char* name, const Type* type, bool is_type);

Type* make_struct_or_enum_type(
    TypeTable*,
    TypeTag,
    const char* name,
    size_t member_count,
    size_t type_param_count);

Type* make_alias_type(TypeTable*, const char* name, size_t type_param_count);

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_error_type(TypeTable*);
const Type* make_noret_type(TypeTable*);
const Type* make_type_var(TypeTable*, const char* name, Kind kind);
const Type* make_tuple_type(TypeTable*, const Type** args, size_t arg_count);
const Type* make_unit_type(TypeTable*);
const Type* make_type_app(TypeTable*, const Type* applied_type, const Type** args, size_t arg_count);
const Type* make_fun_type(TypeTable*, const Type* dom, const Type* codom);
const Type* make_array_type(TypeTable*, const Type* elem_type);
const Type* make_ptr_type(TypeTable*, bool is_const, const Type* pointee_type);

// Note: The members may need to be sorted before creating a signature type,
// which is why they are passed as a non-const pointer to this function.
const Type* make_sig_type(
    TypeTable*,
    TypeMember* members,
    size_t member_count,
    const Type** type_params,
    size_t type_param_count);

const Type* make_poly_fun_type(
    TypeTable*,
    const Type** type_params,
    size_t type_param_count,
    const Type* dom,
    const Type* codom);

#endif
