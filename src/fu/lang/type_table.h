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

void set_type_member_name(TypeTable*, Type*, size_t, const char*);

Type* make_struct_type(TypeTable*, const char* name, size_t field_count, size_t param_count);
Type* make_enum_type(TypeTable*, const char* name, size_t option_count, size_t param_count);
Type* make_alias_type(TypeTable*, const char* name);

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_error_type(TypeTable*);
const Type* make_noret_type(TypeTable*);
const Type* make_type_param(TypeTable*, const char* name);
const Type* make_tuple_type(TypeTable*, const Type** args, size_t arg_count);
const Type* make_unit_type(TypeTable*);
const Type* make_type_app(TypeTable*, const Type* applied_type, const Type** args, size_t arg_count);
const Type* make_fun_type(TypeTable*, const Type* dom, const Type* codom);
const Type* make_array_type(TypeTable*, const Type* elem_type);
const Type* make_sig_type(TypeTable*, const Member*, size_t, const Type**, size_t);

#endif
