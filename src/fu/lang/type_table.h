#ifndef FU_LANG_TYPE_TABLE_H
#define FU_LANG_TYPE_TABLE_H

#include "fu/core/hash_table.h"
#include "fu/core/str_pool.h"
#include "fu/lang/types.h"

typedef struct TypeTable {
    HashTable types;
    MemPool* mem_pool;
    StrPool str_pool;
    size_t type_count;
} TypeTable;

TypeTable new_type_table(MemPool*);
void free_type_table(TypeTable*);

Type* make_struct_type(TypeTable*, const char* name, size_t field_count);
Type* make_enum_type(TypeTable*, const char* name, size_t option_count);
Type* make_alias_type(TypeTable*, const char* name);

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_error_type(TypeTable*);
const Type* make_noret_type(TypeTable*);
const Type* make_type_param(TypeTable*, const char* name);
const Type* make_tuple_type(TypeTable*, const Type** arg_types, size_t arg_count);
const Type* make_unit_type(TypeTable*);
const Type* make_type_app(TypeTable*, const Type* applied_type, const Type** type_args, size_t arg_count);
const Type* make_fun_type(TypeTable*, const Type* dom_type, const Type* codom_type);
const Type* make_array_type(TypeTable*, const Type* elem_type);

#endif
