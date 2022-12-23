#ifndef FU_LANG_TYPE_TABLE_H
#define FU_LANG_TYPE_TABLE_H

#include "fu/lang/types.h"

/*
 * The type-table is a factory object for types. Types that can be structurally compared are
 * created uniquely, and other (i.e. nominal) types are created every time they are requested.
 */

typedef struct TypeTable { Module* module; } TypeTable;

TypeTable make_type_table(Module*);

Type make_star_kind(TypeTable*);
Type make_arrow_kind(TypeTable*, const Type*, size_t, Type);
Type make_singleton_kind(TypeTable*, Type);
Type make_unknown_type(TypeTable*);
Type make_error_type(TypeTable*);
Type make_top_type(TypeTable*);
Type make_bottom_type(TypeTable*);
Type make_noret_type(TypeTable*);
Type make_prim_type(TypeTable*, PrimTypeTag);
Type make_unit_type(TypeTable*);
Type make_tuple_type(TypeTable*, const Type*, size_t);
Type make_unsized_array_type(TypeTable*, Type);
Type make_sized_array_type(TypeTable*, Type, size_t);
Type make_ptr_type(TypeTable*, Type, bool);
Type make_fun_type(TypeTable*, Type, Type);
Type make_poly_fun_type(TypeTable*, const Type*, size_t, Type, Type);
Type make_struct_type(TypeTable*, size_t, const char*);
Type make_enum_type(TypeTable*, size_t, const char*);
Type make_signature_type(TypeTable*, size_t, const char*);
Type make_app_type(TypeTable*, Type, const Type*, size_t);
Type make_proj_type(TypeTable*, Type, size_t);

#endif
