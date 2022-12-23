#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/core/format.h"
#include "fu/ir/containers.h"
#include "fu/ir/rewrite.h"
#include "fu/ir/node.h"

#include <stddef.h>
#include <stdbool.h>

/*
 * Front-end types, including a simple module system based on M. Lillibridge's translucent
 * sums, and HM-style polymorphism. Types should always be created via a `TypeTable` object.
 */

#define PRIM_TYPE_LIST(f) \
    f(BOOL, "bool") \
    f(I8,   "i8") \
    f(I16,  "i16") \
    f(I32,  "i32") \
    f(I64,  "i64") \
    f(U8,   "u8") \
    f(U16,  "u16") \
    f(U32,  "u32") \
    f(U64,  "u64") \
    f(F32,  "f32") \
    f(F64,  "f64")

typedef enum {
#define f(tag, str) TYPE_##tag,
    PRIM_TYPE_LIST(f)
#undef f
} PrimTypeTag;

typedef NodeSet TypeSet;
typedef NodeMap TypeMap;
#define new_type_map new_node_map
#define new_type_set new_node_set
#define free_type_map free_node_map
#define free_type_set free_node_set
#define find_in_type_map find_in_node_map
#define find_in_type_set find_in_node_set
#define insert_in_type_map insert_in_node_map
#define insert_in_type_set insert_in_node_set
#define find_in_type_map find_in_node_map
#define find_in_type_set find_in_node_set
#define clear_type_map clear_node_map
#define clear_type_set clear_node_set
#define replace_types replace_nodes

typedef const Node* Type;

typedef enum {
    TYPE_CONSTANT      = 0x0,
    TYPE_COVARIANT     = 0x1,
    TYPE_CONTRAVARIANT = 0x2,
    TYPE_INVARIANT     = TYPE_COVARIANT | TYPE_CONTRAVARIANT
} TypeVariance;

typedef struct {
    Type lower, upper;
} TypeBounds;

enum TypeLevel {
    LEVEL_VALUE,
    LEVEL_TYPE,
    LEVEL_KIND,
    LEVEL_UNIVERSE
};

const char* get_type_name(Type);

bool is_unknown_type(Type);
bool is_valid_type(Type);
bool is_int_type(Type);
bool is_float_type(Type);
bool is_int_or_float_type(Type);
bool is_prim_type(Type);
bool is_unit_type(Type);
bool is_tuple_type(Type);
bool is_fun_type(Type);
bool is_struct_type(Type);
bool is_enum_type(Type);
bool is_signature_type(Type);
bool is_sub_type(Type, Type);
bool is_struct_like_option(Type, size_t);
bool is_tuple_like_struct_type(Type);
bool is_singleton_kind(Type);
bool is_ptr_type(Type);
bool is_const_ptr_type(Type);

size_t get_prim_type_bitwidth(Type);

TypeLevel get_type_level(Type);

Type resolve_type(Type);
Type get_applied_type(Type);
Type get_singleton_kind_value(Type);

Type get_fun_type_dom(Type);
Type get_fun_type_codom(Type);

size_t get_tuple_type_arg_count(Type);
size_t get_struct_type_field_count(Type);
size_t get_enum_type_option_count(Type);
size_t get_signature_member_count(Type);

size_t find_struct_field(Type, const char*);
size_t find_enum_option(Type, const char*);
size_t find_signature_member(Type, const char*);

Type get_tuple_type_arg(Type, size_t);
Type get_struct_field_type(Type, size_t);
Type get_enum_option_type(Type, size_t);
Type get_signature_member_type(Type, size_t);

const char* get_struct_field_name(Type, size_t);
const char* get_enum_option_name(Type, size_t);
const char* get_signature_member_name(Type, size_t);

void set_struct_field_type(Type, size_t, const char*, Type);
void set_enum_option_type(Type, size_t, const char*, Type);
void set_signature_member_type(Type, size_t, const char*, Type);

size_t get_type_param_count(Type);
Type get_type_param(Type, size_t);

Type copy_signature_type(Type);

TypeBounds* compute_type_param_bounds(Type fun_type, Type arg_type);
TypeVariance* compute_type_param_variance(Type fun_type);

void print_type(FormatState*, Type);

#ifndef NDEBUG
void dump_type(Type);
#endif

#endif
