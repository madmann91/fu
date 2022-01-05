#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/lang/ast.h"
#include "fu/core/str_pool.h"
#include "fu/core/hash_table.h"

/*
 * Types are uniquely stored and hashed in a `TypeTable` object, using a process called hash-consing.
 * The idea is that whenever a type that already exists is requested, the existing type is returned,
 * instead of allocating a new copy.
 */

typedef struct Type Type;

typedef enum {
#define f(name, ...) TYPE_##name,
    AST_PRIM_TYPE_LIST(f)
#undef f
    TYPE_UNKNOWN,
    TYPE_TUPLE,
    TYPE_ARRAY,
    TYPE_FUN,
    TYPE_PARAM,
    TYPE_ALIAS,
    TYPE_STRUCT,
    TYPE_ENUM
} TypeTag;

struct Type {
    TypeTag tag;
    size_t id;
    const Type* parent_type;
    const Type* sibling_type;
    union {
        struct {
            const Type** arg_types;
            size_t arg_count;
        } tuple_type;
        struct {
            const Type* elem_type;
        } array_type;
        struct {
            const Type* dom_type;
            const Type* codom_type;
        } fun_type;
        struct {
            const char* name;
        } type_param;
        struct {
            const char* name;
            const Type** members;
            const char** member_names;
            size_t member_count;
            const Type* child_types;
            const Type* type_params;
        } enum_type, struct_type;
        struct {
            const char* name;
            const Type* type_params;
            const Type* aliased_type;
        } alias_type;
    };
};

typedef struct {
    HashTable types;
    MemPool* mem_pool;
    StrPool str_pool;
    size_t type_count;
} TypeTable;

bool is_prim_type(TypeTag);
void set_member_name(TypeTable*, Type*, size_t, const char*);

TypeTable new_type_table(MemPool*);
void free_type_table(TypeTable*);

Type* make_struct_type(TypeTable*, const char* name, size_t field_count);
Type* make_enum_type(TypeTable*, const char* name, size_t option_count);
Type* make_alias_type(TypeTable*, const char* name);

const Type* make_prim_type(TypeTable*, TypeTag);
const Type* make_unknown_type(TypeTable*);
const Type* make_type_param(TypeTable*, const char* name);
const Type* make_tuple_type(TypeTable*, const Type** arg_types, size_t arg_count);
const Type* make_fun_type(TypeTable*, const Type* dom_type, const Type* codom_type);
const Type* make_array_type(TypeTable*, const Type* elem_type);

void print_type(FormatState*, const Type*);
void dump_type(const Type*);

#endif
