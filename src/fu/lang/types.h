#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/core/format.h"
#include "fu/core/hash_table.h"

#include <stddef.h>
#include <stdbool.h>

/*
 * Front-end types, including a simple module system and HM-style polymorphism.
 * Types should always be created via a `TypeTable` object.
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

typedef struct Type Type;
typedef struct TypeTable TypeTable;

typedef enum {
    KIND_TYPE,
    KIND_NAT
} Kind;

typedef enum {
#define f(name, ...) TYPE_##name,
    PRIM_TYPE_LIST(f)
#undef f
    TYPE_UNKNOWN,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_ALIAS,
    TYPE_SIGNATURE,
    TYPE_PTR,
    TYPE_VAR,
    TYPE_NORET,
    TYPE_ERROR,
    TYPE_ARRAY,
    TYPE_FUN,
    TYPE_TUPLE,
    TYPE_APP
} TypeTag;

typedef struct SignatureMember {
    const char* name;
    const Type* type;
    bool is_type;
} SignatureMember;

typedef struct StructField {
    const char* name;
    const Type* type;
    bool has_default;
} StructField;

typedef struct EnumOption {
    const char* name;
    const Type* param_type;
} EnumOption;

struct Type {
    TypeTag tag;
    bool contains_error : 1;
    bool contains_unknown : 1;
    size_t id;
    union {
        struct {
            const char* name;
            const Type** type_params;
            size_t type_param_count;
            const Type* aliased_type;
        } type_alias;
        struct {
            const char* name;
            const Type* parent;
            const Type** type_params;
            size_t type_param_count;
            EnumOption* options;
            size_t option_count;
        } enum_type;
        struct {
            const char* name;
            const Type* parent;
            const Type** type_params;
            size_t type_param_count;
            StructField* fields;
            size_t field_count;
            const Type* parent_enum;
        } struct_type;
        struct {
            const Type** args;
            size_t arg_count;
        } tuple_type;
        struct {
            const Type** type_params;
            size_t type_param_count;
            SignatureMember* members;
            size_t member_count;
        } signature;
        struct {
            const Type* applied_type;
            const Type** args;
            size_t arg_count;
        } type_app;
        struct {
            const Type** type_params;
            size_t type_param_count;
            const Type* dom;
            const Type* codom;
        } fun_type;
        struct {
            const Type* elem_type;
        } array_type;
        struct {
            bool is_const;
            const Type* pointee;
        } ptr_type;
        struct {
            Kind kind;
            const char* name;
        } type_var;
    };
};

typedef HashTable TypeMap;

TypeMap new_type_map(void);
void free_type_map(TypeMap*);
bool insert_type_in_map(TypeMap*, const Type*, const Type*);
const Type* find_type_in_map(const TypeMap*, const Type*);

bool is_prim_type(TypeTag);
bool is_nominal_type(TypeTag);
bool is_float_type(TypeTag);
bool is_unsigned_int_type(TypeTag);
bool is_signed_int_type(TypeTag);
bool is_int_type(TypeTag);
bool is_int_or_float_type(TypeTag);
bool is_subtype(const Type*, const Type*);
bool is_non_const_ptr_type(const Type*);
bool is_struct_like_option(const EnumOption*);

const Type* skip_type_app(const Type*);
size_t get_prim_type_bitwidth(TypeTag);
size_t get_type_param_count(const Type*);

int compare_signature_members(const void*, const void*);
int compare_signature_members_by_name(const void*, const void*);
int compare_struct_fields_by_name(const void*, const void*);
int compare_enum_options_by_name(const void*, const void*);

const SignatureMember* find_signature_member(const Type*, const char*);
const EnumOption* find_enum_option(const Type*, const char*);
const StructField* find_struct_field(const Type*, const char*);

void print_type(FormatState*, const Type*);

#ifndef NDEBUG
void dump_type(const Type*);
#endif

#endif
