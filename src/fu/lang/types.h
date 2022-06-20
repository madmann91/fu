#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/core/format.h"
#include "fu/core/hash_table.h"

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

typedef struct Type Type;
typedef struct TypeTable TypeTable;

typedef enum TypeVariance {
    TYPE_CONSTANT = 0x00,
    TYPE_COVARIANT = 0x01,
    TYPE_CONTRAVARIANT = 0x02,
    TYPE_INVARIANT = TYPE_COVARIANT | TYPE_CONTRAVARIANT
} TypeVariance;

typedef enum {
#define f(name, ...) TYPE_##name,
    PRIM_TYPE_LIST(f)
#undef f
    KIND_STAR,
    KIND_ARROW,
    TYPE_UNKNOWN,
    TYPE_SIGNATURE,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_ALIAS,
    TYPE_PROJ,
    TYPE_PTR,
    TYPE_VAR,
    TYPE_NORET,
    TYPE_ERROR,
    TYPE_ARRAY,
    TYPE_PI,
    TYPE_FUN,
    TYPE_TUPLE,
    TYPE_APP
} TypeTag;

typedef struct SignatureMember {
    const Type* var;
    const Type* value;
} SignatureMember;

typedef struct StructField {
    const char* name;
    const Type* type;
    bool has_default : 1;
    bool is_inherited : 1;
} StructField;

typedef struct EnumOption {
    const char* name;
    const Type* param_type;
    bool is_inherited;
} EnumOption;

struct Type {
    TypeTag tag;
    bool contains_error : 1;
    bool contains_unknown : 1;
    size_t id;
    union {
        struct {
            const Type** type_params;
            size_t type_param_count;
            const Type* body;
        } arrow;
        struct {
            const char* name;
            const Type** type_params;
            size_t type_param_count;
            const Type* aliased_type;
        } alias;
        struct {
            const Type** type_params;
            size_t type_param_count;
            SignatureMember* members;
            size_t member_count;
        } signature;
        struct {
            const char* name;
            const Type* sub_type;
            const Type** type_params;
            size_t type_param_count;
            EnumOption* options;
            size_t option_count;
#ifndef NDEBUG
            bool is_frozen;
#endif
        } enum_;
        struct {
            const char* name;
            const Type* super_type;
            const Type** type_params;
            size_t type_param_count;
            StructField* fields;
            size_t field_count;
            const Type* parent_enum;
            bool is_tuple_like;
#ifndef NDEBUG
            bool is_frozen;
#endif
        } struct_;
        struct {
            const Type** args;
            size_t arg_count;
        } tuple;
        struct {
            const Type* projected_type;
            size_t index;
        } proj;
        struct {
            const Type* applied_type;
            const Type** args;
            size_t arg_count;
        } app;
        struct {
            const Type** type_params;
            size_t type_param_count;
            const Type* dom;
            const Type* codom;
        } fun;
        struct {
            const Type* elem_type;
            const Type* size;
        } array;
        struct {
            bool is_const;
            const Type* pointee;
        } ptr;
        struct {
            const Type* type;
            const char* name;
            TypeVariance variance;
        } var;
    };
};

typedef HashTable TypeMap;

TypeMap new_type_map(void);
void free_type_map(TypeMap*);
bool insert_in_type_map(TypeMap*, const Type*, void*);
void* find_in_type_map(const TypeMap*, const Type*);

bool is_prim_type(TypeTag);
bool is_nominal_type(TypeTag);
bool is_float_type(TypeTag);
bool is_unsigned_int_type(TypeTag);
bool is_signed_int_type(TypeTag);
bool is_int_type(TypeTag);
bool is_int_or_float_type(TypeTag);
bool is_kind_level_type(const Type*);
bool is_non_const_ptr_type(const Type*);
bool is_struct_like_option(const EnumOption*);
bool is_tuple_like_struct_type(const Type*);

bool is_sub_type(TypeTable*, const Type*, const Type*);
bool is_sub_struct_type(TypeTable*, const Type*, const Type*);
bool is_sub_enum_type(TypeTable*, const Type*, const Type*);

const Type* skip_app_type(const Type*);
const Type** get_type_params(const Type*);
size_t get_type_param_count(const Type*);
size_t get_prim_type_bitwidth(TypeTag);
size_t get_type_inheritance_depth(const Type*);

int compare_signature_members_by_name(const void* left, const void* right);
int compare_struct_fields_by_name(const void* left, const void* right);
int compare_enum_options_by_name(const void* left, const void* right);

const SignatureMember* find_signature_member(const Type*, const char*);
const EnumOption* find_enum_option(const Type*, const char*);
const StructField* find_struct_field(const Type*, const char*);

void print_type(FormatState*, const Type*);

#ifndef NDEBUG
void dump_type(const Type*);
#endif

#endif
