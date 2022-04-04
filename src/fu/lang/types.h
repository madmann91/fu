#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/core/format.h"

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
    TYPE_NORET,
    TYPE_ERROR,
    TYPE_TUPLE,
    TYPE_ARRAY,
    TYPE_FUN,
    TYPE_APP,
    TYPE_STRUCT,
    TYPE_ENUM,
    TYPE_ALIAS,
    TYPE_SIG,
    TYPE_PTR,
    TYPE_VAR
} TypeTag;

typedef struct TypeMember {
    const char* name;
    const Type* type;
    bool is_type;
} TypeMember;

struct Type {
    TypeTag tag;
    bool contains_error : 1;
    bool contains_unknown : 1;
    size_t id;
    union {
        struct {
            TypeMember* members;
            size_t member_count;
            const Type** type_params;
            size_t type_param_count;
            const char* name;
        } struct_type, enum_type;
        struct {
            const TypeMember* members;
            size_t member_count;
            const Type** type_params;
            size_t type_param_count;
        } sig_type;
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
            const Type** args;
            size_t arg_count;
        } tuple_type;
        struct {
            const Type* elem_type;
        } array_type;
        struct {
            const Type** type_params;
            size_t type_param_count;
            const Type* aliased_type;
            const char* name;
        } alias_type;
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

bool is_prim_type(TypeTag);
bool is_compound_type(TypeTag);
bool is_nominal_type(TypeTag);
bool is_float_type(TypeTag);
bool is_unsigned_int_type(TypeTag);
bool is_signed_int_type(TypeTag);
bool is_int_type(TypeTag);
bool is_int_or_float_type(TypeTag);
bool is_subtype(const Type*, const Type*);
bool is_non_const_ptr_type(const Type*);

size_t get_prim_type_bitwidth(TypeTag);
size_t find_type_member_index(const Type*, const char*);
const TypeMember* get_type_member(const Type*, size_t);

void print_type(FormatState*, const Type*);

#ifndef NDEBUG
void dump_type(const Type*);
#endif

#endif
