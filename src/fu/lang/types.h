#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/lang/ast.h"

/*
 * Front-end types, including a simple module system and HM-style polymorphism.
 * Types should always be created via a `TypeTable` object.
 */

typedef struct Type Type;
typedef struct TypeTable TypeTable;

typedef enum {
#define f(name, ...) TYPE_##name,
    AST_PRIM_TYPE_LIST(f)
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

typedef struct StructOrEnumMember {
    const char* name;
    const Type* type;
} StructOrEnumMember;

typedef struct SigMember {
    const char* name;
    const Type* type;
    bool is_type;
} SigMember;

struct Type {
    TypeTag tag;
    bool contains_error : 1;
    bool contains_unknown : 1;
    size_t id;
    union {
        struct {
            StructOrEnumMember* members;
            size_t member_count;
            const Type** type_params;
            size_t type_param_count;
            const char* name;
        } struct_type, enum_type;
        struct {
            const SigMember* members;
            size_t member_count;
            const Type** type_params;
            size_t type_param_count;
            const Type** exist_vars;
            size_t exist_var_count;
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
            const Type* aliased_type;
        } alias_type;
        struct {
            bool is_const;
            const Type* pointee;
        } ptr_type;
        struct {
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
size_t get_member_index_by_name(const Type*, const char*);
const Type* get_member_type_by_name(const Type*, const char*);

void print_type(FormatState*, const Type*);

#ifndef NDEBUG
void dump_type(const Type*);
#endif

#endif
