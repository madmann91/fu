#ifndef FU_LANG_TYPES_H
#define FU_LANG_TYPES_H

#include "fu/lang/ast.h"

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
    TYPE_PARAM,
    TYPE_ALIAS,
    TYPE_STRUCT,
    TYPE_ENUM
} TypeTag;

struct Type {
    TypeTag tag;
    bool contains_error : 1;
    bool contains_unknown : 1;
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
            const Type* type_params;
        } fun_type;
        struct {
            const Type* applied_type;
            const Type** type_args;
            size_t arg_count;
        } type_app;
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

bool is_prim_type(TypeTag);
bool is_nominal_type(TypeTag);
bool is_float_type(TypeTag);
bool is_unsigned_int_type(TypeTag);
bool is_signed_int_type(TypeTag);
bool is_int_type(TypeTag);
bool is_int_or_float_type(TypeTag);
bool is_subtype(const Type*, const Type*);
size_t get_prim_type_bitwidth(TypeTag);

void print_type(FormatState*, const Type*);
void dump_type(const Type*);

#endif
