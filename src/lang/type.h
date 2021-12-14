#ifndef FU_LANG_TYPE_H
#define FU_LANG_TYPE_H

#include <stddef.h>

#include "core/log.h"

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

struct type;
struct type_table;
struct mem_pool;

struct type_member {
    const char* name;
    const struct type* type;
};

struct type {
    enum type_tag {
#define f(x, ...) TYPE_##x,
        PRIM_TYPE_LIST(f)
#undef f
        TYPE_ERROR,
        TYPE_NORET,
        TYPE_TUPLE,
        TYPE_ARRAY,
        TYPE_STRUCT,
        TYPE_ENUM,
        TYPE_FUN,
        TYPE_VAR
    } tag;
    bool contains_error : 1;
    union {
        struct {
            const struct type** args;
            size_t arg_count;
        } tuple_type;
        struct {
            const struct type* elem;
            size_t elem_count;
        } array_type;
        struct {
            const struct type* dom;
            const struct type* codom;
        } fun_type;
        struct {
            const char* name;
        } type_var;
        struct {
            const char* name;
            struct type_member* members;
            const struct type* type_vars;
            const struct type** subtypes;
            size_t member_count;
            size_t subtype_count;
            size_t type_var_count;
            struct file_loc loc;
        } struct_type, enum_type;
    };
};

bool is_prim_type(const struct type*);
bool is_nominal_type(const struct type*);

struct type_table* new_type_table(void);
void free_type_table(struct type_table*);

const struct type* make_error_type(struct type_table*);
const struct type* make_noret_type(struct type_table*);
const struct type* make_prim_type(struct type_table*, enum type_tag);
const struct type* make_unit_type(struct type_table*);
const struct type* make_tuple_type(struct type_table*, const struct type** args, size_t arg_count);
const struct type* make_array_type(struct type_table*, const struct type* elem, size_t arg_count);
const struct type* make_fun_type(struct type_table*, const struct type* dom, const struct type* codom);

const struct type* new_type_var(struct type_table*, const char* name);

struct type* new_struct_or_enum_type(
    struct type_table*,
    enum type_tag,
    const char* name,
    size_t member_count,
    size_t subtype_count,
    size_t type_var_count,
    struct file_loc* loc);

bool is_subtype(const struct type*, const struct type*);
void print_type(struct format_state*, const struct type*);

#endif
