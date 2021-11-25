#ifndef FU_LANG_TYPE_H
#define FU_LANG_TYPE_H

#include <stddef.h>

struct ast;

enum prim_type_tag {
    PRIM_TYPE_BOOL,
    PRIM_TYPE_I8,
    PRIM_TYPE_I16,
    PRIM_TYPE_I32,
    PRIM_TYPE_I64,
    PRIM_TYPE_U8,
    PRIM_TYPE_U16,
    PRIM_TYPE_U32,
    PRIM_TYPE_U64,
    PRIM_TYPE_F32,
    PRIM_TYPE_F64
};

struct type {
    enum type_tag {
        TYPE_PRIM,
        TYPE_TUPLE,
        TYPE_STRUCT,
        TYPE_ENUM,
        TYPE_FUN
    } tag;
    union {
        struct {
            enum prim_type_tag tag;
        } prim;
        struct {
            const struct type* args;
            size_t arg_count;
        } tuple;
        struct {
            const struct type* from;
            const struct type* to;
        } fun;
        struct {
            struct ast* decl;
        } struct_, enum_;
    };
};

#endif
