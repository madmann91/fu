#include "fu/lang/types.h"
#include "fu/lang/type_table.h"
#include "fu/core/utils.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <string.h>

bool is_prim_type(TypeTag tag) {
    switch (tag) {
#define f(name, ...) case TYPE_##name:
        AST_PRIM_TYPE_LIST(f)
#undef f
            return true;
        default:
            return false;
    }
}

bool is_nominal_type(TypeTag tag) {
    return
        tag == TYPE_STRUCT ||
        tag == TYPE_ENUM ||
        tag == TYPE_ALIAS;
}

bool is_float_type(TypeTag tag) {
    return tag == TYPE_F32 || tag == TYPE_F64;
}

bool is_unsigned_int_type(TypeTag tag) {
    return tag == TYPE_U8 || tag == TYPE_U16 || tag == TYPE_U32 || tag == TYPE_U64;
}

bool is_signed_int_type(TypeTag tag) {
    return tag == TYPE_I8 || tag == TYPE_I16 || tag == TYPE_I32 || tag == TYPE_I64;
}

bool is_int_type(TypeTag tag) {
    return is_unsigned_int_type(tag) || is_signed_int_type(tag);
}

bool is_int_or_float_type(TypeTag tag) {
    return is_int_type(tag) || is_float_type(tag);
}

static bool are_all_subtypes(const Type** left_types, const Type** right_types, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        if (!is_subtype(left_types[i], right_types[i]))
            return false;
    }
    return true;
}

bool is_subtype(const Type* left, const Type* right) {
    if (left == right ||
        left->tag == TYPE_NORET ||
        left->tag == TYPE_UNKNOWN ||
        right->tag == TYPE_UNKNOWN)
        return true;

    if ((is_signed_int_type(left->tag) && is_signed_int_type(right->tag)) ||
        (is_unsigned_int_type(left->tag) && is_unsigned_int_type(right->tag)) ||
        (is_float_type(left->tag) && is_float_type(right->tag)))
        return get_prim_type_bitwidth(left->tag) <= get_prim_type_bitwidth(right->tag);

    if (left->tag != right->tag)
        return false;

    if (left->tag == TYPE_TUPLE && left->tuple_type.arg_count == right->tuple_type.arg_count) {
        return are_all_subtypes(
            left->tuple_type.arg_types,
            right->tuple_type.arg_types,
            left->tuple_type.arg_count);
    }

    if (left->tag == TYPE_FUN) {
        return
            is_subtype(right->fun_type.dom_type, left->fun_type.dom_type) &&
            is_subtype(left->fun_type.dom_type, right->fun_type.dom_type);
    }

    return false;
}

size_t get_prim_type_bitwidth(TypeTag tag) {
    switch (tag) {
        case TYPE_BOOL: return 1;
        case TYPE_I8: case TYPE_U8: return 8;
        case TYPE_I16: case TYPE_U16: return 16;
        case TYPE_I32: case TYPE_U32: case TYPE_F32: return 32;
        case TYPE_I64: case TYPE_U64: case TYPE_F64: return 64;
        default:
            assert(false && "invalid primitive type");
            return 0;
    }
}

static void print_type_params(FormatState* state, const Type* type_params) {
    if (type_params) {
        format(state, "[", NULL);
        for (; type_params; type_params = type_params->sibling_type) {
            print_type(state, type_params);
            if (type_params->sibling_type)
                format(state, ", ", NULL);
        }
        format(state, "]", NULL);
    }
}

static void print_many_types(FormatState* state, const char* sep, const Type** types, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        print_type(state, types[i]);
        if (i != count - 1)
            format(state, sep, NULL);
    }
}

void print_type(FormatState* state, const Type* type) {
    switch (type->tag) {
#define f(name, str) case TYPE_##name: print_keyword(state, str); break;
        AST_PRIM_TYPE_LIST(f);
        case TYPE_UNKNOWN:
            format(state, "?", NULL);
            break;
        case TYPE_ERROR:
            print_with_style(state, "<error>", error_style);
            break;
        case TYPE_NORET:
            format(state, "!", NULL);
            break;
        case TYPE_TUPLE:
            format(state, "(", NULL);
            print_many_types(state, ", ", type->tuple_type.arg_types, type->tuple_type.arg_count);
            format(state, ")", NULL);
            break;
        case TYPE_APP:
            print_type(state, type->type_app.applied_type);
            format(state, "[", NULL);
            print_many_types(state, ", ", type->tuple_type.arg_types, type->tuple_type.arg_count);
            format(state, "]", NULL);
            break;
        case TYPE_ARRAY:
            format(state, "[", NULL);
            print_type(state, type->array_type.elem_type);
            format(state, "]", NULL);
            break;
        case TYPE_PARAM:
            format(state, "{s}", (FormatArg[]) { { .s = type->type_param.name } });
            break;
        case TYPE_FUN:
            print_keyword(state, "fun");
            print_type_params(state, type->fun_type.type_params);
            if (type->fun_type.dom_type->tag == TYPE_TUPLE)
                print_type(state, type->fun_type.dom_type);
            else {
                format(state, "(", NULL);
                print_type(state, type->fun_type.dom_type);
                format(state, ")", NULL);
            }
            format(state, " -> ", NULL);
            print_type(state, type->fun_type.codom_type);
            break;
        case TYPE_ENUM:
            print_keyword(state, "enum");
            format(state, " {s}", (FormatArg[]) { { .s = type->enum_type.name } });
            print_type_params(state, type->enum_type.type_params);
            break;
        case TYPE_STRUCT:
            print_keyword(state, "struct");
            format(state, " {s}", (FormatArg[]) { { .s = type->struct_type.name } });
            print_type_params(state, type->struct_type.type_params);
            break;
        case TYPE_ALIAS:
            print_keyword(state, "type");
            format(state, " {s}", (FormatArg[]) { { .s = type->alias_type.name } });
            print_type_params(state, type->alias_type.type_params);
            break;
        default:
            assert(false && "invalid type");
            break;
    }
}

void dump_type(const Type* type) {
    FormatState state = new_format_state("    ", !is_color_supported(stdout));
    print_type(&state, type);
    write_format_state(&state, stdout);
    free_format_state(&state);
    printf("\n");
}
