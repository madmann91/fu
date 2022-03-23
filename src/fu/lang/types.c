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

bool is_compound_type(TypeTag tag) {
    return tag == TYPE_STRUCT || tag == TYPE_ENUM || tag == TYPE_SIG;
}

bool is_nominal_type(TypeTag tag) {
    return tag == TYPE_STRUCT || tag == TYPE_ENUM || tag == TYPE_ALIAS;
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
            left->tuple_type.args,
            right->tuple_type.args,
            left->tuple_type.arg_count);
    }

    if (left->tag == TYPE_FUN) {
        return
            is_subtype(right->fun_type.dom, left->fun_type.dom) &&
            is_subtype(left->fun_type.codom, right->fun_type.codom);
    }

    return false;
}

bool is_non_const_ptr_type(const Type* type) {
    return type->tag == TYPE_PTR && !type->ptr_type.is_const;
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

size_t get_member_index_by_name(const Type* type, const char* name) {
    for (size_t i = 0; i < type->struct_type.member_count; ++i) {
        if (!strcmp(name, type->struct_type.members[i].name))
            return i;
    }
    return SIZE_MAX;
}

const Type* get_member_type_by_name(const Type* type, const char* name) {
    size_t index = get_member_index_by_name(type, name);
    return index == SIZE_MAX ? NULL : type->struct_type.members[index].type;
}

static void print_many_types(FormatState* state, const char* sep, const Type** types, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        print_type(state, types[i]);
        if (i != count - 1)
            format(state, sep, NULL);
    }
}

static void print_params(FormatState* state, const Type** params, size_t param_count) {
    if (param_count == 0)
        return;
    format(state, "[", NULL);
    print_many_types(state, ", ", params, param_count);
    format(state, "]", NULL);
}

static void print_member(FormatState* state, const Member* member) {
    format(state, "\n", NULL);
    print_keyword(state, member->is_type ? "type" : "const");
    format(state, " {s}", (FormatArg[]) { { .s = member->name } });
    if (member->type) {
        format(state, member->is_type ? " = " : ": ", NULL);
        print_type(state, member->type);
    }
    format(state, ";", NULL);
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
            print_many_types(state, ", ", type->tuple_type.args, type->tuple_type.arg_count);
            format(state, ")", NULL);
            break;
        case TYPE_APP:
            print_type(state, type->type_app.applied_type);
            format(state, "[", NULL);
            print_many_types(state, ", ", type->type_app.args, type->type_app.arg_count);
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
            print_params(state, type->fun_type.params, type->fun_type.param_count);
            if (type->fun_type.dom->tag == TYPE_TUPLE)
                print_type(state, type->fun_type.dom);
            else {
                format(state, "(", NULL);
                print_type(state, type->fun_type.dom);
                format(state, ")", NULL);
            }
            format(state, " -> ", NULL);
            print_type(state, type->fun_type.codom);
            break;
        case TYPE_ENUM:
            print_keyword(state, "enum");
            format(state, " {s}", (FormatArg[]) { { .s = type->enum_type.name } });
            break;
        case TYPE_STRUCT:
            print_keyword(state, "struct");
            format(state, " {s}", (FormatArg[]) { { .s = type->struct_type.name } });
            break;
        case TYPE_SIG:
            print_keyword(state, "sig");
            print_params(state, type->sig_type.params, type->sig_type.param_count);
            format(state, " {{{>}", NULL);
            for (size_t i = 0; i < type->sig_type.member_count; ++i)
                print_member(state, type->sig_type.members + i);
            format(state, "{<}\n}", NULL);
            break;
        case TYPE_PTR:
            format(state, "&", NULL);
            if (type->ptr_type.is_const) {
                print_keyword(state, "const");
                format(state, " ", NULL);
            }
            print_type(state, type->ptr_type.pointee);
            break;
        default:
            assert(false && "invalid type");
            break;
    }
}

#ifndef NDEBUG // GCOV_EXCL_START
void dump_type(const Type* type) {
    FormatState state = new_format_state("    ", !is_color_supported(stdout));
    print_type(&state, type);
    write_format_state(&state, stdout);
    free_format_state(&state);
    printf("\n");
}
#endif // GCOV_EXCL_STOP
