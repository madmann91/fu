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

void set_type_member_name(TypeTable* type_table, Type* type, size_t i, const char* name) {
    assert(i < type->struct_type.member_count);
    type->struct_type.member_names[i] = make_str(&type_table->str_pool, name);
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

static const Type** merge_many_types(
    TypeTable* type_table,
    const Type** from_types,
    const Type** to_types,
    size_t count,
    bool is_join)
{
    const Type** merged_types = malloc_or_die(sizeof(Type*) * count);
    for (size_t i = 0; i < count; ++i)
        merged_types[i] = merge_types(type_table, from_types[i], to_types[i], is_join);
    return merged_types;
}

const Type* merge_types(TypeTable* type_table, const Type* from, const Type* to, bool is_join) {
    if (from == to) return to;
    if (to->tag == TYPE_UNKNOWN) return from;

    if ((is_signed_int_type(from->tag) && is_signed_int_type(to->tag)) ||
        (is_unsigned_int_type(from->tag) && is_unsigned_int_type(to->tag)) ||
        (is_float_type(from->tag) && is_float_type(to->tag)))
    {
        if ((!is_join) ^ (get_prim_type_bitwidth(from->tag) <= get_prim_type_bitwidth(to->tag)))
            return to;
    }

    if (from->tag != to->tag || is_nominal_type(from->tag))
        return make_error_type(type_table);

    switch (from->tag) {
        case TYPE_TUPLE: {
            if (from->tuple_type.arg_count != to->tuple_type.arg_count)
                break;
            const Type** arg_types = merge_many_types(type_table,
                from->tuple_type.arg_types, to->tuple_type.arg_types,
                from->tuple_type.arg_count, is_join);
            const Type* result = make_tuple_type(type_table, arg_types, from->tuple_type.arg_count);
            free(arg_types);
            return result;
        }
        case TYPE_APP: {
            if (from->type_app.arg_count != to->type_app.arg_count)
                break;
            const Type** arg_types = merge_many_types(type_table,
                from->tuple_type.arg_types, to->tuple_type.arg_types,
                from->tuple_type.arg_count, is_join);
            const Type* result = make_type_app(type_table,
                merge_types(type_table, from->type_app.applied_type, to->type_app.applied_type, is_join),
                arg_types, from->type_app.arg_count);
            free(arg_types);
            return result;
        }
        case TYPE_ARRAY:
            return make_array_type(type_table,
                merge_types(type_table, from->array_type.elem_type, to->array_type.elem_type, is_join));
        case TYPE_FUN:
            return make_fun_type(type_table,
                merge_types(type_table, from->fun_type.dom_type, to->fun_type.dom_type, !is_join),
                merge_types(type_table, from->fun_type.codom_type, to->fun_type.codom_type, is_join));
        default:
            assert(false && "invalid structural type");
            break;
    }
    return make_error_type(type_table);
}

void swap_types(const Type** left, const Type** right) {
    const Type* tmp = *left;
    *left = *right;
    *right = tmp;
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
