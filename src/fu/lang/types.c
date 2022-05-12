#include "fu/lang/types.h"
#include "fu/lang/type_table.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <string.h>

typedef struct TypeMapElem {
    const Type* from;
    const Type* to;
} TypeMapElem;

TypeMap new_type_map(void) {
    return new_hash_table(sizeof(TypeMapElem));
}

void free_type_map(TypeMap* type_map) {
    free_hash_table(type_map);
}

static bool compare_type_map_elems(const void* left, const void* right) {
    return ((TypeMapElem*)left)->from == ((TypeMapElem*)right)->from;
}

bool insert_type_in_map(TypeMap* type_map, const Type* from, const Type* to) {
    return insert_in_hash_table(type_map,
        &(TypeMapElem) { .from = from, .to = to },
        hash_uint64(hash_init(), from->id),
        sizeof(TypeMapElem),
        compare_type_map_elems);
}

const Type* find_type_in_map(const TypeMap* type_map, const Type* from) {
    const TypeMapElem* elem = find_in_hash_table(type_map,
        &(TypeMapElem) { .from = from },
        hash_uint64(hash_init(), from->id),
        sizeof(TypeMapElem),
        compare_type_map_elems);
    return elem ? elem->to : NULL;
}

bool is_prim_type(TypeTag tag) {
    switch (tag) {
#define f(name, ...) case TYPE_##name:
        PRIM_TYPE_LIST(f)
#undef f
            return true;
        default:
            return false;
    }
}

bool is_nominal_type(TypeTag tag) {
    return tag == TYPE_STRUCT || tag == TYPE_ENUM;
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
        for (size_t i = 0; i < left->tuple_type.arg_count; ++i) {
            if (!is_subtype(left->tuple_type.args[i], right->tuple_type.args[i]))
                return false;
        }
        return true;
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

bool is_struct_like_option(const EnumOption* option) {
    return
        option->param_type &&
        option->param_type->tag == TYPE_STRUCT &&
        option->param_type->struct_type.parent_enum;
}

const Type* skip_type_app(const Type* type) {
    return type->tag == TYPE_APP ? type->type_app.applied_type : type;
}

size_t get_prim_type_bitwidth(TypeTag tag) {
    switch (tag) {
        case TYPE_BOOL: return 1;
        case TYPE_I8:  case TYPE_U8:  return 8;
        case TYPE_I16: case TYPE_U16: return 16;
        case TYPE_I32: case TYPE_U32: case TYPE_F32: return 32;
        case TYPE_I64: case TYPE_U64: case TYPE_F64: return 64;
        default:
            assert(false && "invalid primitive type");
            return 0;
    }
}

size_t get_type_param_count(const Type* type) {
    return
        type->tag == TYPE_STRUCT ? type->struct_type.type_param_count :
        type->tag == TYPE_ENUM ? type->enum_type.type_param_count : 0;
}

static int compare_signature_members_by_name(const void* left, const void* right) {
    return strcmp(((SignatureMember*)left)->name, ((SignatureMember*)right)->name);
}

static int compare_struct_fields_by_name(const void* left, const void* right) {
    return strcmp(((StructField*)left)->name, ((StructField*)right)->name);
}

static int compare_enum_options_by_name(const void* left, const void* right) {
    return strcmp(((EnumOption*)left)->name, ((EnumOption*)right)->name);
}

static inline bool is_sorted(
    const void* elems,
    size_t elem_count,
    size_t elem_size,
    int (*compare_elems)(const void*, const void*))
{
    for (size_t i = 1; i < elem_count; ++i) {
        if (compare_elems(
            ((char*)elems) + elem_size * (i - 1),
            ((char*)elems) + elem_size * i) > 0)
            return false;
    }
    return true;
}

static inline void* safe_bsearch(
    const void* key,
    const void* elems,
    size_t elem_count,
    size_t elem_size,
    int (*compare_elems)(const void*, const void*))
{
    assert(is_sorted(elems, elem_count, elem_size, compare_elems));
    return bsearch(key, elems, elem_count, elem_size, compare_elems);
}

const SignatureMember* find_signature_member(const Type* signature, const char* name) {
    assert(signature->tag == TYPE_SIGNATURE);
    return safe_bsearch(&(StructField) { .name = name },
        signature->signature.members,
        signature->signature.member_count,
        sizeof(SignatureMember),
        compare_signature_members_by_name);
}

const StructField* find_struct_field(const Type* struct_type, const char* name) {
    assert(struct_type->tag == TYPE_STRUCT);
    return safe_bsearch(&(StructField) { .name = name },
        struct_type->struct_type.fields,
        struct_type->struct_type.field_count,
        sizeof(StructField),
        compare_struct_fields_by_name);
}

const EnumOption* find_enum_option(const Type* enum_type, const char* name) {
    assert(enum_type->tag == TYPE_ENUM);
    return safe_bsearch(&(EnumOption) { .name = name },
        enum_type->enum_type.options,
        enum_type->enum_type.option_count,
        sizeof(EnumOption),
        compare_enum_options_by_name);
}

static void print_many_types(FormatState* state, const char* sep, const Type** types, size_t count) {
    for (size_t i = 0; i < count; ++i) {
        print_type(state, types[i]);
        if (i != count - 1)
            format(state, sep, NULL);
    }
}

static void print_type_params(FormatState* state, const Type** type_params, size_t type_param_count) {
    if (type_param_count == 0)
        return;
    format(state, "[", NULL);
    print_many_types(state, ", ", type_params, type_param_count);
    format(state, "]", NULL);
}

static void print_signature_member(FormatState* state, const SignatureMember* member) {
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
        PRIM_TYPE_LIST(f);
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
        case TYPE_VAR:
            format(state, "{s}", (FormatArg[]) { { .s = type->type_var.name } });
            break;
        case TYPE_FUN:
            print_keyword(state, "fun");
            print_type_params(state, type->fun_type.type_params, type->fun_type.type_param_count);
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
        case TYPE_SIGNATURE:
            print_keyword(state, "sig");
            format(state, " {{ ", NULL);
            for (size_t i = 0, n = type->signature.member_count; i < n; ++i) {
                print_signature_member(state, type->signature.members + i);
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, " }", NULL);
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
