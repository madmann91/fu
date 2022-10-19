#include "fu/lang/types.h"
#include "fu/lang/type_table.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <string.h>

//================================== TYPE MAPS/SETS ======================================

typedef struct TypeMapElem {
    const Type* from;
    void* to;
} TypeMapElem;

TypeMap new_type_map(void) {
    return (TypeMap) { .hash_table = new_hash_table(sizeof(TypeMapElem)) };
}

TypeSet new_type_set(void) {
    return (TypeSet) { .hash_table = new_hash_table(sizeof(Type*)) };
}

void clear_type_map(TypeMap* type_map) { clear_hash_table(&type_map->hash_table); }
void clear_type_set(TypeSet* type_set) { clear_hash_table(&type_set->hash_table); }
void free_type_map(TypeMap* type_map) { free_hash_table(&type_map->hash_table); }
void free_type_set(TypeSet* type_set) { free_hash_table(&type_set->hash_table); }

static bool compare_type_map_elems(const void* left, const void* right) {
    return ((TypeMapElem*)left)->from == ((TypeMapElem*)right)->from;
}

bool insert_in_type_map(TypeMap* type_map, const Type* from, void* to) {
    assert(from != NULL);
    return insert_in_hash_table(&type_map->hash_table,
        &(TypeMapElem) { .from = from, .to = to },
        hash_uint64(hash_init(), from->id),
        sizeof(TypeMapElem),
        compare_type_map_elems);
}

static bool compare_type_set_elems(const void* left, const void* right) {
    return *(Type**)left == *(Type**)right;
}

bool insert_in_type_set(TypeSet* type_set, const Type* from) {
    assert(from != NULL);
    return insert_in_hash_table(&type_set->hash_table,
        &from,
        hash_uint64(hash_init(), from->id),
        sizeof(Type*),
        compare_type_set_elems);
}

void* find_in_type_map(const TypeMap* type_map, const Type* from) {
    const TypeMapElem* elem = find_in_hash_table(&type_map->hash_table,
        &(TypeMapElem) { .from = from },
        hash_uint64(hash_init(), from->id),
        sizeof(TypeMapElem),
        compare_type_map_elems);
    return elem ? elem->to : NULL;
}

bool find_in_type_set(const TypeSet* type_set, const Type* from) {
    const TypeMapElem* elem = find_in_hash_table(&type_set->hash_table,
        &(TypeMapElem) { .from = from },
        hash_uint64(hash_init(), from->id),
        sizeof(Type*),
        compare_type_set_elems);
    return elem != NULL;
}

//========================================================================================

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

bool is_bottom_or_top_type(TypeTag tag) {
    return tag == TYPE_BOTTOM || tag == TYPE_TOP;
}

bool is_unit_type(const Type* type) {
    return type->tag == TYPE_TUPLE && type->tuple.arg_count == 0;
}

bool is_kind_level_type(const Type* type) {
    return type->tag == KIND_STAR || type->tag == KIND_ARROW;
}

bool is_non_const_ptr_type(const Type* type) {
    return type->tag == TYPE_PTR && !type->ptr.is_const;
}

bool is_struct_like_option(const EnumOption* option) {
    if (!option->param_type)
        return false;
    const Type* param_type = get_applied_type(option->param_type);
    return param_type->tag == TYPE_STRUCT && param_type->struct_.parent_enum;
}

bool is_tuple_like_struct_type(const Type* type) {
    return type->tag == TYPE_STRUCT && type->struct_.is_tuple_like;
}

bool is_sub_type(TypeTable* type_table, const Type* left, const Type* right) {
    left = resolve_type(left);
    right = resolve_type(right);

    if (left == right ||
        right->tag == TYPE_TOP ||
        left->tag == TYPE_BOTTOM ||
        left->tag == TYPE_NORET ||
        left->tag == TYPE_UNKNOWN ||
        right->tag == TYPE_UNKNOWN)
        return true;

    if ((is_signed_int_type(left->tag) && is_signed_int_type(right->tag)) ||
        (is_unsigned_int_type(left->tag) && is_unsigned_int_type(right->tag)) ||
        (is_float_type(left->tag) && is_float_type(right->tag)))
        return get_prim_type_bitwidth(left->tag) <= get_prim_type_bitwidth(right->tag);

    if (is_sub_struct_type(type_table, left, right))
        return true;

    if (is_sub_enum_type(type_table, left, right))
        return true;

    if (left->tag != right->tag)
        return false;

    if (left->tag == TYPE_TUPLE && left->tuple.arg_count == right->tuple.arg_count) {
        for (size_t i = 0; i < left->tuple.arg_count; ++i) {
            if (!is_sub_type(type_table, left->tuple.args[i], right->tuple.args[i]))
                return false;
        }
        return true;
    }

    if (left->tag == TYPE_FUN) {
        return
            is_sub_type(type_table, right->fun.dom, left->fun.dom) &&
            is_sub_type(type_table, left->fun.codom, right->fun.codom);
    }

    return false;
}

static const Type* replace_types_using_type_app(
    TypeTable* type_table,
    const Type* type,
    const Type* type_app)
{
    type_app = resolve_type(type_app);
    if (type_app->tag != TYPE_APP)
        return type;

    // Take the arguments from the type application and use them to substitute the type parameters
    const Type* applied_type = get_applied_type(type_app);
    const Type** type_params = get_type_params(
        applied_type->kind->tag == TYPE_SIGNATURE ? applied_type->kind : applied_type);
    return replace_types(type_table, type,
        type_params,
        type_app->app.args,
        type_app->app.arg_count);
}

static bool is_sub_struct_or_super_enum_type(
    TypeTable* type_table,
    TypeTag type_tag,
    const Type* left,
    const Type* right)
{
    assert(type_tag == TYPE_STRUCT || type_tag == TYPE_ENUM);

    const Type* left_struct_or_enum  = get_applied_type(left);
    const Type* right_struct_or_enum = get_applied_type(right);
    if (left_struct_or_enum->tag != type_tag || right_struct_or_enum->tag != type_tag)
        return false;

    size_t left_depth  = get_type_inheritance_depth(left_struct_or_enum);
    size_t right_depth = get_type_inheritance_depth(right_struct_or_enum);
    if (left_depth < right_depth)
        return false;

    while (left_depth > right_depth) {
        left = replace_types_using_type_app(type_table,
            type_tag == TYPE_STRUCT
                ? left_struct_or_enum->struct_.super_type
                : left_struct_or_enum->enum_.sub_type,
            left);
        left_struct_or_enum = get_applied_type(left);
        assert(left_struct_or_enum->tag == type_tag);
        left_depth--;
    }

    return left == right;
}

bool is_sub_struct_type(TypeTable* type_table, const Type* left, const Type* right) {
    return is_sub_struct_or_super_enum_type(type_table, TYPE_STRUCT, left, right);
}

bool is_sub_enum_type(TypeTable* type_table, const Type* left, const Type* right) {
    return is_sub_struct_or_super_enum_type(type_table, TYPE_ENUM, right, left);
}

const Type* get_struct_field_type(TypeTable* type_table, const Type* type, size_t i) {
    const Type* struct_type = get_applied_type(type);
    assert(struct_type->tag == TYPE_STRUCT);
    if (type->tag != TYPE_APP)
        return struct_type->struct_.fields[i].type;
    return replace_types(type_table,
        struct_type->struct_.fields[i].type,
        struct_type->struct_.type_params,
        type->app.args,
        type->app.arg_count);
}

const Type* get_enum_option_param_type(TypeTable* type_table, const Type* type, size_t i) {
    const Type* enum_type = get_applied_type(type);
    assert(enum_type->tag == TYPE_ENUM);
    if (type->tag != TYPE_APP)
        return enum_type->enum_.options[i].param_type;
    return replace_types(type_table,
        enum_type->enum_.options[i].param_type,
        enum_type->struct_.type_params,
        type->app.args,
        type->app.arg_count);
}

const Type* resolve_type(const Type* type) {
    while (true) {
        if (type->tag == TYPE_VAR && type->var.value)
            type = type->var.value;
        else if (type->tag == TYPE_ALIAS && type->alias.type_param_count == 0)
            type = type->alias.aliased_type;
        else if (type->tag == TYPE_PROJ) {
            const Type* signature = type->proj.projected_type->kind;
            assert(signature && signature->tag == TYPE_SIGNATURE);
            assert(signature->signature.type_param_count == 0);
            const Type* var = signature->signature.vars[type->proj.index];
            if (!var->var.value)
                break;
            type = var->var.value;
        } else
            break;
    }
    return type;
}

const Type* skip_app_type(const Type* type) {
    return type->tag == TYPE_APP ? type->app.applied_type : type;
}

const Type* get_applied_type(const Type* type) {
    return resolve_type(skip_app_type(type));
}

const Type** get_type_params(const Type* type) {
    switch (type->tag) {
        case TYPE_ALIAS:     return type->alias.type_params;
        case TYPE_SIGNATURE: return type->signature.type_params;
        case TYPE_STRUCT:    return type->struct_.type_params;
        case TYPE_ENUM:      return type->enum_.type_params;
        case TYPE_FUN:       return type->fun.type_params;
        default:             return NULL;
    }
}

size_t get_type_param_count(const Type* type) {
    switch (type->tag) {
        case TYPE_ALIAS:     return type->alias.type_param_count;
        case TYPE_SIGNATURE: return type->signature.type_param_count;
        case TYPE_STRUCT:    return type->struct_.type_param_count;
        case TYPE_ENUM:      return type->enum_.type_param_count;
        case TYPE_FUN:       return type->fun.type_param_count;
        default:             return 0;
    }
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

size_t get_type_inheritance_depth(const Type* type) {
    size_t depth = 0;
    while (true) {
        type = get_applied_type(type);
        if (type->tag == TYPE_STRUCT && type->struct_.super_type)
            type = type->struct_.super_type, depth++;
        else if (type->tag == TYPE_ENUM && type->enum_.sub_type)
            type = type->enum_.sub_type, depth++;
        else
            break;
    }
    return depth;
}

static inline TypeVariance invert_variance(TypeVariance variance) {
    if (variance == TYPE_COVARIANT) return TYPE_CONTRAVARIANT;
    if (variance == TYPE_CONTRAVARIANT) return TYPE_COVARIANT;
    return variance;
}

static void traverse_type_with_variance(
    const Type* from,
    const Type* to,
    TypeVariance variance,
    TypeMap* type_map,
    void (*traverse_var)(const Type*, const Type*, TypeVariance, TypeMap*))
{
    if (from->tag != to->tag && from->tag != TYPE_VAR)
        return;
    switch (from->tag) {
        case TYPE_VAR:
            traverse_var(from, to, variance, type_map);
            break;
        case TYPE_TUPLE:
            for (size_t i = 0; i < from->tuple.arg_count && i < to->tuple.arg_count; ++i)
                traverse_type_with_variance(from->tuple.args[i], to->tuple.args[i], variance, type_map, traverse_var);
            break;
        case TYPE_FUN:
            traverse_type_with_variance(from->fun.dom, to->fun.dom, invert_variance(variance), type_map, traverse_var);
            traverse_type_with_variance(from->fun.codom, to->fun.codom, variance, type_map, traverse_var);
            break;
        case TYPE_ARRAY:
            traverse_type_with_variance(from->array.elem_type, to->array.elem_type, TYPE_INVARIANT, type_map, traverse_var);
            break;
        case TYPE_PTR:
            traverse_type_with_variance(from->ptr.pointed_type, to->ptr.pointed_type, TYPE_INVARIANT, type_map, traverse_var);
            break;
        case TYPE_PROJ:
            traverse_type_with_variance(from->proj.projected_type, to->proj.projected_type, TYPE_INVARIANT, type_map, traverse_var);
            return;
        case TYPE_APP: {
            traverse_type_with_variance(from->app.applied_type, to->app.applied_type, TYPE_INVARIANT, type_map, traverse_var);
            for (size_t i = 0; i < from->app.arg_count && i < to->app.arg_count; ++i)
                traverse_type_with_variance(from->app.args[i], to->app.args[i], TYPE_INVARIANT, type_map, traverse_var);
            break;
        }
        default:
            break;
    }
}

static void store_type_bounds(
    const Type* from,
    const Type* to,
    TypeVariance variance,
    TypeMap* type_map)
{
    TypeBounds* type_bounds = find_in_type_map(type_map, from);
    if (type_bounds) {
        if (variance & TYPE_COVARIANT)     type_bounds->upper = to;
        if (variance & TYPE_CONTRAVARIANT) type_bounds->lower = to;
    }
}

static void store_type_variance(
    const Type* from,
    const Type* to,
    TypeVariance variance,
    TypeMap* type_map)
{
    (void)to;
    TypeVariance* type_variance = find_in_type_map(type_map, from);
    if (type_variance)
        *type_variance = variance;
}

void get_type_vars_bounds(
    const Type* from,
    const Type* to,
    TypeVariance variance,
    TypeMap* type_map)
{
    traverse_type_with_variance(from, to, variance, type_map, store_type_bounds);
}

void get_type_vars_variance(const Type* type, TypeVariance variance, TypeMap* type_map) {
    traverse_type_with_variance(type, type, variance, type_map, store_type_variance);
}

int compare_signature_vars_by_name(const void* left, const void* right) {
    return strcmp((*(const Type**)left)->var.name, (*(const Type**)right)->var.name);
}

int compare_struct_fields_by_name(const void* left, const void* right) {
    return strcmp(((StructField*)left)->name, ((StructField*)right)->name);
}

int compare_enum_options_by_name(const void* left, const void* right) {
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

const Type** find_signature_var(const Type* signature, const char* name) {
    assert(signature->tag == TYPE_SIGNATURE);
    const Type* key = &(Type) { .var.name = name };
    return safe_bsearch(&key,
        signature->signature.vars,
        signature->signature.var_count,
        sizeof(Type*),
        compare_signature_vars_by_name);
}

const StructField* find_struct_field(const Type* struct_type, const char* name) {
    assert(struct_type->tag == TYPE_STRUCT);
    return safe_bsearch(&(StructField) { .name = name },
        struct_type->struct_.fields,
        struct_type->struct_.field_count,
        sizeof(StructField),
        compare_struct_fields_by_name);
}

const EnumOption* find_enum_option(const Type* enum_type, const char* name) {
    assert(enum_type->tag == TYPE_ENUM);
    return safe_bsearch(&(EnumOption) { .name = name },
        enum_type->enum_.options,
        enum_type->enum_.option_count,
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

static void print_many_types_with_delim(
    FormatState* state,
    const char* open,
    const char* sep,
    const char* close,
    const Type** types,
    size_t count)
{
    format(state, open, NULL);
    print_many_types(state, sep, types, count);
    format(state, close, NULL);
}

static void print_type_params(FormatState* state, const Type** type_params, size_t type_param_count) {
    if (type_param_count != 0)
        print_many_types_with_delim(state, "[", ", ", "]", type_params, type_param_count);
}

void print_type(FormatState* state, const Type* type) {
    switch (type->tag) {
#define f(name, str) case TYPE_##name: print_keyword(state, str); break;
        PRIM_TYPE_LIST(f);
#undef f
        case KIND_STAR:
            format(state, "*", NULL);
            break;
        case KIND_ARROW:
            if (type->arrow.kind_param_count == 1 && type->arrow.kind_params[0]->tag == KIND_STAR) {
                print_type(state, type->arrow.kind_params[0]);
                format(state, " => ", NULL);
            } else {
                print_many_types_with_delim(state, "(", ", ", ") => ",
                    type->arrow.kind_params, type->arrow.kind_param_count);
            }
            print_type(state, type->arrow.body);
            break;
        case TYPE_UNKNOWN:
            format(state, "?", NULL);
            break;
        case TYPE_ERROR:
            print_with_style(state, "<error>", error_style);
            break;
        case TYPE_BOTTOM:
            print_with_style(state, "bot", error_style);
            break;
        case TYPE_TOP:
            print_with_style(state, "top", error_style);
            break;
        case TYPE_NORET:
            format(state, "!", NULL);
            break;
        case TYPE_TUPLE:
            format(state, "(", NULL);
            print_many_types(state, ", ", type->tuple.args, type->tuple.arg_count);
            format(state, ")", NULL);
            break;
        case TYPE_APP:
            print_type(state, type->app.applied_type);
            format(state, "[", NULL);
            print_many_types(state, ", ", type->app.args, type->app.arg_count);
            format(state, "]", NULL);
            break;
        case TYPE_ARRAY:
            format(state, "[", NULL);
            print_type(state, type->array.elem_type);
            format(state, "]", NULL);
            break;
        case TYPE_VAR:
            format(state, "{s}", (FormatArg[]) { { .s = type->var.name } });
            break;
        case TYPE_FUN:
            print_keyword(state, "fun");
            print_type_params(state, type->fun.type_params, type->fun.type_param_count);
            if (type->fun.dom->tag == TYPE_TUPLE)
                print_type(state, type->fun.dom);
            else {
                format(state, "(", NULL);
                print_type(state, type->fun.dom);
                format(state, ")", NULL);
            }
            format(state, " -> ", NULL);
            print_type(state, type->fun.codom);
            break;
        case TYPE_SIGNATURE:
            print_keyword(state, "sig");
            print_type_params(state, type->fun.type_params, type->fun.type_param_count);
            format(state, " {{ ... }", NULL);
            break;
        case TYPE_ENUM:
            print_keyword(state, "enum");
            format(state, " {s}", (FormatArg[]) { { .s = type->enum_.name } });
            break;
        case TYPE_STRUCT:
            print_keyword(state, "struct");
            format(state, " {s}", (FormatArg[]) { { .s = type->struct_.name } });
            break;
        case TYPE_ALIAS:
            format(state, "{s}", (FormatArg[]) { { .s = type->alias.name } });
            break;
        case TYPE_PTR:
            format(state, "&", NULL);
            if (type->ptr.is_const) {
                print_keyword(state, "const");
                format(state, " ", NULL);
            }
            print_type(state, type->ptr.pointed_type);
            break;
        case TYPE_PROJ: {
            assert(type->proj.projected_type->kind->tag == TYPE_SIGNATURE);
            const Type* signature = type->proj.projected_type->kind;
            const char* field_name = signature->signature.vars[type->proj.index]->var.name;
            print_type(state, type->proj.projected_type);
            format(state, ".{s}", (FormatArg[]) { { .s = field_name } });
            break;
        }
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
