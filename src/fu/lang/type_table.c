#include "fu/lang/type_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/str_pool.h"
#include "fu/core/alloc.h"
#include "fu/core/hash_table.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

enum {
#define f(name, ...) PRIM_TYPE_##name,
    PRIM_TYPE_LIST(f)
#undef f
    PRIM_TYPE_COUNT
};

struct TypeTable {
    HashTable types;
    HashTable kinds;
    MemPool* mem_pool;
    StrPool str_pool;
    size_t type_count, kind_count;
    const Type* star_kind;
    const Type* prim_types[PRIM_TYPE_COUNT];
    const Type* unknown_type;
    const Type* noret_type;
    const Type* unit_type;
    const Type* error_type;
};

static HashCode hash_types(HashCode hash, const Type** types, size_t count) {
    for (size_t i = 0; i < count; ++i)
        hash = hash_uint64(hash, types[i]->id);
    return hash;
}

static HashCode hash_type_params(HashCode hash, const Type* type) {
    return hash_types(hash, get_type_params(type), get_type_param_count(type));
}

static HashCode hash_type(HashCode hash, const Type* type) {
    hash = hash_uint32(hash, type->tag);
    switch (type->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case KIND_STAR:
        case TYPE_UNKNOWN:
        case TYPE_NORET:
        case TYPE_ERROR:
            break;
        case TYPE_ALIAS:
            hash = hash_type_params(hash, type);
            hash = hash_str(hash, type->alias.name);
            hash = hash_uint64(hash, type->alias.aliased_type->id);
            break;
        case TYPE_PROJ:
            hash = hash_uint64(hash, type->proj.projected_type->id);
            hash = hash_uint64(hash, (uint64_t)type->proj.index);
            break;
        case TYPE_TUPLE:
            hash = hash_types(hash, type->tuple.args, type->tuple.arg_count);
            break;
        case TYPE_APP:
            hash = hash_uint64(hash, type->app.applied_type->id);
            hash = hash_types(hash, type->app.args, type->app.arg_count);
            break;
        case KIND_ARROW:
            hash = hash_types(hash, type->arrow.kind_params, type->arrow.kind_param_count);
            hash = hash_uint64(hash, type->arrow.body->id);
            break;
        case TYPE_FUN:
            hash = hash_type_params(hash, type);
            hash = hash_uint64(hash, type->fun.dom->id);
            hash = hash_uint64(hash, type->fun.codom->id);
            break;
        case TYPE_ARRAY:
            hash = hash_uint64(hash, type->array.elem_type->id);
            break;
        case TYPE_VAR:
            hash = hash_str(hash, type->var.name);
            break;
        case TYPE_PTR:
            hash = hash_uint8(hash, type->ptr.is_const);
            hash = hash_uint64(hash, type->ptr.pointee->id);
            break;
        default:
            assert(false && "invalid type");
            break;
    }
    return hash;
}

static bool compare_type_params(const Type* left, const Type* right) {
    return
        get_type_param_count(left) == get_type_param_count(right) &&
        !memcpy(get_type_params(left), get_type_params(right),
            sizeof(Type*) * get_type_param_count(left));
}

static bool compare_types(const Type* left, const Type* right) {
    if (left->tag != right->tag)
        return false;
    switch (left->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case KIND_STAR:
        case TYPE_UNKNOWN:
        case TYPE_NORET:
        case TYPE_ERROR:
            break;
        case TYPE_TUPLE: {
            return
                left->tuple.arg_count == right->tuple.arg_count &&
                !memcmp(left->tuple.args, right->tuple.args,
                    sizeof(Type*) * left->tuple.arg_count);
        }
        case TYPE_ALIAS: {
            return
                left->alias.aliased_type == right->alias.aliased_type &&
                left->alias.name == right->alias.name &&
                compare_type_params(left, right);
        }
        case TYPE_PROJ: {
            return
                left->proj.projected_type == right->proj.projected_type &&
                left->proj.index == right->proj.index;
        }
        case TYPE_APP: {
            return
                left->app.arg_count == right->app.arg_count &&
                left->app.applied_type == right->app.applied_type &&
                !memcmp(left->app.args, right->app.args,
                    sizeof(Type*) * left->app.arg_count);
        }
        case KIND_ARROW: {
            return
                left->arrow.body == right->arrow.body &&
                left->arrow.kind_param_count == right->arrow.kind_param_count &&
                !memcpy(left->arrow.kind_params, right->arrow.kind_params,
                    sizeof(Type*) * left->arrow.kind_param_count);
        }
        case TYPE_FUN: {
            return
                left->fun.dom == right->fun.dom &&
                left->fun.codom == right->fun.codom &&
                compare_type_params(left, right);
        }
        case TYPE_ARRAY:
            return left->array.elem_type == right->array.elem_type;
        case TYPE_VAR:
            return left->var.name == right->var.name;
        case TYPE_PTR:
            return
                left->ptr.is_const == right->ptr.is_const &&
                left->ptr.pointee  == right->ptr.pointee;
        default:
            assert(false && "invalid type");
            break;
    }
    return true;
}

static const Type** copy_types(TypeTable* type_table, const Type** types, size_t count) {
    const Type** types_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * count);
    memcpy(types_copy, types, sizeof(Type*) * count);
    return types_copy;
}

static bool compare_types_wrapper(const void* left, const void* right) {
    return compare_types(*(const Type**)left, *(const Type**)right);
}

static const Type* get_or_insert_type(TypeTable* type_table, const Type* type) {
    assert(!is_nominal_type(type->tag));

    uint32_t hash = hash_type(hash_init(), type);
    const Type** type_ptr = find_in_hash_table(&type_table->types, &type, hash, sizeof(Type*), compare_types_wrapper);
    if (type_ptr)
        return *type_ptr;

    Type* new_type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    memcpy(new_type, type, sizeof(Type));
    new_type->id = type_table->type_count++;

    new_type->contains_error = type->tag == TYPE_ERROR;
    new_type->contains_unknown = type->tag == TYPE_UNKNOWN;
    switch (type->tag) {
        case TYPE_TUPLE:
            new_type->tuple.args =
                copy_types(type_table, type->tuple.args, type->tuple.arg_count);
            for (size_t i = 0; i < type->tuple.arg_count; ++i) {
                new_type->contains_error   |= type->tuple.args[i]->contains_error;
                new_type->contains_unknown |= type->tuple.args[i]->contains_unknown;
            }
            break;
        case TYPE_ALIAS:
            new_type->alias.type_params =
                copy_types(type_table, type->alias.type_params, type->alias.type_param_count);
            new_type->contains_error   |= type->alias.aliased_type->contains_error;
            new_type->contains_unknown |= type->alias.aliased_type->contains_unknown;
            break;
        case TYPE_APP:
            new_type->app.args =
                copy_types(type_table, type->app.args, type->app.arg_count);
            new_type->contains_error |= type->app.applied_type->contains_error;
            new_type->contains_unknown |= type->app.applied_type->contains_unknown;
            for (size_t i = 0; i < type->app.arg_count; ++i) {
                new_type->contains_error   |= type->app.args[i]->contains_error;
                new_type->contains_unknown |= type->app.args[i]->contains_unknown;
            }
            break;
        case TYPE_ARRAY:
            new_type->contains_error   |= type->array.elem_type->contains_error;
            new_type->contains_unknown |= type->array.elem_type->contains_unknown;
            break;
        case TYPE_FUN:
            new_type->fun.type_params =
                copy_types(type_table, type->fun.type_params, type->fun.type_param_count);
            new_type->contains_error |= type->fun.dom->contains_error;
            new_type->contains_error |= type->fun.codom->contains_error;
            new_type->contains_unknown |= type->fun.dom->contains_unknown;
            new_type->contains_unknown |= type->fun.codom->contains_unknown;
            break;
        case KIND_ARROW:
            new_type->arrow.kind_params =
                copy_types(type_table, type->arrow.kind_params, type->arrow.kind_param_count);
            break;
        default:
            break;
    }

    must_succeed(insert_in_hash_table(&type_table->types, &new_type, hash, sizeof(Type*), compare_types_wrapper));
    return new_type;
}

static TypeTag get_first_prim_type_tag() {
#define f(name, ...) return TYPE_##name;
    PRIM_TYPE_LIST(f)
#undef f
}

TypeTable* new_type_table(MemPool* mem_pool) {
    TypeTable* type_table = malloc_or_die(sizeof(TypeTable));
    type_table->types = new_hash_table(sizeof(Type*));
    type_table->str_pool = new_str_pool(mem_pool);
    type_table->mem_pool = mem_pool;
    type_table->type_count = 0;

    const Type* star = type_table->star_kind = get_or_insert_type(type_table, &(Type) { .tag = KIND_STAR });
    for (size_t i = 0; i < PRIM_TYPE_COUNT; ++i)
        type_table->prim_types[i] = get_or_insert_type(type_table, &(Type) { .tag = get_first_prim_type_tag() + i, .kind = star });
    type_table->unknown_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_UNKNOWN, .kind = star });
    type_table->noret_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_NORET, .kind = star });
    type_table->error_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_ERROR, .kind = star });
    type_table->unit_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_TUPLE, .kind = star, .tuple.arg_count = 0 });
    return type_table;
}

void free_type_table(TypeTable* type_table) {
    free_hash_table(&type_table->types);
    free_str_pool(&type_table->str_pool);
    free(type_table);
}

const Type* make_star_kind(TypeTable* type_table) {
    return type_table->star_kind;
}

const Type* make_arrow_kind(
    TypeTable* type_table,
    const Type** kind_params,
    size_t kind_param_count,
    const Type* body_kind)
{
    if (kind_param_count == 0)
        return body_kind;
    return get_or_insert_type(type_table, &(Type) {
        .tag = KIND_ARROW,
        .arrow = {
            .kind_params = kind_params,
            .kind_param_count = kind_param_count,
            .body = body_kind
        }
    });
}

const Type* make_type_ctor_kind(
    TypeTable* type_table,
    const Type** type_params,
    size_t type_param_count,
    const Kind* body)
{
    const Type** kind_params = malloc_or_die(sizeof(Type*) * type_param_count);
    for (size_t i = 0; i < type_param_count; ++i)
        kind_params[i] = type_params[i]->kind;
    const Type* arrow_kind = make_arrow_kind(
        type_table, kind_params, type_param_count, body);
    free(kind_params);
    return arrow_kind;
}

static Type* alloc_type_with_tag(TypeTable* type_table, TypeTag tag) {
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    memset(type, 0, sizeof(Type));
    type->id = type_table->type_count++;
    type->tag = tag;
    return type;
}

Type* make_var_type(TypeTable* type_table, const char* name) {
    Type* var = alloc_type_with_tag(type_table, TYPE_VAR);
    var->var.name = make_str(&type_table->str_pool, name);
    return var;
}

Type* make_var_type_with_kind(TypeTable* type_table, const char* name, const Kind* kind) {
    Type* var = make_var_type(type_table, name);
    var->kind = kind;
    return var;
}

Type* make_var_type_with_value(TypeTable* type_table, const char* name, const Type* value) {
    assert(value->kind);
    Type* var = make_var_type_with_kind(type_table, name, value->kind);
    var->var.value = value;
    return var;
}

Type* make_struct_type(TypeTable* type_table, const char* name) {
    Type* struct_type = alloc_type_with_tag(type_table, TYPE_STRUCT);
    struct_type->struct_.name = make_str(&type_table->str_pool, name);
    return struct_type;
}

Type* make_enum_type(TypeTable* type_table, const char* name) {
    Type* enum_type = alloc_type_with_tag(type_table, TYPE_ENUM);
    enum_type->enum_.name = make_str(&type_table->str_pool, name);
    return enum_type;
}

Type* make_signature_type(TypeTable* type_table) {
    Type* signature = alloc_type_with_tag(type_table, TYPE_SIGNATURE);
    return signature;
}

static StructField* copy_and_sort_struct_fields(
    TypeTable* type_table,
    StructField* fields,
    size_t field_count)
{
    StructField* fields_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(StructField) * field_count);
    memcpy(fields_copy, fields, sizeof(StructField) * field_count);
    for (size_t i = 0; i < field_count; ++i)
        fields_copy[i].name = make_str(&type_table->str_pool, fields[i].name);
    qsort(fields_copy, field_count, sizeof(StructField), compare_struct_fields_by_name);
    return fields_copy;
}

static const Type** copy_type_params(
    TypeTable* type_table,
    const Type** type_params,
    size_t type_param_count)
{
    const Type** type_params_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * type_param_count);
    memcpy(type_params_copy, type_params, sizeof(Type*) * type_param_count);
    return type_params_copy;
}

const Type* seal_struct_type(TypeTable* type_table, Type* type) {
    assert(type->tag == TYPE_STRUCT);
    assert(!type->struct_.is_sealed);
    assert(type->kind);
    type->struct_.fields =
        copy_and_sort_struct_fields(type_table, type->struct_.fields, type->struct_.field_count);
    type->struct_.type_params =
        copy_type_params(type_table, type->struct_.type_params, type->struct_.type_param_count);
#ifndef NDEBUG
    type->struct_.is_sealed = true;
#endif
    return type;
}

static EnumOption* copy_and_sort_enum_options(
    TypeTable* type_table,
    EnumOption* options,
    size_t option_count)
{
    EnumOption* options_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(EnumOption) * option_count);
    memcpy(options_copy, options, sizeof(EnumOption) * option_count);
    for (size_t i = 0; i < option_count; ++i)
        options_copy[i].name = make_str(&type_table->str_pool, options[i].name);
    qsort(options_copy, option_count, sizeof(EnumOption), compare_enum_options_by_name);
    return options_copy;
}

const Type* seal_enum_type(TypeTable* type_table, Type* type) {
    assert(type->tag == TYPE_ENUM);
    assert(!type->enum_.is_sealed);
    assert(type->kind);
    type->enum_.options =
        copy_and_sort_enum_options(type_table, type->enum_.options, type->enum_.option_count);
    type->enum_.type_params =
        copy_type_params(type_table, type->enum_.type_params, type->enum_.type_param_count);
#ifndef NDEBUG
    type->enum_.is_sealed = true;
#endif
    return type;
}

static const Type** copy_and_sort_signature_vars(
    TypeTable* type_table,
    const Type** vars,
    size_t var_count)
{
    const Type** vars_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * var_count);
    memcpy(vars_copy, vars, sizeof(Type*) * var_count);
    qsort(vars_copy, var_count, sizeof(Type*), compare_signature_vars_by_name);
    return vars_copy;
}

const Type* seal_signature_type(TypeTable* type_table, Type* type) {
    assert(type->tag == TYPE_SIGNATURE);
    assert(!type->signature.is_sealed);
    assert(type->kind);
    type->signature.vars =
        copy_and_sort_signature_vars(type_table, type->signature.vars, type->signature.var_count);
    type->signature.type_params =
        copy_type_params(type_table, type->signature.type_params, type->signature.type_param_count);
#ifndef NDEBUG
    type->signature.is_sealed = true;
#endif
    return type;
}

const Type* make_prim_type(TypeTable* type_table, TypeTag tag) {
    assert(is_prim_type(tag));
    return type_table->prim_types[tag];
}

const Type* make_unknown_type(TypeTable* type_table) {
    return type_table->unknown_type;
}

const Type* make_error_type(TypeTable* type_table) {
    return type_table->error_type;
}

const Type* make_noret_type(TypeTable* type_table) {
    return type_table->noret_type;
}

const Type* make_tuple_type(TypeTable* type_table, const Type** args, size_t arg_count) {
    if (arg_count == 0)
        return type_table->unit_type;
    if (arg_count == 1)
        return args[0];
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_TUPLE,
        .kind = make_star_kind(type_table),
        .tuple = { .args = args, .arg_count = arg_count }
    });
}

const Type* make_unit_type(TypeTable* type_table) {
    return type_table->unit_type;
}

const Type* make_app_type(TypeTable* type_table, const Type* applied_type, const Type** args, size_t arg_count) {
    if (arg_count == 0)
        return applied_type;
    assert(applied_type->kind->tag == KIND_ARROW);
    assert(applied_type->kind->arrow.kind_param_count == arg_count);
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_APP,
        .kind = applied_type->kind->arrow.body,
        .app = { .applied_type = applied_type, .args = args, .arg_count = arg_count }
    });
}

const Type* make_array_type(TypeTable* type_table, const Type* elem_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_ARRAY,
        .kind = make_star_kind(type_table),
        .array = { .elem_type = elem_type }
    });
}

const Type* make_ptr_type(TypeTable* type_table, bool is_const, const Type* pointee_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_PTR,
        .kind = make_star_kind(type_table),
        .ptr = { .is_const = is_const, .pointee = pointee_type }
    });
}

const Type* make_fun_type(TypeTable* type_table, const Type* dom, const Type* codom) {
    return make_poly_fun_type(type_table, NULL, 0, dom, codom);
}

const Type* make_proj_type(TypeTable* type_table, const Type* projected_type, size_t index) {
    assert(projected_type->tag == TYPE_VAR);
    assert(projected_type->kind->tag == TYPE_SIGNATURE);
    assert(index < projected_type->kind->signature.var_count);
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_PROJ,
        .kind = projected_type->kind->signature.vars[index]->kind,
        .proj = { .projected_type = projected_type, .index = index }
    });
}

const Type* make_mod_type(TypeTable* type_table, const char* name, Type* signature) {
    // This code replaces signature members with no value with projections onto the module.
    // For instance, consider the following module:
    //
    //     mod M { pub opaque type T = i32; pub type U = (T, T); }
    //
    // This module ends up having the following signature:
    //
    //     sig { type T; type U = (M.T, M.T); }
    //
    // This makes extracting `M.U` result in the correct type.
    const Type* mod_type = make_var_type_with_kind(type_table, name, signature);
    const Type* projected_type = make_app_type(type_table, mod_type,
        signature->signature.type_params,
        signature->signature.type_param_count);

    TypeMap type_map = new_type_map();

    // Map variables to their projection
    for (size_t i = 0; i < signature->signature.var_count; ++i) {
        insert_in_type_map(&type_map,
            signature->signature.vars[i],
            (void*)make_proj_type(type_table, projected_type, i));
    }
    for (size_t i = 0; i < signature->signature.var_count; ++i) {
        // This replaces variables deeply inside structures and enumerations
        Type* var = (Type*)signature->signature.vars[i];
        var->kind = replace_types_with_map(type_table, var->kind, &type_map, false);
        if (var->var.value)
            var->var.value = replace_types_with_map(type_table, var->var.value, &type_map, false);
    }
    free_type_map(&type_map);
    return mod_type;
}

const Type* make_alias_type(
    TypeTable* type_table,
    const char* name,
    const Type** type_params,
    size_t type_param_count,
    const Type* aliased_type)
{
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_ALIAS,
        .kind = make_type_ctor_kind(type_table, type_params, type_param_count, aliased_type->kind),
        .alias = {
            .name = make_str(&type_table->str_pool, name),
            .type_params = type_params,
            .type_param_count = type_param_count,
            .aliased_type = aliased_type,
        }
    });
}

const Type* make_poly_fun_type(
    TypeTable* type_table,
    const Type** type_params,
    size_t type_param_count,
    const Type* dom,
    const Type* codom)
{
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_FUN,
        .kind = make_star_kind(type_table),
        .fun = {
            .type_params = type_params,
            .type_param_count = type_param_count,
            .dom = dom,
            .codom = codom
        }
    });
}

const Type* replace_types_with_map(
    TypeTable* type_table,
    const Type* type,
    TypeMap* type_map,
    bool is_shallow)
{
    switch (type->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case TYPE_UNKNOWN:
        case KIND_STAR:
        case KIND_ARROW:
        case TYPE_NORET:
        case TYPE_ERROR:
            return type;
        case TYPE_STRUCT:
            if (!is_shallow && !find_in_type_map(type_map, type)) {
                for (size_t i = 0; i < type->struct_.field_count; ++i) {
                    ((Type*)type)->struct_.fields[i].type = replace_types_with_map(
                        type_table, type->struct_.fields[i].type, type_map, is_shallow);
                }
                insert_in_type_map(type_map, type, (void*)type);
            }
            return type;
        case TYPE_ENUM:
            if (!is_shallow && !find_in_type_map(type_map, type)) {
                for (size_t i = 0; i < type->enum_.option_count; ++i) {
                    ((Type*)type)->enum_.options[i].param_type = replace_types_with_map(
                        type_table, type->enum_.options[i].param_type, type_map, is_shallow);
                }
                insert_in_type_map(type_map, type, (void*)type);
            }
            return type;
        case TYPE_TUPLE: {
            const Type** types = malloc(sizeof(Type*) * type->tuple.arg_count);
            for (size_t i = 0, n = type->tuple.arg_count; i < n; ++i)
                types[i] = replace_types_with_map(type_table, type->tuple.args[i], type_map, is_shallow);
            type = make_tuple_type(type_table, types, type->tuple.arg_count);
            free(types);
            return type;
        }
        case TYPE_ALIAS:
            return make_alias_type(type_table,
                type->alias.name,
                type->alias.type_params,
                type->alias.type_param_count,
                replace_types_with_map(type_table, type->alias.aliased_type, type_map, is_shallow));
        case TYPE_PROJ:
            return make_proj_type(type_table,
                replace_types_with_map(type_table, type->proj.projected_type, type_map, is_shallow),
                type->proj.index);
        case TYPE_APP: {
            const Type** args = malloc(sizeof(Type*) * type->app.arg_count);
            for (size_t i = 0, n = type->app.arg_count; i < n; ++i)
                args[i] = replace_types_with_map(type_table, type->app.args[i], type_map, is_shallow);
            type = make_app_type(type_table,
                replace_types_with_map(type_table, type->app.applied_type, type_map, is_shallow),
                args, type->app.arg_count);
            free(args);
            return type;
        }
        case TYPE_FUN: {
            return make_poly_fun_type(type_table,
                type->fun.type_params,
                type->fun.type_param_count,
                replace_types_with_map(type_table, type->fun.dom, type_map, is_shallow),
                replace_types_with_map(type_table, type->fun.codom, type_map, is_shallow));
        }
        case TYPE_ARRAY: {
            return make_array_type(type_table,
                replace_types_with_map(type_table, type->array.elem_type, type_map, is_shallow));
        }
        case TYPE_VAR: {
            const Type* mapped_type = find_in_type_map(type_map, type);
            return mapped_type ? mapped_type : type;
        }
        case TYPE_PTR: {
            return make_ptr_type(type_table,
                type->ptr.is_const,
                replace_types_with_map(type_table, type->ptr.pointee, type_map, is_shallow));
        }
        default:
            assert(false && "invalid type");
            break;
    }
}

const Type* replace_types(
    TypeTable* type_table,
    const Type* type,
    const Type** from,
    const Type** to,
    size_t mapped_type_count,
    bool is_shallow)
{
    TypeMap type_map = new_type_map();
    for (size_t i = 0; i < mapped_type_count; ++i)
        insert_in_type_map(&type_map, from[i], (void*)to[i]);
    const Type* replaced_type = replace_types_with_map(type_table, type, &type_map, is_shallow);
    free_type_map(&type_map);
    return replaced_type;
}
