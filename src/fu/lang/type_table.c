#include "fu/lang/type_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/str_pool.h"
#include "fu/core/alloc.h"
#include "fu/core/hash_table.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"
#include "fu/core/dyn_array.h"

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
    MemPool* mem_pool;
    StrPool str_pool;
    size_t type_count;
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

static HashCode hash_type(HashCode hash, const Type* type) {
    hash = hash_uint32(hash, type->tag);
    switch (type->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case TYPE_UNKNOWN:
        case TYPE_NORET:
        case TYPE_ERROR:
            break;
        case TYPE_ALIAS:
            hash = hash_types(hash, type->type_alias.type_params, type->type_alias.type_param_count);
            hash = hash_str(hash, type->type_alias.name);
            hash = hash_uint64(hash, type->type_alias.aliased_type->id);
            break;
        case TYPE_SIGNATURE:
            hash = hash_types(hash, type->signature.type_params, type->signature.type_param_count);
            for (size_t i = 0; i < type->signature.member_count; ++i) {
                hash = hash_str(hash, type->signature.members[i].name);
                hash = hash_uint64(hash, type->signature.members[i].type->id);
                hash = hash_uint8(hash, type->signature.members[i].is_type);
            }
            break;
        case TYPE_TUPLE:
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i)
                hash = hash_uint64(hash, type->tuple_type.args[i]->id);
            break;
        case TYPE_APP:
            hash = hash_uint64(hash, type->type_app.applied_type->id);
            for (size_t i = 0; i < type->type_app.arg_count; ++i)
                hash = hash_uint64(hash, type->type_app.args[i]->id);
            break;
        case TYPE_FUN:
            hash = hash_types(hash, type->fun_type.type_params, type->fun_type.type_param_count);
            hash = hash_uint64(hash, type->fun_type.dom->id);
            hash = hash_uint64(hash, type->fun_type.codom->id);
            break;
        case TYPE_ARRAY:
            hash = hash_uint64(hash, type->array_type.elem_type->id);
            break;
        case TYPE_VAR:
            hash = hash_str(hash, type->type_var.name);
            break;
        case TYPE_PTR:
            hash = hash_uint8(hash, type->ptr_type.is_const);
            hash = hash_uint64(hash, type->ptr_type.pointee->id);
            break;
        default:
            assert(false && "invalid type");
            break;
    }
    return hash;
}

static bool compare_types(const Type* left, const Type* right) {
    if (left->tag != right->tag)
        return false;
    switch (left->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
            break;
        case TYPE_TUPLE: {
            return
                left->tuple_type.arg_count == right->tuple_type.arg_count &&
                !memcmp(left->tuple_type.args, right->tuple_type.args,
                    sizeof(Type*) * left->tuple_type.arg_count);
        }
        case TYPE_ALIAS: {
            return
                left->type_alias.aliased_type == right->type_alias.aliased_type &&
                left->type_alias.type_param_count == right->type_alias.type_param_count &&
                left->type_alias.name == right->type_alias.name &&
                !memcmp(left->type_alias.type_params, right->type_alias.type_params,
                    sizeof(Type*) * left->type_alias.type_param_count);
        }
        case TYPE_SIGNATURE: {
            return
                left->signature.type_param_count == right->signature.type_param_count &&
                left->signature.member_count == right->signature.member_count &&
                !memcmp(left->signature.members, right->signature.members,
                    sizeof(SignatureMember) * left->signature.member_count) &&
                !memcmp(left->signature.type_params, right->signature.type_params,
                    sizeof(Type*) * left->signature.type_param_count);
        }
        case TYPE_APP: {
            return
                left->type_app.arg_count == right->type_app.arg_count &&
                left->type_app.applied_type == right->type_app.applied_type &&
                !memcmp(left->type_app.args, right->type_app.args,
                    sizeof(Type*) * left->type_app.arg_count);
        }
        case TYPE_FUN: {
            return
                left->fun_type.type_param_count == right->fun_type.type_param_count &&
                left->fun_type.dom == right->fun_type.dom &&
                left->fun_type.codom == right->fun_type.codom &&
                !memcmp(left->fun_type.type_params, right->fun_type.type_params,
                    sizeof(Type*) * left->fun_type.type_param_count);
        }
        case TYPE_ARRAY:
            return left->array_type.elem_type == right->array_type.elem_type;
        case TYPE_VAR:
            return left->type_var.name == right->type_var.name;
        case TYPE_PTR:
            return
                left->ptr_type.is_const == right->ptr_type.is_const &&
                left->ptr_type.pointee  == right->ptr_type.pointee;
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
    return compare_types(left, right);
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
            new_type->tuple_type.args =
                copy_types(type_table, type->tuple_type.args, type->tuple_type.arg_count);
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i) {
                new_type->contains_error   |= type->tuple_type.args[i]->contains_error;
                new_type->contains_unknown |= type->tuple_type.args[i]->contains_unknown;
            }
            break;
        case TYPE_ALIAS:
            new_type->type_alias.type_params =
                copy_types(type_table, type->type_alias.type_params, type->type_alias.type_param_count);
            new_type->contains_error   |= type->type_alias.aliased_type->contains_error;
            new_type->contains_unknown |= type->type_alias.aliased_type->contains_unknown;
            break;
        case TYPE_SIGNATURE:
            new_type->signature.type_params =
                copy_types(type_table, type->signature.type_params, type->signature.type_param_count);
            new_type->signature.members =
                alloc_from_mem_pool(type_table->mem_pool, sizeof(SignatureMember) * type->signature.member_count);
            for (size_t i = 0; i < type->signature.member_count; ++i) {
                new_type->signature.members[i] = type->signature.members[i];
                new_type->contains_error   |= type->signature.members[i].type->contains_error;
                new_type->contains_unknown |= type->signature.members[i].type->contains_unknown;
            }
            break;
        case TYPE_APP:
            new_type->type_app.args =
                copy_types(type_table, type->type_app.args, type->type_app.arg_count);
            new_type->contains_error |= type->type_app.applied_type->contains_error;
            new_type->contains_unknown |= type->type_app.applied_type->contains_unknown;
            for (size_t i = 0; i < type->type_app.arg_count; ++i) {
                new_type->contains_error   |= type->type_app.args[i]->contains_error;
                new_type->contains_unknown |= type->type_app.args[i]->contains_unknown;
            }
            break;
        case TYPE_ARRAY:
            new_type->contains_error   |= type->array_type.elem_type->contains_error;
            new_type->contains_unknown |= type->array_type.elem_type->contains_unknown;
            break;
        case TYPE_FUN:
            new_type->fun_type.type_params =
                copy_types(type_table, type->fun_type.type_params, type->fun_type.type_param_count);
            new_type->contains_error |= type->fun_type.dom->contains_error;
            new_type->contains_error |= type->fun_type.codom->contains_error;
            new_type->contains_unknown |= type->fun_type.dom->contains_unknown;
            new_type->contains_unknown |= type->fun_type.codom->contains_unknown;
            break;
        default:
            break;
    }

    must_succeed(insert_in_hash_table(&type_table->types, &new_type, hash, sizeof(Type*), compare_types_wrapper));
    return new_type;
}

TypeTable* new_type_table(MemPool* mem_pool) {
    TypeTable* type_table = malloc_or_die(sizeof(TypeTable));
    type_table->types = new_hash_table(sizeof(Type*));
    type_table->str_pool = new_str_pool(mem_pool);
    type_table->mem_pool = mem_pool;
    type_table->type_count = 0;

    for (size_t i = 0; i < PRIM_TYPE_COUNT; ++i)
        type_table->prim_types[i] = get_or_insert_type(type_table, &(Type) { .tag = i });
    type_table->unknown_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_UNKNOWN });
    type_table->noret_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_NORET });
    type_table->error_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_ERROR });
    type_table->unit_type = get_or_insert_type(type_table, &(Type) { .tag = TYPE_TUPLE, .tuple_type.arg_count = 0 });
    return type_table;
}

void free_type_table(TypeTable* type_table) {
    free_hash_table(&type_table->types);
    free_str_pool(&type_table->str_pool); 
    free(type_table);
}

static Type* alloc_nominal_type(TypeTable* type_table, TypeTag tag) {
    assert(tag == TYPE_ENUM || tag == TYPE_STRUCT);
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    memset(type, 0, sizeof(Type));
    type->id = type_table->type_count++;
    type->tag = tag;
    return type;
}

Type* make_struct_type(TypeTable* type_table, const char* name) {
    Type* struct_type = alloc_nominal_type(type_table, TYPE_STRUCT);
    struct_type->struct_type.name = make_str(&type_table->str_pool, name);
    struct_type->struct_type.fields = new_dyn_array(sizeof(StructField));
    struct_type->struct_type.type_params = new_dyn_array(sizeof(Type*));
    return struct_type;
}

Type* make_enum_type(TypeTable* type_table, const char* name) {
    Type* enum_type = alloc_nominal_type(type_table, TYPE_ENUM);
    enum_type->enum_type.name = make_str(&type_table->str_pool, name);
    enum_type->enum_type.options = new_dyn_array(sizeof(EnumOption));
    enum_type->enum_type.type_params = new_dyn_array(sizeof(Type*));
    return enum_type;
}

static int compare_struct_fields_by_name(const void* left, const void* right) {
    return strcmp(((StructField*)left)->name, ((StructField*)right)->name);
}

static int compare_enum_options_by_name(const void* left, const void* right) {
    return strcmp(((EnumOption*)left)->name, ((EnumOption*)right)->name);
}

static StructField* copy_and_sort_struct_fields(TypeTable* type_table, StructField* fields) {
    size_t field_count = get_dyn_array_size(fields);
    StructField* fields_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(StructField) * field_count);
    memcpy(fields_copy, fields, sizeof(StructField) * field_count);
    for (size_t i = 0; i < field_count; ++i)
        fields_copy[i].name = make_str(&type_table->str_pool, fields[i].name);
    qsort(fields_copy, field_count, sizeof(StructField), compare_struct_fields_by_name);
    free_dyn_array(fields);
    return fields_copy;
}

static EnumOption* copy_and_sort_enum_options(TypeTable* type_table, EnumOption* options) {
    size_t option_count = get_dyn_array_size(options);
    EnumOption* options_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(EnumOption) * option_count);
    memcpy(options_copy, options, sizeof(EnumOption) * option_count);
    for (size_t i = 0; i < option_count; ++i)
        options_copy[i].name = make_str(&type_table->str_pool, options[i].name);
    qsort(options_copy, option_count, sizeof(EnumOption), compare_enum_options_by_name);
    free_dyn_array(options);
    return options_copy;
}

static const Type** copy_type_params(TypeTable* type_table, const Type** type_params, size_t* type_param_count) {
    *type_param_count = get_dyn_array_size(type_params);
    const Type** type_params_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * *type_param_count);
    memcpy(type_params_copy, type_params, sizeof(Type*) * *type_param_count);
    free_dyn_array(type_params);
    return type_params_copy;
}

const Type* freeze_struct_type(TypeTable* type_table, Type* type) {
    assert(type->tag == TYPE_STRUCT);
    assert(!type->struct_type.is_frozen);
    type->struct_type.field_count = get_dyn_array_size(type->struct_type.fields);
    type->struct_type.fields = copy_and_sort_struct_fields(type_table, type->struct_type.fields);
    type->struct_type.type_params =
        copy_type_params(type_table, type->struct_type.type_params, &type->struct_type.type_param_count);
#ifndef NDEBUG
    type->struct_type.is_frozen = true;
#endif
    return type;
}

const Type* freeze_enum_type(TypeTable* type_table, Type* type) {
    assert(type->tag == TYPE_ENUM);
    assert(!type->enum_type.is_frozen);
    type->enum_type.option_count = get_dyn_array_size(type->enum_type.options);
    type->enum_type.options = copy_and_sort_enum_options(type_table, type->enum_type.options);
    type->enum_type.type_params =
        copy_type_params(type_table, type->enum_type.type_params, &type->enum_type.type_param_count);
#ifndef NDEBUG
    type->enum_type.is_frozen = true;
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

const Type* make_type_var(TypeTable* type_table, const char* name, Kind kind) {
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    type->tag = TYPE_VAR;
    type->type_var.name = make_str(&type_table->str_pool, name);
    type->type_var.kind = kind;
    type->id = type_table->type_count++;
    return type;
}

const Type* make_tuple_type(TypeTable* type_table, const Type** args, size_t arg_count) {
    if (arg_count == 0)
        return type_table->unit_type;
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_TUPLE,
        .tuple_type = { .args = args, .arg_count = arg_count }
    });
}

const Type* make_unit_type(TypeTable* type_table) {
    return type_table->unit_type;
}

const Type* make_type_app(TypeTable* type_table, const Type* applied_type, const Type** args, size_t arg_count) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_APP,
        .type_app = { .applied_type = applied_type, .args = args, .arg_count = arg_count }
    });
}

const Type* make_array_type(TypeTable* type_table, const Type* elem_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_ARRAY,
        .array_type = { .elem_type = elem_type }
    });
}

const Type* make_ptr_type(TypeTable* type_table, bool is_const, const Type* pointee_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_PTR,
        .ptr_type = { .is_const = is_const, .pointee = pointee_type }
    });
}

const Type* make_fun_type(TypeTable* type_table, const Type* dom, const Type* codom) {
    return make_poly_fun_type(type_table, NULL, 0, dom, codom);
}

const Type* make_type_alias(
    TypeTable* type_table,
    const char* name,
    const Type** type_params,
    size_t type_param_count,
    const Type* aliased_type)
{
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_ALIAS,
        .type_alias = {
            .name = make_str(&type_table->str_pool, name),
            .type_params = type_params,
            .type_param_count = type_param_count,
            .aliased_type = aliased_type,
        }
    });
}

#define LEXICOGRAPHICAL_COMPARE(l, r) \
    if (l < r) return -1; \
    if (l > r) return 1;

static int compare_signature_members(const SignatureMember* left, const SignatureMember* right) {
    int d = strcmp(left->name, right->name);
    LEXICOGRAPHICAL_COMPARE(d, 0)
    LEXICOGRAPHICAL_COMPARE(left->type->id, right->type->id)
    LEXICOGRAPHICAL_COMPARE(left->is_type, right->is_type)
    return 0;
}

#undef LEXICOGRAPHICAL_COMPARE

static int compare_signature_members_wrapper(const void* left, const void* right) {
    return compare_signature_members(left, right);
}

const Type* make_signature_type(
    TypeTable* type_table,
    const Type** type_params,
    size_t type_param_count,
    SignatureMember* members,
    size_t member_count)
{
    for (size_t i = 0; i < member_count; ++i)
        members[i].name = make_str(&type_table->str_pool, members[i].name);
    qsort(members, member_count, sizeof(SignatureMember), compare_signature_members_wrapper);
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_SIGNATURE,
        .signature = {
            .type_params = type_params,
            .type_param_count = type_param_count,
            .members = members,
            .member_count = member_count
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
        .fun_type = {
            .dom = dom,
            .codom = codom,
            .type_params = type_params,
            .type_param_count = type_param_count
        }
    });
}

const Type* replace_types_with_map(TypeTable* type_table, const Type* type, TypeMap* type_map) {
    switch (type->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case TYPE_UNKNOWN:
        case TYPE_NORET:
        case TYPE_ERROR:
        case TYPE_STRUCT:
        case TYPE_ENUM:
            return type;
        case TYPE_TUPLE: {
            const Type** types = malloc(sizeof(Type*) * type->tuple_type.arg_count);
            for (size_t i = 0, n = type->tuple_type.arg_count; i < n; ++i)
                types[i] = replace_types_with_map(type_table, type->tuple_type.args[i], type_map);
            type = make_tuple_type(type_table, types, type->tuple_type.arg_count);
            free(types);
            return type;
        }
        case TYPE_ALIAS: {
            return make_type_alias(type_table,
                type->type_alias.name,
                type->type_alias.type_params,
                type->type_alias.type_param_count,
                replace_types_with_map(type_table, type->type_alias.aliased_type, type_map));
        }
        case TYPE_SIGNATURE: {
            SignatureMember* members = malloc(sizeof(SignatureMember) * type->signature.member_count);
            memcpy(members, type->signature.members, sizeof(SignatureMember) * type->signature.member_count);
            for (size_t i = 0, n = type->signature.member_count; i < n; ++i)
                members[i].type = replace_types_with_map(type_table, members[i].type, type_map);
            type = make_signature_type(type_table,
                type->signature.type_params,
                type->signature.type_param_count, members,
                type->signature.member_count);
            free(members);
            return type;
        }
        case TYPE_APP: {
            const Type** args = malloc(sizeof(Type*) * type->type_app.arg_count);
            for (size_t i = 0, n = type->type_app.arg_count; i < n; ++i)
                args[i] = replace_types_with_map(type_table, type->type_app.args[i], type_map);
            type = make_type_app(type_table,
                replace_types_with_map(type_table, type->type_app.applied_type, type_map),
                args, type->type_app.arg_count);
            free(args);
            return type;
        }
        case TYPE_FUN: {
            return make_poly_fun_type(type_table,
                type->fun_type.type_params,
                type->fun_type.type_param_count,
                replace_types_with_map(type_table, type->fun_type.dom, type_map),
                replace_types_with_map(type_table, type->fun_type.codom, type_map));
        }
        case TYPE_ARRAY: {
            return make_array_type(type_table,
                replace_types_with_map(type_table, type->array_type.elem_type, type_map));
        }
        case TYPE_VAR: {
            const Type* mapped_type = find_type_in_map(type_map, type);
            return mapped_type ? mapped_type : type;
        }
        case TYPE_PTR: {
            return make_ptr_type(type_table,
                type->ptr_type.is_const,
                replace_types_with_map(type_table, type->ptr_type.pointee, type_map));
        }
        default:
            assert(false && "invalid type");
            break;
    }
}

const Type* replace_types(
    TypeTable* type_table,
    const Type* type,
    const Type** src_types,
    const Type** dst_types,
    size_t type_count)
{
    TypeMap type_map = new_type_map();
    for (size_t i = 0; i < type_count; ++i)
        insert_type_in_map(&type_map, src_types[i], dst_types[i]);
    const Type* replaced_type = replace_types_with_map(type_table, type, &type_map);
    free_type_map(&type_map);
    return replaced_type;
}
