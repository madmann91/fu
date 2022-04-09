#include "fu/lang/type_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/str_pool.h"
#include "fu/core/alloc.h"
#include "fu/core/hash_table.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"
#include "fu/core/sort.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define DEFAULT_TYPE_TABLE_CAPACITY 16

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

static uint32_t hash_type(uint32_t hash, const Type* type) {
    hash = hash_uint32(hash, type->tag);
    switch (type->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
        case TYPE_UNKNOWN:
        case TYPE_NORET:
        case TYPE_ERROR:
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
            for (size_t i = 0; i < type->fun_type.type_param_count; ++i)
                hash = hash_uint64(hash, type->fun_type.type_params[i]->id);
            hash = hash_uint64(hash, type->fun_type.dom->id);
            hash = hash_uint64(hash, type->fun_type.codom->id);
            break;
        case TYPE_SIG:
            for (size_t i = 0; i < type->sig_type.type_param_count; ++i)
                hash = hash_uint64(hash, type->sig_type.type_params[i]->id);
            for (size_t i = 0; i < type->sig_type.member_count; ++i) {
                hash = hash_str(hash, type->sig_type.members[i].name);
                hash = hash_uint8(hash, type->sig_type.members[i].is_type);
                hash = hash_uint64(hash, type->sig_type.members[i].type->id);
            }
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

static bool compare_types(const void* left, const void* right) {
    const Type* type_left = *(const Type**)left;
    const Type* type_right = *(const Type**)right;
    if (type_left->tag != type_right->tag)
        return false;
    switch (type_left->tag) {
#define f(name, ...) case TYPE_##name:
    PRIM_TYPE_LIST(f)
#undef f
            break;
        case TYPE_TUPLE:
            if (type_left->tuple_type.arg_count != type_right->tuple_type.arg_count)
                return false;
            for (size_t i = 0; i < type_left->tuple_type.arg_count; ++i) {
                if (type_left->tuple_type.args[i] != type_right->tuple_type.args[i])
                    return false;
            }
            break;
        case TYPE_APP:
            if (type_left->type_app.arg_count != type_right->type_app.arg_count ||
                type_left->type_app.applied_type != type_right->type_app.applied_type)
                return false;
            for (size_t i = 0; i < type_left->type_app.arg_count; ++i) {
                if (type_left->type_app.args[i] != type_right->type_app.args[i])
                    return false;
            }
            break;
        case TYPE_FUN: {
            size_t params_size = sizeof(Type*) * type_left->fun_type.type_param_count;
            return
                type_left->fun_type.type_param_count == type_right->fun_type.type_param_count &&
                type_left->fun_type.dom == type_right->fun_type.dom &&
                type_left->fun_type.codom == type_right->fun_type.codom &&
                !memcmp(type_left->fun_type.type_params, type_right->fun_type.type_params, params_size);
        }
        case TYPE_SIG: {
            size_t params_size  = sizeof(Type*)      * type_left->sig_type.type_param_count;
            size_t members_size = sizeof(TypeMember) * type_left->sig_type.member_count;
            return
                type_left->sig_type.type_param_count == type_right->sig_type.type_param_count &&
                type_left->sig_type.member_count     == type_right->sig_type.member_count &&
                !memcmp(type_left->sig_type.type_params, type_right->sig_type.type_params, params_size) &&
                !memcmp(type_left->sig_type.members, type_right->sig_type.members, members_size);
        }
        case TYPE_ARRAY:
            return type_left->array_type.elem_type == type_right->array_type.elem_type;
        case TYPE_VAR:
            return type_left->type_var.name == type_right->type_var.name;
        case TYPE_PTR:
            return
                type_left->ptr_type.is_const == type_right->ptr_type.is_const &&
                type_left->ptr_type.pointee  == type_right->ptr_type.pointee;
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

static const TypeMember* copy_members(TypeTable* type_table, const TypeMember* members, size_t count) {
    TypeMember* members_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(TypeMember) * count);
    memcpy(members_copy, members, sizeof(TypeMember) * count);
    return members_copy;
}

static const Type* get_or_insert_type(TypeTable* type_table, const Type* type) {
    assert(!is_nominal_type(type->tag));

    uint32_t hash = hash_type(hash_init(), type);
    const Type** type_ptr = find_in_hash_table(&type_table->types, &type, hash, sizeof(Type*), compare_types);
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
            new_type->contains_error |= type->array_type.elem_type->contains_error;
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
        case TYPE_SIG:
            new_type->sig_type.type_params =
                copy_types(type_table, type->sig_type.type_params, type->sig_type.type_param_count);
            new_type->sig_type.members =
                copy_members(type_table, type->sig_type.members, type->sig_type.member_count);
            for (size_t i = 0; i < type->sig_type.member_count; ++i) {
                new_type->contains_error   |= type->sig_type.members[i].type->contains_error;
                new_type->contains_unknown |= type->sig_type.members[i].type->contains_unknown;
            }
            break;
        default:
            break;
    }

    must_succeed(insert_in_hash_table(&type_table->types, &new_type, hash, sizeof(Type*), compare_types));
    return new_type;
}

TypeTable* new_type_table(MemPool* mem_pool) {
    TypeTable* type_table = malloc_or_die(sizeof(TypeTable));
    type_table->types = new_hash_table(DEFAULT_TYPE_TABLE_CAPACITY, sizeof(Type*));
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

TypeMember make_type_member(
    TypeTable* type_table,
    const char* name,
    const Type* type,
    bool is_type,
    bool has_default)
{
    return (TypeMember) {
        .name = make_str(&type_table->str_pool, name),
        .type = type,
        .is_type = is_type,
        .has_default = has_default
    };
}

Type* make_struct_or_enum_type(
    TypeTable* type_table,
    TypeTag tag,
    const char* name,
    size_t member_count,
    size_t type_param_count)
{
    assert(tag == TYPE_STRUCT || tag == TYPE_ENUM);
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    type->tag = tag;
    type->struct_type.name = make_str(&type_table->str_pool, name);
    type->struct_type.members = alloc_from_mem_pool(type_table->mem_pool, sizeof(TypeMember) * member_count);
    type->struct_type.type_params = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * type_param_count);
    type->struct_type.member_count = member_count;
    type->struct_type.type_param_count = type_param_count;
    type->id = type_table->type_count++;
    return type;
}

Type* make_alias_type(TypeTable* type_table, const char* name, size_t type_param_count) {
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    type->tag = TYPE_ALIAS;
    type->struct_type.name = make_str(&type_table->str_pool, name);
    type->struct_type.type_params = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * type_param_count);
    type->struct_type.type_param_count = type_param_count;
    type->id = type_table->type_count++;
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

const Type* make_fun_type(TypeTable* type_table, const Type* dom, const Type* codom) {
    return make_poly_fun_type(type_table, NULL, 0, dom, codom);
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

static inline bool is_member_smaller_than(const TypeMember* left, const TypeMember* right) {
    return
        left->is_type < right->is_type || (left->is_type == right->is_type &&
        (left->type->id < right->type->id || (left->type->id == right->type->id &&
        strcmp(left->name, right->name) < 0)));
}

DECLARE_SHELL_SORT(sort_members, TypeMember, is_member_smaller_than)

const Type* make_sig_type(
    TypeTable* type_table,
    TypeMember* members,
    size_t member_count,
    const Type** type_params,
    size_t type_param_count)
{
    // This ensures that signatures that have the same members compare equal,
    // regardless of the order in which members are declared.
    sort_members(members, member_count);
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_SIG,
        .sig_type = {
            .members = members,
            .member_count = member_count,
            .type_params = type_params,
            .type_param_count = type_param_count
        }
    });
}
