#include "fu/lang/type_table.h"
#include "fu/core/mem_pool.h"
#include "fu/core/alloc.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>

#define DEFAULT_TYPE_TABLE_CAPACITY 16

TypeTable new_type_table(MemPool* mem_pool) {
    return (TypeTable) {
        .types = new_hash_table(DEFAULT_TYPE_TABLE_CAPACITY, sizeof(Type*)),
        .str_pool = new_str_pool(mem_pool),
        .mem_pool = mem_pool
    };
}

void free_type_table(TypeTable* type_table) {
    free_hash_table(&type_table->types);
    free_str_pool(&type_table->str_pool); 
}

static Type* make_struct_or_enum_type(TypeTable* type_table, TypeTag tag, const char* name, size_t member_count) {
    Type* type = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type));
    type->tag = tag;
    type->struct_type.name = make_str(&type_table->str_pool, name);
    type->struct_type.members = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * member_count);
    type->struct_type.member_names = alloc_from_mem_pool(type_table->mem_pool, sizeof(char*) * member_count);
    type->struct_type.member_count = member_count;
    type->struct_type.child_types = NULL;
    type->id = type_table->type_count++;
    return type;
}

Type* make_struct_type(TypeTable* type_table, const char* name, size_t field_count) {
    return make_struct_or_enum_type(type_table, TYPE_STRUCT, name, field_count);
}

Type* make_enum_type(TypeTable* type_table, const char* name, size_t option_count) {
    return make_struct_or_enum_type(type_table, TYPE_ENUM, name, option_count);
}

static uint32_t hash_type(uint32_t hash, const Type* type) {
    hash = hash_uint32(hash, type->tag);
    switch (type->tag) {
        case TYPE_TUPLE:
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i)
                hash = hash_uint64(hash, type->tuple_type.arg_types[i]->id);
            break;
        case TYPE_APP:
            hash = hash_uint64(hash, type->type_app.applied_type->id);
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i)
                hash = hash_uint64(hash, type->type_app.type_args[i]->id);
            break;
        case TYPE_FUN:
            hash = hash_uint64(hash, type->fun_type.dom_type->id);
            hash = hash_uint64(hash, type->fun_type.codom_type->id);
            break;
        case TYPE_ARRAY:
            hash = hash_uint64(hash, type->array_type.elem_type->id);
            break;
        case TYPE_PARAM:
            hash = hash_str(hash, type->type_param.name);
            break;
        default:
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
        case TYPE_TUPLE:
            if (type_left->tuple_type.arg_count != type_right->tuple_type.arg_count)
                return false;
            for (size_t i = 0; i < type_left->tuple_type.arg_count; ++i) {
                if (type_left->tuple_type.arg_types[i] != type_right->tuple_type.arg_types[i])
                    return false;
            }
            break;
        case TYPE_APP:
            if (type_left->type_app.arg_count != type_right->type_app.arg_count ||
                type_left->type_app.applied_type != type_right->type_app.applied_type)
                return false;
            for (size_t i = 0; i < type_left->type_app.arg_count; ++i) {
                if (type_left->type_app.type_args[i] != type_right->type_app.type_args[i])
                    return false;
            }
            break;
        case TYPE_FUN:
            return
                type_left->fun_type.dom_type == type_right->fun_type.dom_type &&
                type_left->fun_type.codom_type == type_right->fun_type.codom_type;
        case TYPE_ARRAY:
            return type_left->array_type.elem_type == type_right->array_type.elem_type;
        case TYPE_PARAM:
            return type_left->type_param.name == type_right->type_param.name;
        default:
            break;
    }
    return true;
}

static const Type** copy_types(TypeTable* type_table, const Type** types, size_t count) {
    const Type** types_copy = alloc_from_mem_pool(type_table->mem_pool, sizeof(Type*) * count);
    memcpy(types_copy, types, sizeof(Type*) * count);
    return types_copy;
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
            new_type->tuple_type.arg_types =
                copy_types(type_table, type->tuple_type.arg_types, type->tuple_type.arg_count);
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i) {
                new_type->contains_error   |= type->tuple_type.arg_types[i]->contains_error;
                new_type->contains_unknown |= type->tuple_type.arg_types[i]->contains_unknown;
            }
            break;
        case TYPE_APP:
            new_type->type_app.type_args =
                copy_types(type_table, type->type_app.type_args, type->type_app.arg_count);
            new_type->contains_error |= type->type_app.applied_type->contains_error;
            new_type->contains_unknown |= type->type_app.applied_type->contains_unknown;
            for (size_t i = 0; i < type->type_app.arg_count; ++i) {
                new_type->contains_error   |= type->type_app.type_args[i]->contains_error;
                new_type->contains_unknown |= type->type_app.type_args[i]->contains_unknown;
            }
            break;
        case TYPE_ARRAY:
            new_type->contains_error |= type->array_type.elem_type->contains_error;
            new_type->contains_unknown |= type->array_type.elem_type->contains_unknown;
            break;
        case TYPE_FUN:
            new_type->contains_error |= type->fun_type.dom_type->contains_error;
            new_type->contains_error |= type->fun_type.codom_type->contains_error;
            new_type->contains_unknown |= type->fun_type.dom_type->contains_unknown;
            new_type->contains_unknown |= type->fun_type.codom_type->contains_unknown;
            break;
        default:
            break;
    }

    must_succeed(insert_in_hash_table(&type_table->types, &new_type, hash, sizeof(Type*), compare_types));
    return new_type;
}

const Type* make_prim_type(TypeTable* type_table, TypeTag tag) {
    assert(is_prim_type(tag));
    return get_or_insert_type(type_table, &(Type) { .tag = tag });
}

const Type* make_unknown_type(TypeTable* type_table) {
    return get_or_insert_type(type_table, &(Type) { .tag = TYPE_UNKNOWN });
}

const Type* make_error_type(TypeTable* type_table) {
    return get_or_insert_type(type_table, &(Type) { .tag = TYPE_ERROR });
}

const Type* make_noret_type(TypeTable* type_table) {
    return get_or_insert_type(type_table, &(Type) { .tag = TYPE_NORET });
}

const Type* make_type_param(TypeTable* type_table, const char* name) {
    name = make_str(&type_table->str_pool, name);
    return get_or_insert_type(type_table, &(Type) { .tag = TYPE_PARAM, .type_param.name = name });
}

const Type* make_tuple_type(TypeTable* type_table, const Type** arg_types, size_t arg_count) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_TUPLE,
        .tuple_type = { .arg_types = arg_types, .arg_count = arg_count }
    });
}

const Type* make_unknown_tuple_type(TypeTable* type_table, size_t arg_count) {
    const Type** arg_types = malloc_or_die(sizeof(Type*) * arg_count);
    const Type* unknown_type = make_unknown_type(type_table);
    for (size_t i = 0; i < arg_count; ++i)
        arg_types[i] = unknown_type;
    const Type* result = make_tuple_type(type_table, arg_types, arg_count);
    free(arg_types);
    return result;
}

const Type* make_type_app(TypeTable* type_table, const Type* applied_type, const Type** type_args, size_t arg_count) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_APP,
        .type_app = { .applied_type = applied_type, .type_args = type_args, .arg_count = arg_count }
    });
}

const Type* make_fun_type(TypeTable* type_table, const Type* dom_type, const Type* codom_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_FUN,
        .fun_type = { .dom_type = dom_type, .codom_type = codom_type }
    });
}

const Type* make_array_type(TypeTable* type_table, const Type* elem_type) {
    return get_or_insert_type(type_table, &(Type) {
        .tag = TYPE_ARRAY,
        .array_type = { .elem_type = elem_type }
    });
}
