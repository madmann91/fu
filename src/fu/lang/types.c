#include "fu/lang/types.h"
#include "fu/core/mem_pool.h"
#include "fu/core/hash.h"
#include "fu/core/utils.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <string.h>

#define DEFAULT_TYPE_TABLE_CAPACITY 16

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

bool is_int_type(TypeTag tag) {
    switch (tag) {
        case TYPE_I8:
        case TYPE_I16:
        case TYPE_I32:
        case TYPE_I64:
        case TYPE_U8:
        case TYPE_U16:
        case TYPE_U32:
        case TYPE_U64:
            return true;
        default:
            return false;
    }
}

bool is_int_or_float_type(TypeTag tag) {
    return is_int_type(tag) || is_float_type(tag);
}

void set_member_name(TypeTable* type_table, Type* type, size_t i, const char* name) {
    assert(i < type->struct_type.member_count);
    type->struct_type.member_names[i] = make_str(&type_table->str_pool, name);
}

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

static const Type** merge_many_types(TypeTable* type_table, const Type** from_types, const Type** to_types, size_t count) {
    const Type** merged_types = malloc_or_die(sizeof(Type*) * count);
    for (size_t i = 0; i < count; ++i)
        merged_types[i] = merge_types(type_table, from_types[i], to_types[i]);
    return merged_types;
}

const Type* merge_types(TypeTable* type_table, const Type* from, const Type* to) {
    if (from == to || from->tag == TYPE_UNKNOWN) return to;
    if (to->tag == TYPE_UNKNOWN) return from;
    if (from->tag != to->tag || is_nominal_type(from->tag))
        return make_error_type(type_table);
    switch (from->tag) {
        case TYPE_TUPLE: {
            if (from->tuple_type.arg_count != to->tuple_type.arg_count)
                break;
            const Type** arg_types = merge_many_types(type_table,
                from->tuple_type.arg_types, to->tuple_type.arg_types, from->tuple_type.arg_count);
            const Type* result = make_tuple_type(type_table, arg_types, from->tuple_type.arg_count);
            free(arg_types);
            return result;
        }
        case TYPE_APP: {
            if (from->type_app.arg_count != to->type_app.arg_count)
                break;
            const Type** arg_types = merge_many_types(type_table,
                from->tuple_type.arg_types, to->tuple_type.arg_types, from->tuple_type.arg_count);
            const Type* result = make_type_app(type_table,
                merge_types(type_table, from->type_app.applied_type, to->type_app.applied_type),
                arg_types, from->type_app.arg_count);
            free(arg_types);
            return result;
        }
        case TYPE_ARRAY:
            return make_array_type(type_table,
                merge_types(type_table, from->array_type.elem_type, to->array_type.elem_type));
        case TYPE_FUN:
            return make_fun_type(type_table,
                merge_types(type_table, to->fun_type.dom_type, from->fun_type.dom_type),
                merge_types(type_table, from->fun_type.codom_type, to->fun_type.codom_type));
        default:
            assert(false && "invalid structural type");
            break;
    }
    return make_error_type(type_table);
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
