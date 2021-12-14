#include "lang/type.h"
#include "core/hash_table.h"
#include "core/alloc.h"
#include "core/mem_pool.h"
#include "core/hash.h"
#include "core/utils.h"

#include <stdlib.h>
#include <string.h>

enum {
#define f(x, ...) PRIM_TYPE_##x,
    PRIM_TYPE_LIST(f)
#undef f
    PRIM_TYPE_COUNT
};

struct type_table {
    struct hash_table types;
    struct mem_pool mem_pool;
    const struct type* prim_types[PRIM_TYPE_COUNT];
    const struct type* error_type;
    const struct type* noret_type;
    const struct type* unit_type;
};

bool is_prim_type(const struct type* type) {
    switch (type->tag) {
#define f(x, ...) case TYPE_##x:
        PRIM_TYPE_LIST(f)
#undef f
            return true;
        default:
            return false;
    }
}

bool is_nominal_type(const struct type* type) {
    return
        type->tag == TYPE_STRUCT ||
        type->tag == TYPE_ENUM ||
        type->tag == TYPE_VAR;
}

static uint32_t hash_type(const struct type* type) {
    uint32_t hash = hash_uint32(hash_init(), type->tag);
    switch (type->tag) {
        case TYPE_FUN:
            hash = hash_pointer(hash, type->fun_type.dom);
            hash = hash_pointer(hash, type->fun_type.codom);
            break;
        case TYPE_TUPLE:
            for (size_t i = 0; i < type->tuple_type.arg_count; ++i)
                hash = hash_pointer(hash, type->tuple_type.args);
            break;
        default:
            break;
    }
    return hash;
}

static bool compare_types(const void* left, const void* right) {
    const struct type* left_type  = *(struct type**)left;
    const struct type* right_type = *(struct type**)right;
    if (left_type->tag != right_type->tag)
        return false;
    switch (left_type->tag) {
        case TYPE_FUN:
            return
                left_type->fun_type.dom == right_type->fun_type.dom &&
                left_type->fun_type.codom == right_type->fun_type.codom;
        case TYPE_TUPLE:
            if (left_type->tuple_type.arg_count != right_type->tuple_type.arg_count)
                return false;
            for (size_t i = 0; i < left_type->tuple_type.arg_count; ++i) {
                if (left_type->tuple_type.args[i] != right_type->tuple_type.args[i])
                    return false;
            }
            return true;
        default:
            return true;
    }
}

static const struct type* insert_type(struct type_table* type_table, const struct type* type) {
    assert(!is_nominal_type(type));
    uint32_t hash = hash_type(type);
    struct type** type_ptr = find_in_hash_table(&type_table->types, &type, hash, sizeof(struct type*), compare_types);
    if (type_ptr)
        return *type_ptr;
    struct type* new_type = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type));
    memcpy(new_type, type, sizeof(struct type));
    switch (type->tag) {
        case TYPE_FUN:
            new_type->contains_error = type->fun_type.dom->contains_error || type->fun_type.codom->contains_error;
            break;
        case TYPE_TUPLE:
            new_type->tuple_type.args = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type*) * type->tuple_type.arg_count);
            memcpy(new_type->tuple_type.args, type->tuple_type.args, sizeof(struct type*) * type->tuple_type.arg_count);
            new_type->contains_error = false;
            for (size_t i = 0; !new_type->contains_error && i < type->tuple_type.arg_count; ++i)
                new_type->contains_error   |= type->tuple_type.args[i]->contains_error;
            break;
        default:
            new_type->contains_error = false;
            break;
    }
    must_succeed(insert_in_hash_table(&type_table->types, &new_type, hash, sizeof(struct type*), compare_types));
    return new_type;
}

struct type_table* new_type_table(void) {
    struct type_table* type_table = malloc_or_die(sizeof(struct type_table));
    type_table->types = new_hash_table(PRIM_TYPE_COUNT, sizeof(struct type*));
    type_table->mem_pool = new_mem_pool();
#define f(x, ...) \
    type_table->prim_types[PRIM_TYPE_##x] = insert_type(type_table, &(struct type) { .tag = TYPE_##x });
    PRIM_TYPE_LIST(f)
#undef f
    type_table->error_type = insert_type(type_table, &(struct type) { .tag = TYPE_ERROR });
    type_table->noret_type = insert_type(type_table, &(struct type) { .tag = TYPE_NORET });
    type_table->unit_type  = insert_type(type_table, &(struct type) { .tag = TYPE_TUPLE, .tuple_type.arg_count = 0 });
    return type_table;
}

void free_type_table(struct type_table* type_table) {
    free_hash_table(&type_table->types);
    free_mem_pool(&type_table->mem_pool);
    free(type_table);
}

const struct type* make_error_type(struct type_table* type_table) {
    return type_table->error_type;
}

const struct type* make_prim_type(struct type_table* type_table, enum type_tag tag) {
    assert((int)tag < (int)PRIM_TYPE_COUNT);
    return type_table->prim_types[tag];
}

const struct type* make_unit_type(struct type_table* type_table) {
    return type_table->unit_type;
}

const struct type* make_tuple_type(struct type_table* type_table, const struct type** args, size_t arg_count) {
    if (arg_count == 0)
        return make_unit_type(type_table);
    return insert_type(type_table, &(struct type) {
        .tag = TYPE_TUPLE,
        .tuple_type = { .args = args, .arg_count = arg_count }
    });
}

const struct type* make_array_type(struct type_table* type_table, const struct type* elem, size_t elem_count) {
    return insert_type(type_table, &(struct type) {
        .tag = TYPE_ARRAY,
        .array_type = { .elem = elem, .elem_count = elem_count }
    });
}

const struct type* make_fun_type(struct type_table* type_table, const struct type* dom, const struct type* codom) {
    return insert_type(type_table, &(struct type) {
        .tag = TYPE_TUPLE,
        .fun_type = { .dom = dom, .codom = codom }
    });
}

const struct type* new_type_var(struct type_table* type_table, const char* name) {
    struct type* type_var = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type));
    type_var->type_var.name = copy_string_with_mem_pool(&type_table->mem_pool, name);
    type_var->tag = TYPE_VAR;
    return type_var;
}

struct type* new_struct_or_enum_type(
    struct type_table* type_table,
    enum type_tag tag,
    const char* name,
    size_t member_count,
    size_t subtype_count,
    size_t type_var_count,
    struct file_loc* loc)
{
    assert(tag == TYPE_STRUCT || tag == TYPE_ENUM);
    struct type* type = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type));
    memset(type, 0, sizeof(struct type));
    type->tag = tag;
    type->struct_type.name = copy_string_with_mem_pool(&type_table->mem_pool, name);
    type->struct_type.members   = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type_member) * member_count);
    type->struct_type.subtypes  = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type*) * subtype_count);
    type->struct_type.type_vars = alloc_from_mem_pool(&type_table->mem_pool, sizeof(struct type*) * type_var_count);
    type->struct_type.member_count = member_count;
    type->struct_type.subtype_count = subtype_count;
    type->struct_type.type_var_count = type_var_count;
    type->struct_type.loc = *loc;
    return type;
}

bool is_subtype(const struct type* from, const struct type* to) {
    if (from == to || from->tag == TYPE_ERROR || from->tag == TYPE_NORET)
        return true;
    if (from->tag == to->tag) {
        if (from->tag == TYPE_TUPLE && from->tuple_type.arg_count == to->tuple_type.arg_count) {
            for (size_t i = 0, n = from->tuple_type.arg_count; i < n; ++i) {
                if (!is_subtype(from->tuple_type.args[i], to->tuple_type.args[i]))
                    return false;
            }
            return true;
        } else if (from->tag == TYPE_FUN) {
            return
                is_subtype(to->fun_type.dom, from->fun_type.dom) &&
                is_subtype(from->fun_type.codom, to->fun_type.codom);
        }
    }
    return false;
}

void print_type(struct format_state* state, const struct type* type) {
    switch (type->tag) {
#define f(x, str) \
        case TYPE_##x: \
            format(state, "{$}{s}{$}", (union format_arg[]) { \
                { .style = keyword_style }, \
                { .s = str }, \
                { .style = reset_style } \
            }); \
            break;
        PRIM_TYPE_LIST(f)
#undef f
        case TYPE_FUN:
            format(state, "{$}fun{$} ", (union format_arg[]) { { .style = keyword_style }, { .style = reset_style } });
            print_type(state, type->fun_type.dom);
            format(state, " -> ", NULL);
            print_type(state, type->fun_type.codom);
            break;
        case TYPE_TUPLE:
            format(state, "(", NULL);
            for (size_t i = 0, n = type->tuple_type.arg_count; i < n; ++i) {
                print_type(state, type->tuple_type.args[i]);
                if (i != n - 1)
                    format(state, ", ", NULL);
            }
            format(state, ")", NULL);
            break; 
        case TYPE_STRUCT:
            format(state, "{$}struct{$} {}", (union format_arg[]) {
                { .style = keyword_style },
                { .style = reset_style },
                { .s = type->struct_type.name }
            });
            break;
        case TYPE_ENUM:
            format(state, "{$}enum{$} {}", (union format_arg[]) {
                { .style = keyword_style },
                { .style = reset_style },
                { .s = type->enum_type.name }
            });
            break;
        case TYPE_VAR:
            format(state, "{}", (union format_arg[]) { { .s = type->type_var.name } });
            break;
        case TYPE_ERROR:
            format(state, "{$}<error>{$}", (union format_arg[]) {
                { .style = error_style },
                { .style = reset_style }
            });
            break;
        default:
            assert(false && "unsupported type");
            break;
    }
}
