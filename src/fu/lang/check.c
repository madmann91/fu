#include "fu/lang/check.h"
#include "fu/core/alloc.h"

#include <assert.h>
#include <stdlib.h>

TypingContext make_typing_context(TypeTable* type_table, Log* log) {
    return (TypingContext) {
        .log = log,
        .type_table = type_table
    };
}

static const Type* expect_type(
    TypingContext* context, const Type* type, const Type* expected_type, const FileLoc* file_loc)
{
    const Type* merged_type = merge_types(context->type_table, type, expected_type);
    // Do not report error messages when they have already been reported
    if (merged_type->contains_error && !expected_type->contains_error && !type->contains_error) {
        log_error(context->log, file_loc, "expected type '{t}', but got '{t}'",
            (FormatArg[]) { { .t = expected_type }, { .t = type } });
        return make_error_type(context->type_table);
    }
    return merged_type;
}

static const Type* fail_expect(
    TypingContext* context, const char* msg, const Type* type, bool expect_dir, const FileLoc* file_loc)
{
    log_error(context->log, file_loc,
        expect_dir ? "expected type '{1:t}', but got {0:s}" : "expected {s}, but got '{t}'",
        (FormatArg[]) { { .s = msg }, { .t = type } });
    return make_error_type(context->type_table);
}

static const Type* fail_infer(TypingContext* context, const char* msg, const FileLoc* file_loc) {
    log_error(context->log, file_loc, "cannot infer type for {s}", (FormatArg[]) { { .s = msg } });
    return make_error_type(context->type_table);
}

static const Type* expect_int_or_float_literal(
    TypingContext* context, const Type* type, const FileLoc* file_loc)
{
    if (!is_int_or_float_type(type->tag) && !type->contains_error)
        return fail_expect(context, "integer or floating-point literal", type, true, file_loc);
    return type;
}

static const Type* expect_float_literal(
    TypingContext* context, const Type* type, const FileLoc* file_loc)
{
    if (!is_float_type(type->tag) && !type->contains_error)
        return fail_expect(context, "floating-point literal", type, true, file_loc);
    return type;
}

static const Type* infer_path(TypingContext* context, AstNode* path) {
    // TODO
    return make_error_type(context->type_table);
}

const Type* infer_type(TypingContext* context, AstNode* type) {
    switch (type->tag) {
#define f(name, ...) case AST_TYPE_##name: return make_prim_type(context->type_table, TYPE_##name);
        AST_PRIM_TYPE_LIST(f)
#undef f
        case AST_TUPLE_TYPE: {
            size_t arg_count = get_ast_list_length(type->tuple_type.args);
            const Type** arg_types = malloc_or_die(sizeof(Type*) * arg_count);
            AstNode* arg = type->tuple_type.args;
            for (size_t i = 0; arg; ++i, arg = arg->next)
                arg_types[i] = infer_type(context, arg);
            const Type* result = make_tuple_type(context->type_table, arg_types, arg_count);
            free(arg_types);
            return result;
        }
        default:
            assert(false && "invalid type");
            return make_error_type(context->type_table);
    }
}

const Type* check_expr(TypingContext* context, AstNode* expr, const Type* proto_type) {
    switch (expr->tag) {
        case AST_INT_LITERAL:
            if (proto_type->tag != TYPE_UNKNOWN)
                return expect_int_or_float_literal(context, proto_type, &expr->file_loc);
            return make_prim_type(context->type_table, TYPE_I32);
        case AST_FLOAT_LITERAL:
            if (proto_type->tag != TYPE_UNKNOWN)
                return expect_float_literal(context, proto_type, &expr->file_loc);
            return make_prim_type(context->type_table, TYPE_F64);
        case AST_BOOL_LITERAL:
            return expect_type(context,
                make_prim_type(context->type_table, TYPE_BOOL),
                proto_type, &expr->file_loc);
        default:
            assert(false && "invalid expression");
            return make_error_type(context->type_table);
    }
}

const Type* check_pattern(TypingContext* context, AstNode* pattern, const Type* proto_type) {
    switch (pattern->tag) {
        case AST_PATH: {
            if (pattern->path.elems->next || pattern->path.elems->path_elem.type_args)
                return expect_type(context, proto_type, infer_path(context, pattern), &pattern->file_loc);
            else if (!proto_type->contains_unknown)
                return proto_type;
            return fail_infer(context, "pattern", &pattern->file_loc);
        }
        case AST_TYPED_PATTERN:
            return expect_type(context, proto_type,
                infer_type(context, pattern->typed_pattern.type), &pattern->file_loc);
        default:
            assert(false && "invalid pattern");
            return make_error_type(context->type_table);
    }
}

static const Type* check_pattern_and_expr(TypingContext* context, AstNode* pattern, AstNode* expr, const Type* proto_type) {
    if (pattern->tag == AST_TUPLE_PATTERN && expr->tag == AST_TUPLE_EXPR &&
        (proto_type->tag == TYPE_UNKNOWN || proto_type->tag == TYPE_TUPLE))
    {
        size_t arg_count = get_ast_list_length(pattern->tuple_pattern.args);
        if (arg_count == get_ast_list_length(expr->tuple_expr.args) &&
            (proto_type->tag != TYPE_TUPLE || proto_type->tuple_type.arg_count == arg_count))
        {
            AstNode* expr_arg = expr->tuple_expr.args;
            AstNode* pattern_arg = pattern->tuple_pattern.args;
            const Type** arg_types = malloc_or_die(sizeof(Type*) * arg_count);
            for (size_t i = 0; i < arg_count; ++i, expr_arg = expr_arg->next, pattern_arg = pattern_arg->next) {
                const Type* arg_type = proto_type->tag == TYPE_TUPLE
                    ? proto_type->tuple_type.arg_types[i]
                    : make_unknown_type(context->type_table);
                arg_types[i] = check_pattern_and_expr(context, pattern_arg, expr_arg, arg_type);
            }
            const Type* result = make_tuple_type(context->type_table, arg_types, arg_count);
            free(arg_types);
            return result;
        }
    } else if (pattern->tag == AST_TYPED_PATTERN)
        return check_expr(context, expr, check_pattern(context, pattern, proto_type));

    return check_pattern(context, pattern, check_expr(context, expr, proto_type));
}

static const Type* check_const_or_var_decl(TypingContext* context, AstNode* decl, const Type* proto_type) {
    return check_pattern_and_expr(context, decl->var_decl.pattern, decl->var_decl.init, proto_type);
}

static const Type* check_struct_decl(TypingContext* context, AstNode* struct_decl, const Type* proto_type) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* check_enum_decl(TypingContext* context, AstNode* enum_decl, const Type* proto_type) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* check_type_decl(TypingContext* context, AstNode* type_decl, const Type* proto_type) {
    // TODO
    return make_error_type(context->type_table);
}

static const Type* check_fun_decl(TypingContext* context, AstNode* fun_decl, const Type* proto_type) {
    // TODO
    return make_error_type(context->type_table);
}

const Type* check_decl(TypingContext* context, AstNode* decl, const Type* proto_type) {
    switch (decl->tag) {
        case AST_FUN_DECL:
            return check_fun_decl(context, decl, proto_type);
        case AST_STRUCT_DECL:
            return check_struct_decl(context, decl, proto_type);
        case AST_ENUM_DECL:
            return check_enum_decl(context, decl, proto_type);
        case AST_TYPE_DECL:
            return check_type_decl(context, decl, proto_type);
        case AST_VAR_DECL:
        case AST_CONST_DECL:
            return check_const_or_var_decl(context, decl, proto_type);
        default:
            assert(false && "invalid declaration");
            return proto_type;
    }
}

void check_program(TypingContext* context, AstNode* program) {
    for (AstNode* decl = program->program.decls; decl; decl = decl->next)
        check_decl(context, decl, make_unknown_type(context->type_table));
}
