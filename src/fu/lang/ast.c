#include "fu/lang/ast.h"
#include "fu/lang/types.h"
#include "fu/core/utils.h"

#include <limits.h>
#include <assert.h>

static inline void print_many_asts(FormatState* state, const char* sep, const AstNode* elems) {
    for (; elems; elems = elems->next) {
        print_ast(state, elems);
        if (elems->next)
            format(state, sep, NULL);
    }
}

static inline void print_many_asts_with_delim(
    FormatState* state,
    const char* open,
    const char* sep,
    const char* close,
    const AstNode* elems)
{
    format(state, open, NULL);
    print_many_asts(state, sep, elems);
    format(state, close, NULL);
}

static inline void print_ast_with_delim(
    FormatState* state,
    const char* open,
    const char* close,
    const AstNode* elem)
{
    format(state, open, NULL);
    print_ast(state, elem);
    format(state, close, NULL);
}

static inline void print_many_asts_inside_block(
    FormatState* state,
    const char* sep,
    const AstNode* elems,
    bool force_new_line)
{
    if (!elems)
        format(state, "{{}", NULL);
    else if (!force_new_line && !elems->next)
        print_ast_with_delim(state, "{{ ", " }", elems);
    else
        print_many_asts_with_delim(state, "{{{>}\n", sep, "{<}\n}", elems);
}

static inline void print_with_parens(FormatState* state, const AstNode* ast_node) {
    if (is_tuple(ast_node->tag))
        print_ast(state, ast_node);
    else
        print_ast_with_delim(state, "(", ")", ast_node);
}

static inline void print_prim_type(FormatState* state, AstNodeTag tag) {
    print_keyword(state, get_prim_type_name(tag));
}

static inline void print_operand(FormatState* state, const AstNode* ast_node, int prec) {
    if (is_binary_expr(ast_node->tag) && get_binary_expr_precedence(ast_node->tag) > prec)
        print_ast_with_delim(state, "(", ")", ast_node);
    else
        print_ast(state, ast_node);
}

static inline void print_prefix_expr(FormatState* state, const AstNode* ast_node) {
    print_ast_with_delim(state, get_unary_expr_op(ast_node->tag), "", ast_node->unary_expr.operand);
}

static inline void print_postfix_expr(FormatState* state, const AstNode* ast_node) {
    print_ast_with_delim(state, "", get_unary_expr_op(ast_node->tag), ast_node->unary_expr.operand);
}

static inline void print_binary_or_assign_expr(FormatState* state, const AstNode* ast_node, const char* op) {
    int prec = get_binary_expr_precedence(ast_node->tag);
    print_operand(state, ast_node->binary_expr.left, prec);
    format(state, " {s} ", (FormatArg[]) { { .s = op } });
    print_operand(state, ast_node->binary_expr.right, prec);
}

static inline void print_binary_expr(FormatState* state, const AstNode* ast_node) {
    print_binary_or_assign_expr(state, ast_node, get_binary_expr_op(ast_node->tag));
}

static inline void print_assign_expr(FormatState* state, const AstNode* ast_node) {
    print_binary_or_assign_expr(state, ast_node, get_assign_expr_op(ast_node->tag));
}

static inline void print_decl_head(
    FormatState* state,
    const char* keyword,
    bool is_public,
    bool is_opaque,
    const char* name,
    const AstNode* type_params)
{
    if (is_public) {
        print_keyword(state, "pub");
        format(state, " ", NULL);
    }
    if (is_opaque) {
        print_keyword(state, "opaque");
        format(state, " ", NULL);
    }
    print_keyword(state, keyword);
    if (name)
        format(state, " {s}", (FormatArg[]) { { .s = name } });
    if (type_params)
        print_many_asts_with_delim(state, "[", ", ", "]", type_params);
}

void print_ast(FormatState* state, const AstNode* ast_node) {
    if (ast_node->attrs)
        print_many_asts_with_delim(state, "#[", ", ", "] ", ast_node->attrs);
    switch (ast_node->tag) {
        case AST_IMPLICIT_CAST:
            print_with_style(state, "/* cast */", comment_style);
            print_ast(state, ast_node->implicit_cast.expr);
            format(state, "{$}/* to {$}{t}{$} */{$}", (FormatArg[]) {
                { .style = comment_style },
                { .style = reset_style },
                { .t = ast_node->type },
                { .style = comment_style },
                { .style = reset_style }
            });
            break;
        case AST_BOOL_LITERAL:
            print_keyword(state, ast_node->bool_literal.val ? "true" : "false");
            break;
        case AST_CHAR_LITERAL:
            format(state, "{$}{u8}{$}", (FormatArg[]) {
                { .style = literal_style },
                { .u8 = ast_node->char_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_STR_LITERAL:
            format(state, "{$}\"{s}\"{$}", (FormatArg[]) {
                { .style = literal_style },
                { .s = ast_node->str_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_INT_LITERAL:
            format(state, "{$}{s}{u}{$}", (FormatArg[]) {
                { .style = literal_style },
                { .s = ast_node->int_literal.has_minus ? "-" : "" },
                { .u = ast_node->int_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_FLOAT_LITERAL:
            format(state, "{$}{f64}{$}", (FormatArg[]) {
                { .style = literal_style },
                { .f64 = ast_node->float_literal.val },
                { .style = reset_style }
            });
            break;
        case AST_KIND_ARROW:
            print_many_asts_with_delim(state, "(", ", ", ")", ast_node->arrow_kind.dom_kinds);
            print_ast_with_delim(state, " => ", "", ast_node->arrow_kind.codom_kind);
            break;
        case AST_KIND_STAR:
            format(state, "*", NULL);
            break;
#define f(name, ...) case AST_TYPE_##name:
        PRIM_TYPE_LIST(f)
#undef f
            print_prim_type(state, ast_node->tag);
            break;
        case AST_NORET_TYPE:
            format(state, "!", NULL);
            break;
        case AST_ATTR:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->attr.name } });
            if (ast_node->attr.val) {
                if (ast_node->attr.val->tag == AST_ATTR)
                    print_many_asts_with_delim(state, "(", ", ", ")", ast_node->attr.val);
                else
                    print_ast_with_delim(state, " = ", "", ast_node->attr.val);
            }
            break;
        case AST_PATH:
            print_many_asts(state, ".", ast_node->path.elems);
            break;
        case AST_PATH_ELEM:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->path_elem.name } });
            if (ast_node->path_elem.type_args)
                print_many_asts_with_delim(state, "[", ", ", "]", ast_node->path_elem.type_args);
            break;
        case AST_ERROR:
            print_with_style(state, "<error>", error_style);
            break;
#define f(name, ...) case AST_##name##_EXPR:
        AST_PREFIX_EXPR_LIST(f)
#undef f
            print_prefix_expr(state, ast_node);
            break;
#define f(name, ...) case AST_##name##_EXPR:
        AST_POSTFIX_EXPR_LIST(f)
#undef f
            print_postfix_expr(state, ast_node);
            break;
#define f(name, ...) case AST_##name##_EXPR:
        AST_BINARY_EXPR_LIST(f)
#undef f
            print_binary_expr(state, ast_node);
            break;
        case AST_ASSIGN_EXPR:
#define f(name, ...) case AST_##name##_ASSIGN_EXPR:
        AST_ASSIGN_EXPR_LIST(f)
#undef f
            print_assign_expr(state, ast_node);
            break;
        case AST_TYPE_PARAM:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->type_param.name } });
            if (ast_node->type_param.kind) {
                format(state, ": ", NULL);
                print_ast(state, ast_node->type_param.kind);
            }
            break;
        case AST_TYPE_DECL:
            print_decl_head(state, "type",
                ast_node->type_decl.is_public,
                ast_node->type_decl.is_opaque,
                ast_node->type_decl.name,
                ast_node->type_decl.type_params);
            if (ast_node->type_decl.aliased_type)
                print_ast_with_delim(state, " = ", ";", ast_node->type_decl.aliased_type);
            else
                format(state, ";", NULL);
            break;
        case AST_FIELD_DECL:
            format(state, "{s}: ", (FormatArg[]) { { .s = ast_node->field_decl.name } });
            print_ast(state, ast_node->field_decl.type);
            if (ast_node->field_decl.val)
                print_ast_with_delim(state, " = ", "", ast_node->field_decl.val);
            break;
        case AST_OPTION_DECL:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->option_decl.name } });
            if (ast_node->option_decl.param_type) {
                if (ast_node->option_decl.is_struct_like) {
                    format(state, " ", NULL);
                    print_many_asts_inside_block(state, ",\n", ast_node->option_decl.param_type, false);
                } else
                    print_with_parens(state, ast_node->option_decl.param_type);
            }
            break;
        case AST_FUN_DECL:
            print_decl_head(state, "fun",
                ast_node->fun_decl.is_public, false,
                ast_node->fun_decl.name,
                ast_node->fun_decl.type_params);
            print_with_parens(state, ast_node->fun_decl.param);
            if (ast_node->fun_decl.ret_type)
                print_ast_with_delim(state, " -> ", "", ast_node->fun_decl.ret_type);
            if (ast_node->fun_decl.used_sigs) {
                format(state, " ", NULL);
                print_keyword(state, "using");
                print_many_asts_with_delim(state, " ", ", ", "", ast_node->fun_decl.used_sigs);
            }
            if (ast_node->fun_decl.body) {
                format(state, " ", NULL);
                if (ast_node->fun_decl.body->tag != AST_BLOCK_EXPR)
                    print_ast_with_delim(state, "= ", ";", ast_node->fun_decl.body);
                else
                    print_ast(state, ast_node->fun_decl.body);
            } else
                format(state, ";", NULL);
            break;
        case AST_ENUM_DECL: {
            print_decl_head(state, "enum",
                ast_node->enum_decl.is_public,
                ast_node->enum_decl.is_opaque,
                ast_node->enum_decl.name,
                ast_node->enum_decl.type_params);
            format(state, " ", NULL);
            print_many_asts_inside_block(state, ",\n", ast_node->enum_decl.options, false);
            break;
        }
        case AST_STRUCT_DECL: {
            print_decl_head(state, "struct",
                ast_node->struct_decl.is_public,
                ast_node->struct_decl.is_opaque,
                ast_node->struct_decl.name,
                ast_node->struct_decl.type_params);
            if (ast_node->struct_decl.is_tuple_like) {
                if (ast_node->struct_decl.fields)
                    print_many_asts_with_delim(state, "(", ", ", ")", ast_node->struct_decl.fields);
                format(state, ";", NULL);
            } else {
                format(state, " ", NULL);
                print_many_asts_inside_block(state, ",\n", ast_node->struct_decl.fields, false);
            }
            break;
        }
        case AST_SIG_DECL: {
            print_decl_head(state, "sig",
                ast_node->sig_decl.is_public, false,
                ast_node->sig_decl.name,
                ast_node->sig_decl.type_params);
            format(state, " ", NULL);
            print_many_asts_inside_block(state, "\n", ast_node->sig_decl.members, true);
            break;
        }
        case AST_MOD_DECL: {
            if (!ast_node->mod_decl.name) {
                print_many_asts(state, "\n", ast_node->mod_decl.members);
                break;
            }
            print_decl_head(state, "mod",
                ast_node->mod_decl.is_public, false,
                ast_node->mod_decl.name,
                ast_node->mod_decl.type_params);
            if (ast_node->mod_decl.signature)
                print_ast_with_delim(state, " : ", "", ast_node->mod_decl.signature);
            if (ast_node->mod_decl.members) {
                format(state, " ", NULL);
                print_many_asts_inside_block(state, "\n", ast_node->mod_decl.members, true);
            } else if (ast_node->mod_decl.aliased_mod)
                print_ast_with_delim(state, " = ", ";", ast_node->mod_decl.aliased_mod);
            else
                format(state, ";", NULL);
            break;
        }
        case AST_USING_DECL:
            print_decl_head(state, "using", false, false, NULL, ast_node->using_decl.type_params);
            print_ast_with_delim(state, " ", ";", ast_node->using_decl.used_mod);
            break;
        case AST_CONST_DECL:
        case AST_VAR_DECL:
            if (ast_node->const_decl.is_public) {
                print_keyword(state, "pub");
                format(state, " ", NULL);
            }
            print_keyword(state, ast_node->tag == AST_CONST_DECL ? "const" : "var");
            print_ast_with_delim(state, " ", "", ast_node->const_decl.pattern);
            if (ast_node->const_decl.init)
                print_ast_with_delim(state, " = ", "", ast_node->const_decl.init);
            format(state, ";", NULL);
            break;
        case AST_VAL_DECL:
            print_keyword(state, "val");
            format(state, " {s} : ", (FormatArg[]) { { .s = ast_node->val_decl.name } });
            print_ast(state, ast_node->val_decl.type);
            format(state, ";", NULL);
            break;
        case AST_FIELD_PATTERN:
        case AST_FIELD_EXPR:
            format(state, "{s} = ", (FormatArg[]) { { .s = ast_node->field_pattern.name } });
            print_ast(state, ast_node->field_pattern.val);
            break;
        case AST_STRUCT_PATTERN:
        case AST_STRUCT_EXPR:
        case AST_UPDATE_EXPR: {
            print_ast_with_delim(state, "", ast_node->tag != AST_UPDATE_EXPR ? " " : ".", ast_node->struct_pattern.left);
            print_many_asts_inside_block(state, ",\n", ast_node->struct_pattern.fields, false);
            break;
        }
        case AST_TUPLE_TYPE:
        case AST_TUPLE_PATTERN:
        case AST_TUPLE_EXPR:
            print_many_asts_with_delim(state, "(", ", ", ")", ast_node->tuple_type.args);
            break;
        case AST_ARRAY_TYPE:
            print_ast_with_delim(state, "[", "]", ast_node->array_type.elem_type);
            break;
        case AST_FUN_TYPE:
            print_keyword(state, "fun");
            print_with_parens(state, ast_node->fun_type.dom_type);
            print_ast_with_delim(state, " -> ", "", ast_node->fun_type.codom_type);
            break;
        case AST_PTR_TYPE:
            format(state, "&", NULL);
            if (ast_node->ptr_type.is_const) {
                print_keyword(state, "const");
                format(state, " ", NULL);
            }
            print_ast(state, ast_node->ptr_type.pointed_type);
            break;
        case AST_WHERE_CLAUSE:
            format(state, "{s} = ", (FormatArg[]) { { .s = ast_node->where_clause.name } });
            print_ast(state, ast_node->where_clause.type);
            break;
        case AST_WHERE_TYPE:
            print_ast(state, ast_node->where_type.signature);
            format(state, " ", NULL);
            print_keyword(state, "where");
            format(state, " ", NULL);
            print_many_asts_inside_block(state, ", ", ast_node->where_type.clauses, false);
            break;
        case AST_ARRAY_PATTERN:
        case AST_ARRAY_EXPR:
            print_many_asts_with_delim(state, "[", ", ", "]", ast_node->array_expr.elems);
            break;
        case AST_TYPED_PATTERN:
        case AST_TYPED_EXPR:
            if (ast_node->typed_pattern.left->tag == AST_IDENT_PATTERN &&
                ast_node->typed_pattern.left->ident_pattern.name[0] == '_')
            {
                // Print anonymous function parameters the expected way.
                print_ast(state, ast_node->typed_pattern.type);
            } else {
                if (ast_node->typed_pattern.left->tag == ast_node->tag)
                    print_with_parens(state, ast_node->typed_pattern.left);
                else
                    print_ast(state, ast_node->typed_pattern.left);
                format(state, ": ", NULL);
                print_ast(state, ast_node->typed_pattern.type);
            }
            break;
        case AST_BLOCK_EXPR:
            if (!ast_node->block_expr.stmts)
                format(state, "{{}", NULL);
            else {
                format(state, "{{{>}\n", NULL);
                for (AstNode* stmt = ast_node->block_expr.stmts; stmt; stmt = stmt->next) {
                    print_ast(state, stmt);
                    if (stmt->next)
                        format(state, needs_semicolon(stmt->tag) ? ";\n" : "\n", NULL);
                }
                if (ast_node->block_expr.ends_with_semicolon)
                    format(state, ";", NULL);
                format(state, "{<}\n}", NULL);
            }
            break;
        case AST_IF_EXPR:
            print_keyword(state, "if");
            print_ast_with_delim(state, " ", " ", ast_node->if_expr.cond);
            print_ast(state, ast_node->if_expr.then_expr);
            if (ast_node->if_expr.else_expr) {
                format(state, " ", NULL);
                print_keyword(state, "else");
                print_ast_with_delim(state, " ", "", ast_node->if_expr.else_expr);
            }
            break;
        case AST_MATCH_CASE:
            print_ast_with_delim(state, "", " => ", ast_node->match_case.pattern);
            print_ast(state, ast_node->match_case.val);
            break;
        case AST_MATCH_EXPR:
            print_keyword(state, "match");
            print_ast_with_delim(state, " ", " ", ast_node->match_expr.arg);
            print_many_asts_inside_block(state, ",\n", ast_node->match_expr.cases, false);
            break;
        case AST_CALL_EXPR:
            if (ast_node->call_expr.callee->tag == AST_FUN_EXPR)
                print_with_parens(state, ast_node->call_expr.callee);
            else
                print_ast(state, ast_node->call_expr.callee);
            print_with_parens(state, ast_node->call_expr.arg);
            break;
        case AST_IDENT_PATTERN:
            format(state, "{s}", (FormatArg[]) { { .s = ast_node->ident_pattern.name } });
            break;
        case AST_CTOR_PATTERN:
            print_ast(state, ast_node->ctor_pattern.path);
            print_with_parens(state, ast_node->ctor_pattern.arg);
            break;
        case AST_FUN_EXPR:
            print_keyword(state, "fun");
            print_with_parens(state, ast_node->fun_expr.param);
            if (ast_node->fun_expr.ret_type)
                print_ast_with_delim(state, " -> ", "", ast_node->fun_expr.ret_type);
            print_ast_with_delim(state, " => ", "", ast_node->fun_expr.body);
            break;
        case AST_MEMBER_EXPR:
            print_ast(state, ast_node->member_expr.left);
            print_many_asts_with_delim(state, ".", ".", "", ast_node->member_expr.elems_or_index);
            break;
        case AST_FOR_LOOP:
            print_keyword(state, "for");
            print_ast_with_delim(state, " ", " ", ast_node->for_loop.pattern);
            print_keyword(state, "in");
            print_ast_with_delim(state, " ", " ", ast_node->for_loop.range);
            print_ast(state, ast_node->for_loop.body);
            break;
        case AST_WHILE_LOOP:
            print_keyword(state, "while");
            print_ast_with_delim(state, " ", " ", ast_node->while_loop.cond);
            print_ast(state, ast_node->while_loop.body);
            break;
        case AST_BREAK_EXPR:    print_keyword(state, "break"); break;
        case AST_CONTINUE_EXPR: print_keyword(state, "continue"); break;
        case AST_RETURN_EXPR:   print_keyword(state, "return"); break;
        default:
            assert(false && "invalid node tag");
            break;
    }
}

#ifndef NDEBUG // GCOV_EXCL_START
void dump_ast(const AstNode* ast_node) {
    FormatState state = new_format_state("    ", !is_color_supported(stdout));
    print_ast(&state, ast_node);
    write_format_state(&state, stdout);
    free_format_state(&state);
    printf("\n");
}
#endif // GCOV_EXCL_STOP

bool needs_semicolon(AstNodeTag tag) {
    switch (tag) {
        case AST_CONST_DECL:
        case AST_VAR_DECL:
        case AST_FUN_DECL:
        case AST_STRUCT_DECL:
        case AST_ENUM_DECL:
        case AST_TYPE_DECL:
        case AST_IF_EXPR:
        case AST_MATCH_EXPR:
        case AST_BLOCK_EXPR:
        case AST_WHILE_LOOP:
        case AST_FOR_LOOP:
            return false;
        default:
            return true;
    }
}

bool is_tuple(AstNodeTag tag) {
    return
        tag == AST_TUPLE_TYPE ||
        tag == AST_TUPLE_PATTERN ||
        tag == AST_TUPLE_EXPR;
}

bool is_binary_expr(AstNodeTag tag) {
    switch (tag) {
#define f(name, ...) case AST_##name##_EXPR:
    AST_BINARY_EXPR_LIST(f)
#undef f
            return true;
        default:
            return false;
    }
}

bool is_assign_expr(AstNodeTag tag) {
    switch (tag) {
#define f(name, ...) case AST_##name##_ASSIGN_EXPR:
    AST_ASSIGN_EXPR_LIST(f)
#undef f
        case AST_ASSIGN_EXPR:
            return true;
        default:
            return false;
    }
}

bool is_value_decl(AstNodeTag tag) {
    switch (tag) {
        case AST_FUN_DECL:
        case AST_CONST_DECL:
        case AST_VAR_DECL:
        case AST_MOD_DECL:
        case AST_VAL_DECL:
            return true;
        default:
            return false;
    }
}

bool is_assignable_expr(const AstNode* expr) {
    assert(expr->type && "expression must have been type-checked first");
    if (expr->tag == AST_DEREF_EXPR)
        return is_non_const_ptr_type(expr->unary_expr.operand->type);
    else if (expr->tag == AST_MEMBER_EXPR)
        return is_assignable_expr(expr->member_expr.left);
    else if (expr->tag == AST_PATH) {
        return
            expr->path.decl_site->tag == AST_IDENT_PATTERN &&
            !expr->path.decl_site->ident_pattern.is_const;
    }
    return false;
}

bool is_public_decl(const AstNode* decl) {
    switch (decl->tag) {
        case AST_CONST_DECL:  return decl->const_decl.is_public;
        case AST_VAR_DECL:    return decl->var_decl.is_public;
        case AST_FUN_DECL:    return decl->fun_decl.is_public;
        case AST_TYPE_DECL:   return decl->type_decl.is_public;
        case AST_STRUCT_DECL: return decl->struct_decl.is_public;
        case AST_ENUM_DECL:   return decl->enum_decl.is_public;
        case AST_SIG_DECL:    return decl->sig_decl.is_public;
        case AST_MOD_DECL:    return decl->mod_decl.is_public;
        default:
            return false;
    }
}

bool is_opaque_decl(const AstNode* decl) {
    switch (decl->tag) {
        case AST_TYPE_DECL:   return decl->type_decl.is_opaque;
        case AST_STRUCT_DECL: return decl->struct_decl.is_opaque;
        case AST_ENUM_DECL:   return decl->enum_decl.is_opaque;
        default:
            return false;
    }
}

size_t count_ast_nodes(const AstNode* node) {
    size_t len = 0;
    while (node)
        node = node->next, len++;
    return len;
}

const AstNode* get_last_ast_node(const AstNode* node) {
    const AstNode* prev = node;
    for (; node; prev = node, node = node->next);
    return prev;
}

const AstNode* get_parent_scope_with_tag(const AstNode* ast_node, AstNodeTag tag) {
    AstNode* parent_scope = ast_node->parent_scope;
    while (parent_scope && parent_scope->tag != tag)
        parent_scope = parent_scope->parent_scope;
    return parent_scope;
}

const AstNode* get_parent_mod_decl(const AstNode* ast_node) {
    return get_parent_scope_with_tag(ast_node, AST_MOD_DECL);
}

AstNodeTag assign_expr_to_binary_expr(AstNodeTag tag) {
    switch (tag) {
#define f(name, ...) case AST_##name##_ASSIGN_EXPR: return AST_##name##_EXPR;
        AST_ASSIGN_EXPR_LIST(f)
#undef f
        default:
            return tag;
    }
}

const char* get_prim_type_name(AstNodeTag tag) {
    switch (tag) {
#define f(name, str) case AST_TYPE_##name: return str;
        PRIM_TYPE_LIST(f)
#undef f
        default:
            assert(false && "invalid primitive type");
            return "";
    }
}

const char* get_binary_expr_op(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, tok, str, ...) case AST_##name##_EXPR: return str;
        AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid binary expression");
            return "";
    }
}

const char* get_unary_expr_op(AstNodeTag tag) {
    switch (tag) {
#define f(name, tok, str) case AST_##name##_EXPR: return str;
        AST_UNARY_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid unary expression");
            return "";
    }
}

const char* get_assign_expr_op(AstNodeTag tag) {
    switch (tag) {
        case AST_ASSIGN_EXPR: return "=";
#define f(name, prec, tok, str, ...) case AST_##name##_ASSIGN_EXPR: return str"=";
        AST_ASSIGN_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid assignment expression");
            return "";
    }
}

const char* get_binary_expr_fun_name(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, tok, str, op_name) case AST_##name##_EXPR: return op_name;
        AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            assert(false && "invalid binary expression");
            return NULL;
    }
}

const char* get_decl_keyword(AstNodeTag tag) {
    switch (tag) {
        case AST_FUN_DECL:    return "fun";
        case AST_TYPE_DECL:   return "type";
        case AST_STRUCT_DECL: return "struct";
        case AST_ENUM_DECL:   return "enum";
        case AST_MOD_DECL:    return "mod";
        case AST_SIG_DECL:    return "sig";
        case AST_VAR_DECL:    return "var";
        case AST_CONST_DECL:  return "const";
        case AST_VAL_DECL:    return "val";
        default:
            assert(false && "invalid declaration");
            return NULL;
    }
}

const char* get_decl_name(const AstNode* decl) {
    switch (decl->tag) {
        case AST_FUN_DECL:    return decl->fun_decl.name;
        case AST_TYPE_DECL:   return decl->type_decl.name;
        case AST_VAL_DECL:    return decl->val_decl.name;
        case AST_STRUCT_DECL: return decl->struct_decl.name;
        case AST_ENUM_DECL:   return decl->enum_decl.name;
        case AST_SIG_DECL:    return decl->sig_decl.name;
        case AST_MOD_DECL:    return decl->mod_decl.name;
        default:
            return NULL;
    }
}

int get_max_binary_expr_precedence() {
    int max = 0;
#define f(name, prec, ...) max = max < prec ? prec : max;
    AST_BINARY_EXPR_LIST(f)
#undef f
    return max;
}

int get_binary_expr_precedence(AstNodeTag tag) {
    switch (tag) {
#define f(name, prec, ...) case AST_##name##_EXPR: return prec;
    AST_BINARY_EXPR_LIST(f)
#undef f
        default:
            return INT_MAX;
    }
}
