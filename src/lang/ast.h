#ifndef FU_LANG_AST_H
#define FU_LANG_AST_H

#include "lang/type.h"
#include "core/log.h"

#include <stdint.h>
#include <stddef.h>

#define AST_ARITH_OP_LIST(f) \
    f(ADD, "+", 4) \
    f(SUB, "-", 4) \
    f(MUL, "*", 3) \
    f(DIV, "/", 3) \
    f(REM, "%", 3)

#define AST_BIT_OP_LIST(f) \
    f(AND, "&", 8) \
    f(XOR, "^", 9) \
    f(OR,  "|", 10)

#define AST_LOGIC_OP_LIST(f) \
    f(LOGIC_AND, "&&", 11) \
    f(LOGIC_OR,  "||", 12)

#define AST_CMP_OP_LIST(f) \
    f(CMP_EQ, "==", 7) \
    f(CMP_NE, "!=", 7) \
    f(CMP_LE, "<=", 6) \
    f(CMP_LT, "<", 6) \
    f(CMP_GE, ">=", 6) \
    f(CMP_GT, ">", 6) \

#define AST_PREFIX_OP_LIST(f) \
    f(NOT, "!", 2) \
    f(NEG, "-", 2) \
    f(INC, "++", 2) \
    f(DEC, "--", 2)

#define AST_POSTFIX_OP_LIST(f) \
    f(INC, "++", 1) \
    f(DEC, "--", 1)

struct ast {
    enum ast_tag {
        AST_ERROR,
        AST_PRIM_TYPE,
        AST_TUPLE_TYPE,
        AST_LITERAL,
        AST_IDENT,
        AST_STRUCT_DECL,
        AST_FUN_DECL,
        AST_FIELD_DECL,
        AST_ENUM_DECL,
        AST_OPTION_DECL,
        AST_VAR_DECL,
        AST_CONST_DECL,
        AST_FUN_EXPR,
        AST_STRUCT_EXPR,
        AST_FIELD_EXPR,
        AST_BLOCK_EXPR,
        AST_CALL_EXPR,
        AST_IF_EXPR,
        AST_TUPLE_EXPR,
        AST_ARRAY_EXPR,
#define f(x, ...) AST_BINARY_EXPR_##x,
        AST_ARITH_OP_LIST(f)
        AST_BIT_OP_LIST(f)
        AST_CMP_OP_LIST(f)
        AST_LOGIC_OP_LIST(f)
#undef f
        AST_BINARY_EXPR_ASSIGN,
#define f(x, ...) AST_BINARY_EXPR_ASSIGN_##x,
        AST_ARITH_OP_LIST(f)
#undef f
#define f(x, ...) AST_UNARY_EXPR_PREFIX_##x,
        AST_PREFIX_OP_LIST(f)
#undef f
#define f(x, ...) AST_UNARY_EXPR_POSTFIX_##x,
        AST_POSTFIX_OP_LIST(f)
#undef f
        AST_MATCH_EXPR,
        AST_MATCH_CASE,
        AST_PATH,
        AST_PATH_ELEM,
        AST_WHILE_LOOP,
        AST_FOR_LOOP,
        AST_TYPE_ANNOT,
        AST_TUPLE_PATTERN,
        AST_STRUCT_PATTERN,
        AST_FIELD_PATTERN,
        AST_ARRAY_PATTERN,
        AST_CALL_PATTERN,
    } tag;
    const struct type* type;
    struct file_loc loc;
    struct ast* next;
    struct ast* attr;
    union {
        struct {
            enum type_tag tag;
        } prim_type;
        struct literal {
            enum literal_tag {
                LITERAL_BOOL,
                LITERAL_INT,
                LITERAL_FLOAT,
                LITERAL_CHAR,
                LITERAL_STRING
            } tag;
            union {
                bool bool_val;
                uintmax_t int_val;
                double float_val;
                char char_val;
                const char* string_val;
            };
        } literal;
        struct {
            const char* name;
            struct ast* decl;
        } ident;
        struct {
            struct ast* left;
            struct ast* type;
        } type_annot;
        struct {
            struct ast* stmts;
            bool ends_with_semicolon;
        } block_expr;
        struct {
            struct ast* cond;
            struct ast* branch_true;
            struct ast* branch_false;
        } if_expr;
        struct {
            struct ast* cond;
            struct ast* body;
        } while_loop;
        struct {
            struct ast* pattern;
            struct ast* iter_expr;
            struct ast* body;
        } for_loop;
        struct {
            const char* name;
            struct ast* type_params;
            struct ast* filter;
            struct ast* ret_type;
            struct ast* param;
            struct ast* body;
        } fun_decl;
        struct {
            struct ast* param;
            struct ast* body;
        } fun_expr;
        struct {
            const char* name;
            struct ast* type_params;
            struct ast* fields;
        } struct_decl;
        struct {
            struct ast* path;
            struct ast* fields;
        } struct_expr, struct_pattern;
        struct {
            struct ast* elems;
            bool is_type;
        } path;
        struct {
            struct ast* ident;
            struct ast* type_args;
        } path_elem;
        struct {
            struct ast* callee;
            struct ast* arg;
        } call_expr, call_pattern;
        struct {
            struct ast* match_val;
            struct ast* cases;
        } match_expr;
        struct {
            struct ast* pattern;
            struct ast* case_val;
        } match_case;
        struct {
            const char* name; 
            struct ast* type;
        } field_decl;
        struct {
            const char* name; 
            struct ast* val;
        } field_expr;
        struct {
            const char* name;
            struct ast* type_params;
            struct ast* options;
        } enum_decl;
        struct {
            const char* name;
            struct ast* param;
        } option_decl;
        struct {
            struct ast* pattern;
            struct ast* init;
        } var_decl, const_decl;
        struct {
            struct ast* args;
        } tuple_type, tuple_expr, tuple_pattern;
        struct {
            struct ast* elems;
        } array_expr, array_pattern;
        struct {
            struct ast* arg;
        } unary_expr;
        struct {
            struct ast* left;
            struct ast* right;
        } binary_expr;
    };
};

bool is_trivial_pattern(const struct ast*);
size_t get_ast_list_length(const struct ast*);
struct ast* get_ast_list_elem(struct ast*, size_t);

#endif
