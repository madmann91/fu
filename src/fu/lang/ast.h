#ifndef FU_LANG_AST_H
#define FU_LANG_AST_H

#include "fu/core/format.h"
#include "fu/core/log.h"

#define AST_ARITH_EXPR_LIST(f) \
    f(ADD, 3, "+") \
    f(SUB, 3, "-") \
    f(MUL, 2, "*") \
    f(DIV, 2, "/") \
    f(REM, 2, "%")

#define AST_BIT_EXPR_LIST(f) \
    f(AND, 7, "&") \
    f(OR, 9, "|") \
    f(XOR, 8, "^")

#define AST_SHIFT_EXPR_LIST(f) \
    f(L_SHIFT, 4, "<<") \
    f(R_SHIFT, 4, ">>")

#define AST_CMP_EXPR_LIST(f) \
    f(EQ, 6, "==") \
    f(NE, 6, "!=") \
    f(GT, 5, ">") \
    f(LT, 5, "<") \
    f(GE, 5, ">=") \
    f(LE, 5, "<=")

#define AST_LOGIC_EXPR_LIST(f) \
    f(LOGIC_AND, 10, "&&") \
    f(LOGIC_OR,  11, "||")

#define AST_BINARY_EXPR_LIST(f) \
    AST_ARITH_EXPR_LIST(f) \
    AST_BIT_EXPR_LIST(f) \
    AST_SHIFT_EXPR_LIST(f) \
    AST_CMP_EXPR_LIST(f) \
    AST_LOGIC_EXPR_LIST(f)

#define AST_PREFIX_EXPR_LIST(f) \
    f(PRE_DEC, "--") \
    f(PRE_INC, "++") \
    f(MINUS, "-") \
    f(PLUS, "+") \
    f(NOT, "!")

#define AST_POSTFIX_EXPR_LIST(f) \
    f(POST_DEC, "--") \
    f(POST_INC, "++")

#define AST_UNARY_EXPR_LIST(f) \
    AST_PREFIX_EXPR_LIST(f) \
    AST_POSTFIX_EXPR_LIST(f)

typedef enum {
    AST_ERROR,
    AST_PROGRAM,
    AST_PATH_ELEM,
    AST_PATH,
    AST_BOOL,
    AST_I8,
    AST_I16,
    AST_I32,
    AST_I64,
    AST_U8,
    AST_U16,
    AST_U32,
    AST_U64,
    AST_INT_LITERAL,
    AST_FLOAT_LITERAL,
    AST_CHAR_LITERAL,
    AST_STR_LITERAL,
    AST_TUPLE,
    AST_FUN_DECL,
    AST_CONST_DECL,
    AST_VAR_DECL,
    AST_ALIAS_DECL,
    AST_FIELD_DECL,
    AST_OPTION_DECL,
    AST_STRUCT_DECL,
    AST_ENUM_DECL,
#define f(name, ...) AST_##name##_EXPR,
    AST_BINARY_EXPR_LIST(f)
    AST_UNARY_EXPR_LIST(f)
#undef f
#define f(name, ...) AST_##name##_ASSIGN_EXPR,
    AST_ARITH_EXPR_LIST(f)
    AST_BIT_EXPR_LIST(f)
    AST_SHIFT_EXPR_LIST(f)
#undef f
    AST_BLOCK_EXPR,
    AST_CALL_EXPR,
    AST_FUN_EXPR,
    AST_IF_EXPR,
    AST_MATCH_CASE,
    AST_MATCH_EXPR,
    AST_FIELD_EXPR,
    AST_STRUCT_EXPR,
    AST_WHILE_LOOP,
    AST_FOR_LOOP
} AstNodeTag;

typedef struct AstNode AstNode;

struct AstNode {
    AstNodeTag tag;
    FileLoc file_loc;
    AstNode* next;
    union {
        struct {
            AstNode* decls;
        } program;
        struct {
            uintmax_t value;
        } int_literal;
        struct {
            double value;
        } float_literal;
        struct {
            char value;
        } char_literal;
        struct {
            const char* value;
        } str_literal;
        struct {
            AstNode* args;
        } tuple;
        struct {
            const char* name;
            AstNode* type_args;
            AstNode* target;
        } path_elem;
        struct {
            AstNode* elems;
        } path;
        struct {
            const char* name;
            AstNode* param;
            AstNode* type_params;
            AstNode* ret_type;
            AstNode* body;
        } fun_decl;
        struct {
            AstNode* pattern;
            AstNode* init;
        } const_decl, var_decl;
        struct {
            const char* name;
            AstNode* type;
        } field_decl, option_decl;
        struct {
            const char* name;
            AstNode* type_params;
            AstNode* fields;
        } struct_decl;
        struct {
            const char* name;
            AstNode* type_params;
            AstNode* options;
        } enum_decl;
        struct {
            const char* name;
            AstNode* type_params;
            AstNode* aliased_type;
        } type_alias;
        struct {
            AstNode* left;
            AstNode* right;
        } binary_expr;
        struct {
            AstNode* operand;
        } unary_expr;
        struct {
            AstNode* stmts;
            bool ends_with_semicolon;
        } block_expr;
        struct {
            AstNode* callee;
            AstNode* arg;
        } call_expr;
        struct {
            AstNode* param;
            AstNode* ret_type;
            AstNode* body;
        } fun_expr;
        struct {
            AstNode* cond;
            AstNode* if_true;
            AstNode* if_false;
        } if_expr;
        struct {
            AstNode* pattern;
            AstNode* value;
        } match_case;
        struct {
            AstNode* arg;
            AstNode* cases;
        } match_expr;
        struct {
            const char* name;
            AstNode* value;
            size_t index;
        } field_expr;
        struct {
            AstNode* path;
            AstNode* fields;
        } struct_expr;
        struct {
            AstNode* cond;
            AstNode* body;
        } while_loop;
        struct {
            AstNode* pattern;
            AstNode* range;
            AstNode* body;
        } for_loop;
    };
};

void print_ast(FormatState*, const AstNode*);
void dump_ast(const AstNode*);

#endif
