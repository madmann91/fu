#ifndef FU_LANG_AST_H
#define FU_LANG_AST_H

#include "fu/core/format.h"
#include "fu/core/log.h"

#define AST_ARITH_EXPR_LIST(f) \
    f(ADD, 3, PLUS,    "+") \
    f(SUB, 3, MINUS,   "-") \
    f(MUL, 2, STAR,    "*") \
    f(DIV, 2, SLASH,   "/") \
    f(REM, 2, PERCENT, "%")

#define AST_BIT_EXPR_LIST(f) \
    f(AND, 7, AMP,  "&") \
    f(OR,  9, PIPE, "|") \
    f(XOR, 8, HAT,  "^")

#define AST_SHIFT_EXPR_LIST(f) \
    f(L_SHIFT, 4, DOUBLE_LESS, "<<") \
    f(R_SHIFT, 4, DOUBLE_GREATER, ">>")

#define AST_CMP_EXPR_LIST(f) \
    f(EQ, 6, DOUBLE_EQUAL,  "==") \
    f(NE, 6, BANG_EQUAL,    "!=") \
    f(GT, 5, GREATER,       ">") \
    f(LT, 5, LESS,          "<") \
    f(GE, 5, GREATER_EQUAL, ">=") \
    f(LE, 5, LESS_EQUAL,    "<=")

#define AST_LOGIC_EXPR_LIST(f) \
    f(LOGIC_AND, 10, DOUBLE_AMP,  "&&") \
    f(LOGIC_OR,  11, DOUBLE_PIPE, "||")

#define AST_BINARY_EXPR_LIST(f) \
    AST_ARITH_EXPR_LIST(f) \
    AST_BIT_EXPR_LIST(f) \
    AST_SHIFT_EXPR_LIST(f) \
    AST_CMP_EXPR_LIST(f) \
    AST_LOGIC_EXPR_LIST(f)

#define AST_ASSIGN_EXPR_LIST(f) \
    AST_ARITH_EXPR_LIST(f) \
    AST_BIT_EXPR_LIST(f) \
    AST_SHIFT_EXPR_LIST(f)

#define AST_PREFIX_EXPR_LIST(f) \
    f(PRE_DEC, DOUBLE_MINUS, "--") \
    f(PRE_INC, DOUBLE_PLUS, "++") \
    f(MINUS, MINUS, "-") \
    f(PLUS, PLUS, "+") \
    f(NOT, BANG, "!")

#define AST_POSTFIX_EXPR_LIST(f) \
    f(POST_DEC, DOUBLE_MINUS, "--") \
    f(POST_INC, DOUBLE_PLUS, "++")

#define AST_UNARY_EXPR_LIST(f) \
    AST_PREFIX_EXPR_LIST(f) \
    AST_POSTFIX_EXPR_LIST(f)

typedef enum {
    AST_ERROR,
    AST_PROGRAM,
    AST_TYPE_PARAM,
    AST_PATH_ELEM,
    AST_PATH,
    AST_BOOL,
    AST_INT_8,
    AST_INT_16,
    AST_INT_32,
    AST_INT_64,
    AST_WORD_8,
    AST_WORD_16,
    AST_WORD_32,
    AST_WORD_64,
    AST_FLOAT_32,
    AST_FLOAT_64,
    AST_TUPLE_TYPE,
    AST_BOOL_LITERAL,
    AST_INT_LITERAL,
    AST_FLOAT_LITERAL,
    AST_CHAR_LITERAL,
    AST_STR_LITERAL,
    AST_FUN_DECL,
    AST_CONST_DECL,
    AST_VAR_DECL,
    AST_TYPE_DECL,
    AST_FIELD_DECL,
    AST_OPTION_DECL,
    AST_STRUCT_DECL,
    AST_ENUM_DECL,
#define f(name, ...) AST_##name##_EXPR,
    AST_BINARY_EXPR_LIST(f)
    AST_UNARY_EXPR_LIST(f)
#undef f
    AST_ASSIGN_EXPR,
#define f(name, ...) AST_##name##_ASSIGN_EXPR,
    AST_ASSIGN_EXPR_LIST(f)
#undef f
    AST_BLOCK_EXPR,
    AST_FUN_EXPR,
    AST_IF_EXPR,
    AST_FIELD_EXPR,
    AST_STRUCT_EXPR,
    AST_TUPLE_EXPR,
    AST_CALL_EXPR,
    AST_MATCH_CASE,
    AST_MATCH_EXPR,
    AST_WHILE_LOOP,
    AST_FOR_LOOP,
    AST_FIELD_PATTERN,
    AST_STRUCT_PATTERN,
    AST_CTOR_PATTERN,
    AST_TUPLE_PATTERN,
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
            bool val;
        } bool_literal;
        struct {
            uintmax_t val;
        } int_literal;
        struct {
            double val;
        } float_literal;
        struct {
            char val;
        } char_literal;
        struct {
            const char* val;
        } str_literal;
        struct {
            AstNode* args;
        } tuple_type, tuple_expr, tuple_pattern;
        struct {
            const char* name;
            AstNode* kind;
        } type_param;
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
        } type_decl;
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
            AstNode* val;
        } match_case;
        struct {
            AstNode* arg;
            AstNode* cases;
        } match_expr;
        struct {
            const char* name;
            AstNode* val;
            size_t index;
        } field_expr, field_pattern;
        struct {
            AstNode* path;
            AstNode* fields;
        } struct_expr, struct_pattern;
        struct {
            AstNode* path;
            AstNode* arg;
        } ctor_pattern;
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

bool needs_semicolon(const AstNode*);
int max_precedence();
int precedence(AstNodeTag);

#endif
