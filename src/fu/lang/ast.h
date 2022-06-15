#ifndef FU_LANG_AST_H
#define FU_LANG_AST_H

#include "fu/core/format.h"
#include "fu/core/log.h"
#include "fu/lang/types.h"

#define AST_ARITH_EXPR_LIST(f) \
    f(ADD, 3, PLUS,    "+", "add") \
    f(SUB, 3, MINUS,   "-", "sub") \
    f(MUL, 2, STAR,    "*", "mul") \
    f(DIV, 2, SLASH,   "/", "div") \
    f(REM, 2, PERCENT, "%", "rem")

#define AST_BIT_EXPR_LIST(f) \
    f(AND, 7, AMP,  "&", "and") \
    f(OR,  9, PIPE, "|", "or") \
    f(XOR, 8, HAT,  "^", "xor")

#define AST_SHIFT_EXPR_LIST(f) \
    f(L_SHIFT, 4, DOUBLE_LESS, "<<", "lshift") \
    f(R_SHIFT, 4, DOUBLE_GREATER, ">>", "rshift")

#define AST_CMP_EXPR_LIST(f) \
    f(EQ, 6, DOUBLE_EQUAL,  "==", "eq") \
    f(NE, 6, BANG_EQUAL,    "!=", "neq") \
    f(GT, 5, GREATER,       ">", "gt") \
    f(LT, 5, LESS,          "<", "lt") \
    f(GE, 5, GREATER_EQUAL, ">=", "geq") \
    f(LE, 5, LESS_EQUAL,    "<=", "leq")

#define AST_LOGIC_EXPR_LIST(f) \
    f(LOGIC_AND, 10, DOUBLE_AMP,  "&&", NULL) \
    f(LOGIC_OR,  11, DOUBLE_PIPE, "||", NULL)

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
    f(ADDR_OF, AMP, "&") \
    f(DEREF, STAR, "*") \
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
    AST_TYPE_PARAM,
    AST_ATTR,
    AST_IMPLICIT_CAST,
    // Path
    AST_PATH_ELEM,
    AST_PATH,
    // Kinds
    AST_KIND_STAR,
    AST_KIND_ARROW,
    // Types
    AST_TUPLE_TYPE,
    AST_ARRAY_TYPE,
    AST_PTR_TYPE,
    AST_FUN_TYPE,
#define f(name, ...) AST_TYPE_##name,
    PRIM_TYPE_LIST(f)
#undef f
    AST_NORET_TYPE,
    AST_WHERE_TYPE,
    AST_WHERE_CLAUSE,
    // Literals
    AST_BOOL_LITERAL,
    AST_INT_LITERAL,
    AST_FLOAT_LITERAL,
    AST_CHAR_LITERAL,
    AST_STR_LITERAL,
    // Declarations
    AST_FUN_DECL,
    AST_CONST_DECL,
    AST_VAR_DECL,
    AST_TYPE_DECL,
    AST_FIELD_DECL,
    AST_OPTION_DECL,
    AST_STRUCT_DECL,
    AST_ENUM_DECL,
    AST_MOD_DECL,
    AST_SIG_DECL,
    AST_USING_DECL,
    // Expressions
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
    AST_UPDATE_EXPR,
    AST_TUPLE_EXPR,
    AST_CALL_EXPR,
    AST_TYPED_EXPR,
    AST_MATCH_CASE,
    AST_MATCH_EXPR,
    AST_ARRAY_EXPR,
    AST_MEMBER_EXPR,
    AST_BREAK_EXPR,
    AST_CONTINUE_EXPR,
    AST_RETURN_EXPR,
    // Loops
    AST_WHILE_LOOP,
    AST_FOR_LOOP,
    // Patterns
    AST_IDENT_PATTERN,
    AST_FIELD_PATTERN,
    AST_STRUCT_PATTERN,
    AST_CTOR_PATTERN,
    AST_TUPLE_PATTERN,
    AST_TYPED_PATTERN,
    AST_ARRAY_PATTERN
} AstNodeTag;

typedef struct AstNode AstNode;

struct AstNode {
    AstNodeTag tag;
    FileLoc file_loc;
    const Type* type;
    AstNode* next;
    AstNode* attrs;
    union {
        struct {
            AstNode* expr;
        } implicit_cast;
        struct {
            bool val;
        } bool_literal;
        struct {
            uintmax_t val;
            bool has_minus;
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
            const char* name;
            AstNode* val;
        } attr;
        struct {
            AstNode* dom_kinds;
            AstNode* codom_kind;
        } arrow_kind;
        struct {
            AstNode* args;
        } tuple_type, tuple_expr, tuple_pattern;
        struct {
            AstNode* elem_type;
        } array_type;
        struct {
            AstNode* dom_type;
            AstNode* codom_type;
        } fun_type;
        struct {
            bool is_const;
            AstNode* pointed_type;
        } ptr_type;
        struct {
            AstNode* path;
            AstNode* clauses;
        } where_type;
        struct {
            AstNode* path;
            AstNode* type;
        } where_clause;
        struct {
            AstNode* elems;
        } array_expr, array_pattern;
        struct {
            AstNode* left;
            AstNode* elems_or_index;
        } member_expr;
        struct {
            const char* name;
            AstNode* kind;
        } type_param;
        struct {
            const char* name;
            AstNode* type_args;
            size_t index;
            bool is_type;
        } path_elem;
        struct {
            AstNode* elems;
            AstNode* decl_site;
        } path;
        struct {
            const char* name;
            bool is_public;
            AstNode* param;
            AstNode* type_params;
            AstNode* ret_type;
            AstNode* body;
            AstNode* used_sigs;
        } fun_decl;
        struct {
            bool is_public;
            AstNode* pattern;
            AstNode* init;
        } const_decl, var_decl;
        struct {
            const char* name;
            size_t index;
            AstNode* type;
            AstNode* val;
        } field_decl;
        struct {
            const char* name;
            bool is_struct_like;
            AstNode* param_type;
        } option_decl;
        struct {
            const char* name;
            bool is_public : 1;
            bool is_opaque : 1;
            bool is_tuple_like : 1;
            AstNode* super_type;
            AstNode* type_params;
            AstNode* fields;
        } struct_decl;
        struct {
            const char* name;
            bool is_public : 1;
            bool is_opaque : 1;
            AstNode* sub_type;
            AstNode* type_params;
            AstNode* options;
        } enum_decl;
        struct {
            const char* name;
            bool is_public;
            AstNode* type_params;
            AstNode* members;
        } sig_decl;
        struct {
            const char* name;
            bool is_public;
            AstNode* type_params;
            AstNode* signature;
            AstNode* aliased_mod;
            AstNode* members;
        } mod_decl;
        struct {
            const char* name;
            bool is_public : 1;
            bool is_opaque : 1;
            AstNode* type_params;
            AstNode* aliased_type;
        } type_decl;
        struct {
            AstNode* type_params;
            AstNode* used_mod;
        } using_decl;
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
            AstNode* then_expr;
            AstNode* else_expr;
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
            size_t index;
            AstNode* val;
        } field_expr, field_pattern;
        struct {
            AstNode* left;
            AstNode* fields;
        } struct_expr, update_expr, struct_pattern;
        struct {
            AstNode* left;
            AstNode* type;
        } typed_expr, typed_pattern;
        struct {
            AstNode* loop;
        } break_expr, continue_expr;
        struct {
            AstNode* fun;
        } return_expr;
        struct {
            AstNode* path;
            AstNode* arg;
        } ctor_pattern;
        struct {
            const char* name;
            bool is_const;
        } ident_pattern;
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

#ifndef NDEBUG
void dump_ast(const AstNode*);
#endif

bool needs_semicolon(AstNodeTag);
bool is_tuple(AstNodeTag);
bool is_binary_expr(AstNodeTag);
bool is_assign_expr(AstNodeTag);
bool is_assignable_expr(const AstNode*);
bool is_top_level_mod_decl(const AstNode*);

size_t count_ast_nodes(const AstNode*);
const AstNode* get_last_ast_node(const AstNode*);

AstNodeTag assign_expr_to_binary_expr(AstNodeTag);
const char* get_prim_type_name(AstNodeTag);
const char* get_unary_expr_op(AstNodeTag);
const char* get_binary_expr_op(AstNodeTag);
const char* get_assign_expr_op(AstNodeTag);
const char* get_binary_expr_fun_name(AstNodeTag);
const char* get_decl_keyword(AstNodeTag);
const char* get_decl_name(const AstNode*);

int get_max_binary_expr_precedence();
int get_binary_expr_precedence(AstNodeTag);

#endif
