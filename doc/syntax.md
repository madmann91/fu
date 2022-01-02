# Syntax

The syntax of Fu is divided in 5 categories: types, declarations, expressions, patterns, and statements.
The grammar is given in the following sections, assuming that the following terminals are the result
of lexing:

```bnf
IDENTIFIER
CHAR_LITERAL
STRING_LITERAL
INTEGER_LITERAL
FLOAT_LITERAL
```

## Paths

Paths are formed when accessing the fields of a compound object or type.

```bnf
PATH_ELEM ::= IDENTIFIER | IDENTIFIER , "[" , TYPE_ARGS , "]"
PATH ::= PATH_ELEM | PATH_ELEM , "." , PATH_ELEM
```

## Types

Types form the set of types that are accepted as part of a program.

```bnf
TYPE ::=
    PATH |
    PRIM_TYPE |
    TUPLE_TYPE |
    STRUCT_DECL |
    ENUM_DECL

PRIM_TYPE ::=
    "Bool" |
    "Int8" | "Int16" | "Int32" | "Int64" |
    "Word8" | "Word16" | "Word32" | "Word64" |
    "Float32" | "Float64"

TYPE_ARGS ::= TYPE | TYPE , "," , TYPE_ARGS
TUPLE_TYPE ::= "(" , TYPE_ARGS , ")"
```

## Declarations

Declarations can declare either values (functions, constants and variables) or types (aliases, structures, and enumerations).

```bnf
TYPE_PARAM ::= IDENTIFIER
TYPE_PARAMS ::= TYPE_PARAM | TYPE_PARAM , "," , TYPE_PARAMS
TYPE_PARAM_LIST ::= "[", TYPE_PARAMS, "]"

FIELD_NAMES ::= IDENTIFIER | IDENTIFIER , "," , FIELD_NAMES
STRUCT_FIELD ::= FIELD_NAMES , ":", TYPE
STRUCT_FIELDS ::= STRUCT_FIELD | STRUCT_FIELD , "," , STRUCT_FIELDS
STRUCT_DECL ::= "struct" , IDENTIFIER , TYPE_PARAMS_LIST?, "{" , STRUCT_FIELDS, "}"

ENUM_OPTION ::= IDENTIFIER | IDENTIFIER, TUPLE_TYPE
ENUM_OPTIONS ::= ENUM_OPTION | ENUM_OPTION, "," , ENUM_OPTIONS
ENUM_DECL ::= "enum" , IDENTIFIER , TYPE_PARAMS_LIST?, "{" , ENUM_OPTIONS, "}"

ALIAS_DECL ::= "type" , IDENTIFIER , TYPE_PARAM_LIST? , "=" , TYPE , ";"

RET_TYPE ::= "->", TYPE
FUN_DECL ::= "fun" , IDENTIFIER , TYPE_PARAM_LIST?, "(", PATTERN, ")" , RET_TYPE? , FUN_BODY
FUN_BODY ::= "=" , EXPR , ";" | BLOCK_EXPR

CONST_DECL ::= "const" , PATTERN , "=" , EXPR , ";"
VAR_DECL ::= "var" , PATTERN, ("=" , EXPR)? , ";"
```

## Expressions

Expressions appear at the right hand side of `=`, or as part of expression statements.
The precedence of binary operations mirrors that of C.
Note that tuple expressions with only one argument are treated as parenthesized expressions.

Additionally, to resolve the ambiguity between braces in opening blocks and structure expressions,
conditions in `if` or `match` expressions are given as the `CONDITION` non-terminal, which is exactly
like `EXPR` but excluding structure expressions.

```bnf
EXPR ::= ASSIGN_EXPR

MUL_OR_DIV_EXPR ::=
    PREFIX_EXPR |
    PREFIX_EXPR , "*" , PREFIX_EXPR |
    PREFIX_EXPR , "/" , PREFIX_EXPR |
    PREFIX_EXPR , "%" , PREFIX_EXPR

ADD_OR_SUB_EXPR ::=
    MUL_OR_DIV_EXPR |
    MUL_OR_DIV_EXPR , "+" , MUL_OR_DIV_EXPR |
    MUL_OR_DIV_EXPR , "-" , MUL_OR_DIV_EXPR

SHIFT_EXPR ::=
    ADD_OR_SUB_EXPR |
    ADD_OR_SUB_EXPR , "<<" , ADD_OR_SUB_EXPR |
    ADD_OR_SUB_EXPR , ">>" , ADD_OR_SUB_EXPR

COMPARE_EXPR ::=
    SHIFT_EXPR |
    SHIFT_EXPR , ">" , SHIFT_EXPR |
    SHIFT_EXPR , "<" , SHIFT_EXPR |
    SHIFT_EXPR , ">=" , SHIFT_EXPR |
    SHIFT_EXPR , ">=" , SHIFT_EXPR

EQUAL_EXPR ::=
    COMPARE_EXPR |
    COMPARE_EXPR , "==" , COMPARE_EXPR |
    COMPARE_EXPR , "!=" , COMPARE_EXPR

AND_EXPR ::= EQUAL_EXPR | EQUAL_EXPR , "&" , EQUAL_EXPR
XOR_EXPR ::= AND_EXPR | AND_EXPR , "^" , AND_EXPR
OR_EXPR ::= XOR_EXPR | XOR_EXPR , "|" , XOR_EXPR
LOGIC_AND_EXPR ::= OR_EXPR | OR_EXPR , "&&" , OR_EXPR
LOGIC_OR_EXPR ::= LOGIC_AND_EXPR | LOGIC_AND_EXPR , "||" , LOGIC_AND_EXPR

ASSIGN_EXPR ::=
    LOGIC_OR_EXPR |
    PREFIX_EXPR , "=" , ASSIGN_EXPR |
    PREFIX_EXPR , "+=" , ASSIGN_EXPR |
    PREFIX_EXPR , "-=" , ASSIGN_EXPR |
    PREFIX_EXPR , "*=" , ASSIGN_EXPR |
    PREFIX_EXPR , "/=" , ASSIGN_EXPR |
    PREFIX_EXPR , ">>=" , ASSIGN_EXPR |
    PREFIX_EXPR , "<<=" , ASSIGN_EXPR |
    PREFIX_EXPR , "&=" , ASSIGN_EXPR |
    PREFIX_EXPR , "|=" , ASSIGN_EXPR |
    PREFIX_EXPR , "^=" , ASSIGN_EXPR

PREFIX_EXPR ::=
    POSTFIX_EXPR |
    "--" , POSTFIX_EXPR |
    "++" , POSTFIX_EXPR |
    "-" , POSTFIX_EXPR |
    "+" , POSTFIX_EXPR |
    "!" , POSTFIX_EXPR

POSTFIX_EXPR ::=
    TYPED_EXPR |
    POSTFIX_EXPR , "--" |
    POSTFIX_EXPR , "++" |
    CALL_EXPR

CALL_EXPR ::= POSTFIX_EXPR , "(" , EXPR_LIST , ")"
PRIMARY_EXPR ::= TYPED_EXPR | UNTYPED_EXPR
TYPED_EXPR ::= UNTYPED_EXPR , ":" , TYPE

UNTYPED_EXPR ::=
    PATH | "break" | "continue" | "return" |
    TUPLE_EXPR |
    IF_EXPR |
    MATCH_EXPR |
    FUN_EXPR |
    BLOCK_EXPR |
    STRUCT_EXPR |
    CHAR_LITERAL |
    STRING_LITERAL |
    INTEGER_LITERAL |
    FLOAT_LITERAL

EXPR_LIST ::= EXPR | EXPR , "," , EXPR_LIST
TUPLE_EXPR ::= "(" , ")", | "(" , EXPR_LIST , ")"

IF_EXPR ::= "if" , CONDITION , BLOCK_EXPR , ("else" , BLOCK_EXPR)?

MATCH_CASE ::= PATTERN , "=>" , EXPR
MATCH_CASES ::= MATCH_CASE | MATCH_CASE , "," , MATCH_CASES
MATCH_EXPR ::= "match" , CONDITION , "{" , MATCH_CASES , "}"

FUN_EXPR ::= "fun" , TUPLE_PATTERN , RET_TYPE? , "=>" , EXPR

FIELD_EXPR ::= FIELD_NAMES , "=" , EXPR
FIELD_EXPRS ::= FIELD_EXPR | FIELD_EXPR , "," , FIELD_EXPRS
STRUCT_EXPR ::= PATH , "{" , FIELD_EXPRS? , "}"

STATEMENT_LIST ::= STATEMENT | STATEMENT , STATEMENT_LIST
BLOCK_EXPR ::= "{" , "}" | "{" , (STATEMENT_LIST)? , EXPR , "}"
```

## Patterns

Patterns appear in places where expressions are captured: variable declarations, function parameters,
or match expressions. Patterns that appear in function parameters and variable declarations must be
_binding_: They must not be _refutable_, or, in other words, should be able to capture the content of
any expression of the type of the pattern. In practice, this means that a _binding_ pattern cannot
contain an enumeration pattern, or a literal.

```bnf
PATTERN ::= TYPED_PATTERN | UNTYPED_PATTERN
TYPED_PATTERN ::= UNTYPED_PATTERN , ":" , TYPE

UNTYPED_PATTERN ::=
    IDENTIFIER |
    INT_LITERAL |
    CHAR_LITERAL |
    STRING_LITERAL |
    TUPLE_PATTERN |
    STRUCT_PATTERN |
    CTOR_PATTERN

FIELD_PATTERN ::= FIELD_NAMES , "=" , PATTERN
FIELD_PATTERNS ::= FIELD_PATTERN | FIELD_PATTERN , "," , FIELD_PATTERNS
STRUCT_PATTERN ::= PATH , "{" , FIELD_PATTERNS , "}"

PATTERN_LIST ::= PATTERN | PATTERN , "," , PATTERN_LIST
CTOR_PATTERN ::= PATH | PATH , "(" , PATTERN_LIST , ")"

TUPLE_PATTERN ::= "(", ")" | "(" , PATTERN_LIST , ")"
```

## Statements

Statements appear as part of of block expressions, and do not produce a value.
Note that the non-terminal `EXPR_WITHOUT_IF_OR_MATCH` is not given here for size reasons, but it is
simply the same as `EXPR`, excluding `IF_EXPR` and `MATCH_EXPR`: This allows parsing `if` and `match`
expressions as statements without an ending semicolon, even though they produce a value.

```bnf
STATEMENT ::=
    IF_EXPR |
    MATCH_EXPR |
    WHILE_LOOP |
    FOR_LOOP |
    EXPR_WITHOUT_IF_OR_MATCH , ";"

WHILE_LOOP ::= "while" , CONDITION , BLOCK_EXPR
FOR_LOOP ::= "for" , PATTERN , "in" , CALL_EXPR , BLOCK_EXPR
```
