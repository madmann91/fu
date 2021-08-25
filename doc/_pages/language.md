---
layout: default
title: Language Description
permalink: language.html
category: Introduction
---

Fu has a type system that supports modules, kinds, and polymorphism (HM-style).
This type system is designed as a compromise between expressivity and simplicity, so that
performing type inference is still possible.

# Lexical Conventions

File encoding must be UTF-8. The following UTF-8 special characters may be used to replace
the given sequences of characters:

- `⇒` for `=>`
- `→` for `->`
- `≠` for `!=`

# Kinds

Fu has the following kinds:

 - The `Type` kind is, as the name suggests, for types. It's the equivalent of Haskell's `*`.
 - The `Nat` kind is for compile-time constants. Such constants can be created using literals,
   or by using the `to_nat` built-in function which essentially reduces an integer expression
   so that it is known at compile-time.
 - Signatures are the types of modules. The module system is inspired by ML's module system,
   and signatures essentially list the contents of a module, allowing partial specification
   (only giving a subset of the members of the module, or omitting the contents of a type).
 - `Binary16`, `Binary32`, `Binary64`: Those are special kinds that are used to construct
   floating-point types.

# Primitive types

 - `Int <N>`: Signed integer with bitwidth `N`. Integers assume two's complement representation.
 - `Word <N>`: Unsigned integer with bitwidth `N`.
 - `Bool`: Boolean type.
 - `Float [Binary16 | Binary32 | Binary64]`: Floating-point number following the IEEE754 format with 16, 32, or 64 bits respectively.
 - `Mem`: The memory type. More on this in the syntactic sugar section.
 - Common type aliases:
   ```fu
   type Int8 = Int[16];
   type Int16 = Int[16];
   type Int32 = Int[32];
   type Int64 = Int[64];
   type Word8 = Word[8];
   type Word16 = Word[16];
   type Word32 = Word[32];
   type Word64 = Word[64];
   type Float16 = Float[Binary16];
   type Float32 = Float[Binary32];
   type Float64 = Float[Binary64];
   type Byte = Word8;
   type Unit = ();
   ```

# Complex Types

 - `&T`: Immutable pointer to `T`.
 - `&mut T`: Mutable pointer to `T`.
 - `[T]`: Unsized arrays with elements of type `T`.
 - `[T * N]`: Array with elements of type `T` and size `N`.
 - `(T1, T2, ...)`: Tuple type with elements of type `T1`, `T2`, ...
 - `fun (T1, T2, ...) -> U`: Function type with domain `(T1, T2, ...)` and codomain `U`.
 - `forall [T1: K1, T2: K2, ...] . ...`: A polymorphic type.

# Type Aliases

The syntax to declare an alias is:

```fu
type T[T1, T2, ...] = U;
```

Where `T` is the name of the alias for the type `U`, with type parameters `T1`, `T2`, ...

> Type parameter lists, as used in the `type` construct above, and in others that have
> not yet been presented, are of the form:
> ```fu
> [T1: K1, T2: K2, ...]
> ```
> All constructs that take type parameter lists can also omit them, in which case the construct
 becomes monomorphic.
> The `: K1` annotations can be omitted. When they are present, they must be a valid _kind_
> for the type. Kinds are either `Type`, `Nat`, or some signature. When the kind annotation is
> not present, the kind is assumed to be `Type`.

# Signatures and Modules

Signatures are the types of modules. They are introduced with the keyword `sig`:

```fu
sig Ordered {
    type Key;
    fun leq(Key, Key) -> Bool;
}
sig Sort[T: Ordered] {
    fun sort(&mut [T.Key]) -> Unit;
}
```

They essentially represent the types and values that are exposed by a module.
The module system is designed such that a given module may choose to expose only a subset of its contents.

For instance, consider the following module:
```fu
mod InsertionSort[T: Ordered] {
    fun sort(array: &mut [T.Key]) = {
        for i in [1..array.length] {
            var j = i, e = array[i];
            while j > 0 && array[j] > e {
                array[j] = array[j - 1];
                j = j - 1;
            }
        }
    }
}
```
This module will by default have the signature: `forall [T: Ordered] . sig { sort: fun (&mut [T.Key]) -> Unit }`.
This default signature matches the `Sort` signature above, so this module can be used wherever a `Sort` is expected.

However, if some parts of a module must be kept private (because they should not be exposed to the user), a module
can be assigned a custom signature, as long as that signature is compatible with the module.
This is illustrated in the following example:
```fu
sig HasName {
    val name : [Byte];
}
mod ComplexObject : HasName {
    val value = 42;
    val name = "ComplexObject";
}
```
There, the `value` field is hidden from the `ComplexObject` module. Users of that module can therefore only access the `name` field.

Signatures can also be written inline, without a name:
```fu
mod ComplexObject :
    sig { val name : [Byte] }
{
    val value = 42;
    val name = "ComplexObject";
}
```

# Structures and Enumerations

Structures and enumerations declarations are very similar to Rust:

```fu
struct S[T1, T2, ...] {
    elem1: U1,
    elem2: U2,
    ...
}
enum E[T1, T2, ...] {
    option1: U1,
    option2: U2,
    ...
}
```

To construct a value for an enumeration or a structure, the syntax is similar:

```fu
struct S[A, B] { a: A, b: B }
enum E[A, B] { a: A, b: B }
val s = S[Int8, Word16] { a = 1, b = 3 };
val e = E[Int8, Word16] { a = 1 };
```

It is illegal to specify more than one binding `x = z` inside the braces of an enumeration initializer.
The following code would for instance be rejected:

```fu
val e = E[Int8, Word16] { a = 1, b = 3 };
```

# Literals

 - `'x'`: The single-byte character literal equal to 'x' encoded as UTF-8: Types as `Byte`.
 - `true`, `false`: Boolean constants.
 - `"abcd"`: The string literal "abcd" (encoded in UTF-8, this is just an array of bytes). This array _is not_ NULL-terminated. Types as `[Byte]`.
 - `"abcd\0"`: Same literal as above, but NULL-terminated.
 - `1`, `1.0`, `1.0`, `-1.0e-7`, `+1`, `0x1F.FBp+10`: Floating-point or integer literals.
   Integer literals type as an integer of floating-point type, depending on the context.
   Floating-point literals type as floating-point types, depending on the context.
 - `(1, 'a', 3.0)`: Tuple made of 3 literals.
 - `[1, 2, 3]`: An array made of 3 integers.

# Functions

Functions can be declared with the following syntax:

```fu
fun foo[T1, T2, ...](x1: U1, x2: U2) = ...
```

Anonymous functions use the following syntax:

```fu
fun (x: Int32, y: Float32) => x + (y as Int32)
```

In order to help the type inference algorithm (for recursive functions, whose return type cannot be inferred),
it is possible to annotate the return types of both regular and anonymous functions:

```fu
fun foo[T1, T2, ...](x1: U1, x2: U2) -> Int32 = ...
fun (x: Int32, y: Float32) -> Int32 => ...
```

# Variables and Constants

Variables are declared with the keyword `var`, as in the example below:

```fu
var x = 1;
```

This declares a _mutable_ variable named `x`.
The initializer for variables can be omitted, and the variable will be left in an uninitialized state.
While this behavior might be necessary for some very special cases, it is not recommended to leave variables uninitialized.
Therefore, the compiler will emit a warning for every variable left without an initializer.

Constants are declared with the keyword `val`:

```fu
val pi = 3.14159;
```

This declaration creates a constant named `pi` equal to `3.14159`.
The value of `pi` cannot be changed after that declaration.

Declarations for variables or constants can be chained with the same statement:

```fu
var x = 1, y = 2;
val pi = 3.14159, e = 2.71828;
```

Additionally, the `val` keyword accepts trivial patterns as the left-hand side of `=`,
in order to deconstruct a value and bind it directly to some identifiers:
```fu
val (x, y) = (1, 2);
```

# Patterns

Patterns are a way to deconstruct data types efficiently.
There are two types of patterns: _Trivial_ patterns, and _non-trivial_ ones.
A trivial pattern is a pattern that always matches: It does not depend on the value of the object being deconstructed.
A non-trivial pattern is a pattern that may not match the object being deconstructed in some cases.
In Rust terminology, those two terms correspond to "irrefutable" and "refutable" patterns, respectively.

Pattern matching is introduced with the `match` construct:
```fu
match v {
    0 => 1
    7 => 3
    _ => 2
}
```
The example above evaluates to `1` when `v` is `0`, or `3` when `v` is `7`, and otherwise `2`.

Patterns can be made of:

- Identifiers, in which case the pattern is trivial.
  Note that `_` is a special identifier that is not binding, and that it can thus be re-used within the same pattern.
- `(P1, P2, ...)`: Tuples containing other patterns. Such patterns are trivial if all of the patterns they contain are trivial.
- `[P1, P2, ...]`: Arrays of patterns whose size is a compile-time constant.
  Such patterns are trivial if all of the patterns they contain are trivial (they can only match fixed-size arrays).
- `1`, `'a'`: Literals that have an integer type. Those are non-trivial patterns.
- `true`, `false`: Boolean values. Those are also non-trivial.
- `S { a = 1, b = 2 }`: Structure values. Those are trivial if all of the patterns they contain are trivial.
- `E { a = 1 }`: Enumeration values.
  Those are trivial if all of the patterns they contain are trivial _and_ the enumeration has only one possible option.

# Partial Evaluation

Functions can be partially evaluated during compilation.
A partial evaluation _filter_ can be placed either at the call site of a function, or at its declaration site, or both.
If both are present, the resulting filter is the _logical and_ of both filters.
Filters are introduced with the following syntax:
```fu
filter(x == 5) fun foo(x: Int32) = x;
```

Effects can be present in the filter expression:
```fu
filter(some_effectful_function(x) < 100) fun bar(x: Int32) = ...;
```
In that case, they are executed at the call site of the function, when they are instantiated.
A function without side-effects (annotated with the `pure` attribute) cannot have an effectful filter expression.

# Syntactic Sugar

The constructs introduced so far are often syntactic sugar over the base IR.
Whenever printing or saving the IR to a file, the compiler will try to reconstruct this syntactic sugar when it is possible and makes sense.

## Declarations

The `val` syntax is actually syntactic sugar for a more primitive `let x = ... in ...` construct.
Take the following syntax, for instance:
```fu
val x = 1, y = x;
val z = y;
```
This example is in fact desugared into:
```fu
let x = 1 in
    let y = x in
        let z = y in ()
```

## Effects

Effects should be handled explicitly by the IR, but the user should not have to deal with them beyond annotating pure functions.
For that reason, the `{ ... }` and `var` syntax is actually syntactic sugar.
Expressions that load or store to a variable are annotated with `@` to indicate the memory object that they take to perform the operation.
Those expressions also return an additional memory object that corresponds to the state of memory after the operation.
For instance, assuming `mem0` is the current memory object, the code `{ var x = 1; x = 2; }` translates to:
```fu
{
    match ref[Int32](mem0) {
        (mem1, x) =>
            let mem2 = (x = 2)@mem1 in
            mem2
    }
}
```

Every function gets an implicit `mem` parameter that is added automatically by the compiler.
Pure functions cannot have side-effects, and are declared with the `no_implicit_mem` attribute:
```fu
attr(no_implicit_mem) fun f() = 1;
```
When pure functions are called from an effect region, they do change the current state of memory.
Thus, they can be moved around freely by the compiler, as long as dependencies are not invalidated.

## Control-flow

Basic-blocks are represented as functions whose return types are `!`: The "no-return" type.
With this, control flow is direct-style when calling functions, but continuation-passing-style when calling basic-blocks.
In order for this to work, the `control` operator introduces a return continuation inside the scope of a function.
For instance, take the following code:
```fu
attr(no_implicit_mem) fun f() -> Int32 = return(3)
```
This translates to:
```fu
attr(no_implicit_mem, no_implicit_control)
fun f() -> Int32 =
    control return: fun (Int32) -> ! in return(3)
```
The `no_implicit_control` attribute prevents the compiler from automatically inserting a `control` operator at the beginning of a function.

## A Simple Example

Consider the following code:
```fu
fun fact(n: Int32) = {
    var i = 2, p = 1;
    while i < n {
        p *= i;
        i++;
    }
    p
}
```
This example is actually desugared into:
```fu
attr(no_implicit_mem, no_implicit_control)
fun fact(mem0: Mem, n: Int32) =
    control return: fun(Mem, Int32) -> ! in {
        attr(no_implicit_mem, no_implicit_control)
        fun while_head(mem5: Mem) -> ! =>
            let mem6_i_ = i@mem5 in
            let mem6 = extract(mem6_i_, 0) in
            let i_ = extract(mem6_i_, 1) in
            select(i_ < n, while_body, while_break)(mem6)

        attr(no_implicit_mem, no_implicit_control)
        fun while_body(mem7: Mem) -> ! =>
            let mem8_p_ = p@mem7 in
            let mem8 = extract(mem8_p_, 0) in
            let p_ = extract(mem8_p_, 1) in
            let mem9_i_ = i@mem8 in
            let mem9 = extract(mem9_i_, 0) in
            let i_ = extract(mem9_i_, 1) in
            let mem10 = (p = p_ * i_)@mem9 in
            let mem11 = (i = i_ + 1)@mem10 in
            while_head(mem11)

        attr(no_implicit_mem, no_implicit_control)
        fun while_break(mem12: Mem) -> ! =>
            let mem13_p_ = p@mem12 in
            let mem13 = extract(mem13_p_, 0) in
            let p_ = extract(mem13_p_, 1) in
            return(mem13, p_)

        let mem1_i = ref[Int32](mem0) in
        let mem1 = extract(mem1_i, 0) in
        let i = extract(mem1_i, 1) in
        let mem2 = (i = 2)@mem1 in
        let mem3_p = ref[Int32](mem2) in
        let mem3 = extract(mem3_p, 0) in
        let p = extract(mem3_p, 1) in
        let mem4 = (p = 1)@mem3 in
        while_head(mem4)
    }
```
