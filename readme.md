
Pipes is an experimental toy programming language about applying successive transformations to some data.
Think bash with static typing.

This repo contains an interpreter written in rust and a couple related tools, like a web+wasm playground.

## Roadmap

`[x]` means mostly done, `[/]` means partial support, `[ ]` means not done.

Basic language features:
- [x] basic numbers, arrays and operators
- [x] chars and strings
- [x] branching
- [x] iteration
- [x] functions
- [x] variables
- [x] structs definition and field access

Project organization
- [x] be able to split code in different files
- [x] ...and be able to import identifiers
- [x] ...with minimal project definition
- [ ] ...in a way that is compatible with LSP

Typechecking:
- [x] semantic types (`:t` meaning "the previous expression is of type t")
- [/] type inference (like C++ auto)

Metaprogramming:
- [ ] type parameters (aka generics or templates)
- [/] eval (execute strings that contain code)
- [ ] hygienic macros

Tooling
- [x] web playground (https://jmmut.itch.io/pipes)
- [x] check and run continuously (see pipes-check or the web playground)
- [/] print code location in error messages
- [/] debugger breakpoints
- [ ] Language Server Protocol


## Syntax

The general syntax is `(operator parameter+)*`. There's no operator
precedence. All is left-associative and you can use braces `{}` to group
expressions.

You can use these atoms:

- [x] 64-bit signed integer numbers (e.g. `4`, `'a'` (syntax sugar for ASCII code 97))
  - [ ] unary negation (e.g. `-5`)
- [x] Functions (e.g. `function(x) {x}` (which is the anonymous identity function))
  - Functions can declare return types too: `function(x)(:i64) {x}`
- [x] Arrays (e.g. `[1 2 3]`, allocated in the heap)
- [x] Identifiers (e.g. `my_namespaced/variable2`)
- [x] Strings (e.g. `"hello world"` (syntax sugar for an array of utf-8 ints))
- [x] Nothing (e.g. `{}` or `{;}`). This is like Rust's Option::None.

With these operations:

- [x] Arithmetic binary operators `+`, `-`, `|*`, `|/`, `%`
- [x] Comparison binary operators `==`, `!=`, `<`, `<=`, `>`, `>=`
- [x] Array access `#` (e.g. `[5 6 7] #0` returns 5)
- [x] Explicit grouping (e.g. `6 -{3 -2}` evaluates to 5)
- [x] Expression separator (e.g. `2 ;3` (which evaluates to 3))
- [x] Assignment (e.g. `0 =a` puts 0 in the variable `a`). You can not assign to existing variables
- [x] Overwrite (e.g. `0 =a ;5 =>a`). You can not overwrite non-existing variables
- [x] Function call (e.g. `arg |func` (the usual syntax would be `func(arg)` but this is forbidden)
- [x] Ifs/Branching
  - `branch`: takes a value, if non-zero, runs the first chain; if zero, runs the second chain
    - `1 |branch {5} {6}` returns 5
    - `0 |branch {5} {6}` returns 6
  - `something`: if an optional value is present, run the first chain, but if it's nothing, run the second chain
    - `5 |something(x) {x} {0}` returns 5
    - `{} |something(x) {x} {0}` returns 0. "nothing" is an empty chain or a chain that ends in `;`
  - `inspect`: runs a chain, returning the initial value. Useful for side-effects, e.g. debugging with prints
    - `5 |inspect(x) {"some side effect " ++{x |to_str} |print}` returns 5 and prints "some side effect 5"
- [x] Loops
  - `browse_or`: iterates list, breaks iteration if something is returned, otherwise returns some value. E.g.:
    - `[1 2 3] |browse_or(x) {|to_str |print;} {1000}` prints `1\n2\n3\n` and returns 1000
    - `[1 2 3] |browse_or(x) {|to_str |print ;x +50} {1000}` prints `1\n` and returns 51
  - `times`: loops indexes, returns count (e.g. `3 |times(i) {i |to_str |print}` prints `0\n1\n2\n` and returns 3)
  - `times_or`: loops indexes, maybe breaks, returns some value. E.g.:
    - `3 |times_or(i) {x +42} {1000}` returns 42
    - `3 |times_or(i) {x +42;} {1000}` returns 1000
  - `map`: creates a new list from operating the input list (e.g. `[23 24] |map(e) {e +100}` returns a new list `[123 124]`)
  - `replace`: iterates a list, replaces each element (e.g. `[23 24] |replace(e) {e +100}` returns the modified list `[123 124]`)
  - `filter`: creates a sublist (e.g. `[1 2 3 4] |filter(x) {x %2 ==0}` returns a new list `[2 4]`)
- [x] Intrinsic functions
  - `new_array`: takes a number and returns an array of that size (e.g. `4 |new_array`)
  - `size`: takes an array and returns the number of elements it has (e.g. `[1 2 3] |size`)
  - `read_lines`: takes 0 and returns a list of strings (e.g. `0 |read_lines :list(line :list(char :i64))`)
  - `print`: takes a string (list of chars) and prints to stdout (e.g. `"hello" |print`)
- [x] Type annotation (e.g. `2 :int64`)
- [x] Struct field access (e.g. given `a :tuple(x :int64)`, you can do `a .x`
- [x] Comments, ignoring the rest of the line (e.g. `42 // the answer`)

### Nice to haves

- [ ] be able to print call stacks in pipes code
- [ ] const

### Simplified Grammar

Worth a special mention are Chains and Types.

A Chain is delimited by braces (`{`, `}`) and contains a list of operations
applied to a value. Examples are `{5}`, `{;5 +3 -2}` and `{5 |print}`.

Types are delimited by parenthesis and contain a list of named types (name, `:`, type). One of the name or the type can be omitted. Examples with one child are `(x :i64)`, `(x)`, `(:i64)`; with 2 children `(x y)`; with 3 children (second name omitted, third type omitted) `(a :i64  :i64  c)`.

Chains and Types are composable in a few contexts. For example, a function is
the literal `function`, a Types and a Chain, like `function(x) {x}`. A branch is
`branch` and two Chains. A nested type is a name and a Types, like `tuple(x :i64  y :i64)`.

A simplified grammar of most of the language:

    Array = '[' Expression* ']'
    Chain = '{' Operation* '}'
    Types = '(' TypedIdentifier* ')'

    Expression = Number | String | Array | Chain | Function | Branch | Type
    Operation = Operator Expression+
    Operator = '|' | ';' | '+' | '==' | ...
    TypedIdentifier = Identifier ':' Type | Identifier | ':' Type
    Type = Identifier Types?

    Scope = Types Chain

    Branch = 'branch' Chain Chain
    Function = 'function' Scope
    Map = 'map' Scope
    BrowseOr = 'browse_or' Scope Chain

In a Scope, the first identifier of the Types is available as first value for
the Chain, so you can do `function(x) {+1}` and it will increment its argument,
or `[4 5 6] |map(x) {+10}` and create a new list `[14 15 16]`.

IMHO it's quite elegant how Arrays `[]` are for data, Chains `{}` are for code
executed at runtime, and Types `()` are for typechecking done at compile time
(or at least done before runtime, as technically there's no compile time in an
interpreter).

## Philosophy of the language

### Most programs apply transformations to some value

In my experience, most programming is about having some input, applying some
transformations, and returning some output.

Sure, some algorithms need to keep track of many variables, but ideally you want
to do your code modular so that each piece is a simplified algorithm that is
about modifying a datum, or a list of elements. This also makes tests
wonderfully simple.

I think this transformation-oriented-programming is the reason why Bash (well,
shells in general) are so handy for simple tasks. However, they quickly turn
into a mess when you need to do anything more complex than an if and a loop,
which is a shame. I think that happens because their main datatype is free text,
which is why...


### Pipes has static inferred types

Type checking will happen in every language, you can only decide if it will
happen at compile time or at run time. And I think the earlier you can catch
mistakes, the better.

Some people complain that static typing is worse at development speed and
experimentation, but I think that's not necessarily true. In this language you
don't need to specify types most of the time, but they will still be checked
before having to run the program.

At the moment this project only has an interpreter backend, but I could add a
compiler backend (as I did in a previous version of this project
https://bitbucket.org/jmmut/pipes/), and the program `[1 2 3] |function(x) {x+1}
=a` would fail typechecking before runtime, despite not mentioning any type,
neither for the function definition, nor for the variable `a`. Typechecking
fails because the function expects an i64, but an array was passed.

And as the project matures you can add types to make it more robust. A nice
little feature is that you can also add names (for documentation purposes) to
the returned types, and to the nested types:

    [1 2 3]
    |function(score :list(points :i64)(total_points :i64) {|sum}
    ==6

### I hate operator precedence tables

I've had to look up this table too many times, so I made a language where that
table doesn't exist: https://en.cppreference.com/w/cpp/language/operator_precedence

### The problem with unary operators (or why `-5` is not supported)

Unary operators don't play well with the core idea of the language. It's like an
operation with 0 parameters.

To express `-5 % 7` I could support `5 |- % 7` or `5 |negative % 7`, but it's
quite unreadable. Too close to Forth.

## Miscellaneous trivia

If you export the test coverage from RustRover into a .lcov file, you can generate a html report with `genhtml your_file.lcov`.
