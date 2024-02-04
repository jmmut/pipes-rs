

## Roadmap

- [x] basic numbers, arrays and operators
- [x] chars and strings
- [x] branching
- [x] iteration
- [x] functions
- [ ] semantic types (`:t` meaning "the previous expression is of type t")
- [/] file imports, in a way that is compatible with LSP
- [ ] code location (for better error messages)
- [x] assignment to identifiers



## Syntax
In this section, unchecked checkboxes are planned but unimplemented features.

You can use these atoms:

- [x] 64-bit signed integer numbers (e.g. `4`, `'a'` (syntax sugar for ASCII code 97))
  - [ ] unary negation (e.g. `-5`)
- [x] Functions (e.g. `function(x) { x }` (which is the anonymous identity function))
- [x] Arrays (e.g. `[1 2 3]`, allocated in the heap)
- [x] Identifiers (e.g. `my_namespaced/variable2`)
- [x] Strings (e.g. `"hello world"` (syntax sugar for an array of utf-8 ints))

With these operations:

- [x] Binary operators `+`, `-`, `|*`, `|/`, `%`, `=?` (is_equal),`<`,`<=`,`>`,`>=`, `#` (array access) (e.g. `2 + 3`, `[5 6 7] #0`)
- [x] Explicit grouping (e.g. `6 -{3 -2}` evaluates to 5)
- [x] Statement separator (e.g. `2 ;3` (which evaluates to 3))
- [x] Function call (e.g. `arg |func` (the usual syntax would be `func(arg)` but this is forbidden)
- [x] Loops
  - `loop_or`: iterates list, maybe breaks, returns some value. E.g.:
    - `[1 2 3] |loop_or(x) {x +42} {1000}` returns 43
    - `[1 2 3] |loop_or(x) {x +42;} {1000}` returns 1000
  - `times`: loops indexes, returns count (e.g. `3 |times(i) {i |to_str |print}` prints `0\n1\n2\n` and returns 3)
  - `times_or`: loops indexes, maybe breaks, returns some value. E.g.:
    - `3 |times_or(i) {x +42} {1000}` returns 42
    - `3 |times_or(i) {x +42;} {1000}` returns 1000
  - `map`: creates a new list from operating the input list (e.g. `[10 11] |map(e) {e +100}` returns a new list `[110 110]`)
  - `replace`: iterates a list, replaces each element (e.g. `[10 11] |replace(e) {e +100}` returns the modified list `[110 110]`)
- [x] Ifs/Branching 
  - `branch`: takes a value, if non-zero, runs the first chain; if zero, runs the second chain
    - `1 |branch {5} {6}` returns 5
    - `0 |branch {5} {6}` returns 6
  - `something`: if an optional value is present, run the first chain, but if it's nothing, run the second chain
    - `5 |something(x) {x} {0}` returns 5
    - `{} |something(x) {x} {0}` returns 0. "nothing" is an empty chain or a chain that ends in `;`
  - `inspect`: runs a chain, returning the initial value. Useful for debugging with prints
    - `5 |inspect(x) {"some side effect " ++{x |to_str} |print}` returns 5 and prints "some side effect 5"
- [ ] Type annotation (e.g. `2 :int64`)
- [ ] Struct field access (e.g. given `a :struct(x :int64)`, you can do `a.x` or `a .x`. The space is 
  significant, those are different things, TODO: explain below)
- [x] Comments, ignoring the rest of the line (e.g. `42 // the answer`)

### Nice to haves

- [ ] print source location in errors
- [ ] be able to print call stacks in pipes code
- [ ] const

### Simplified Grammar
Worth a special mention are Chains and TypedChildren.

Chains are delimited by braces (`{`, `}`) and contain an initial value and a list of operations applied to it. Examples are `{5}`, `{5 +3 -2}` and `{5 |print}`.

TypedChildren are delimited by parenthesis and contain a list of named types (name, `:`, type). One of the name or the type can be omitted. Examples with one child are `(x :i64)`, `(x)`, `(:i64)`; with 2 children `(x y)`; with 3 children (second name omitted, third type omitted) `(a :i64 :i64 c)`.

Chains and TypedChildren are composable in a few contexts. For example, a function is the literal `function`, a TypedChildren and a Chain, like `function(x) {x}`. A branch is `branch` and two Chains. A nested type is a name and a TypedChildren, like `tuple(x :i64, y:i64)`. 

```
Expression = Number | String | Array | Chain | Function | Branch | Type
Array = '[' Expression* ']'
Chain = '{' Expression Operation* '}' 
Operation = Operator Expression
Type = Identifier [TypedChildren]
TypedChildren = '(' TypedIdentifier+ ')'
TypedIdentifier = Identifier ':' Type | Identifier | ':' Type

Branch = 'branch' Chain Chain
Function = 'function' TypedChildren Chain
```
