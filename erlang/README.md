# Standard Erlang libraries for OCaml

This library contains:

* a definition of an AST for Erlang,
* a parser that tries to follow the [Standard Erlang
  grammar](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_parse.yrl),
  and
* 2 printers: a debugging S-expr printer, and a Standard Erlang printer

As a note, I'm actively deprioritizing Erlang Records in favor of Maps at all
levels.

## Language Support

This list and its completeness is a work in progress.

### Preprocessor Language

- [ ] Macro definition
- [ ] Macro application

### Module Language

- [ ] Module attributes (built-in and custom ones)
- [ ] Type declarations
- [ ] Function declarations

### Type Language

- [ ] Opaque types
- [ ] Visible types
- [ ] Union types
- [ ] Aliases
- [ ] Record types
- [ ] Tuple types
- [ ] Map types
- [ ] Function types
- [ ] Type constructors
- [ ] Type variables
- [ ] Function specs
- [ ] Callback function specs

### Pattern Language

- [ ] Ignore pattern (`_`)
- [ ] Constant matches (against literals) 
- [ ] Tuple patterns
- [ ] List patterns
  - [ ] Empty list
  - [ ] Head-tail patterns
  - [ ] Exact list patterns
- [ ] Record patterns
- [ ] Map patterns
- [ ] Pattern binding

### Expression Language

- [ ] Constants:
  - [ ] Integer
  - [ ] Floats
  - [ ] Binary Strings
  - [ ] IO Lists
  - [ ] Characters
  - [ ] Atoms
- [ ] Let bindings
- [ ] Passing Identifiers to other expressions
- [ ] Function:
  - [ ] Fun references
  - [ ] Lambda application (`F()`)
  - [ ] Unqualified application (`f()`)
  - [ ] Qualified application (`m:f()`)
  - [ ] Dynamic Qualified application (`M:F()`)
- [ ] Map expressions
  - [ ] New map
  - [ ] Map updates
- [ ] List expressions:
  - [ ] Empty list
  - [ ] Lists with elements
  - [ ] List consing (`[ H | T]`)
  - [ ] List comprehensions
- [ ] Tuple expressions
- [ ] Record expressions
  - [ ] New record
  - [ ] Record update
- [ ] Exception expressions:
  - [ ] Try catch
  - [ ] Catch
  - [ ] Throw 
- [ ] Case expressions
- [ ] Messaging expressions
  - [ ] Receive
  - [ ] Receive .. After
  - [ ] Send (although this is just an infix operator for `erlang:send/2`)
- [ ] Clause Guards

### Operators

- [ ] *
- [ ] /
- [ ] div
- [ ] rem
- [ ] band
- [ ] and
- [ ] +
- [ ] -
- [ ] bor
- [ ] bxor
- [ ] bsl
- [ ] bsr
- [ ] or
- [ ] xor
- [ ] ++
- [ ] --
- [ ] ==
- [ ] /=
- [ ] =<
- [ ] <
- [ ] >=
- [ ] >
- [ ] =:=
- [ ] =/=
- [ ] <=
- [ ] =>
- [ ] :=
- [ ] <<
- [ ] >>
- [ ] !
