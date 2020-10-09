# Erlang Language Libraries 

Libraries to work with Erlang sources in Standard Erlang and Core Erlang
syntax.

## Standard Erlang

This library contains:

* a definition of an AST for Erlang,
* a parser that tries to follow the [Standard Erlang
  grammar](https://github.com/erlang/otp/blob/master/lib/stdlib/src/erl_parse.yrl),
  and
* 2 printers: a debugging S-expr printer, and a Standard Erlang printer

### Language Support

This list and its completeness is a work in progress.

#### Preprocessor Language

- [X] Macro definition
- [X] Macro application

#### Module Language

- [x] Module attributes (built-in and custom ones)
- [x] Type declarations
- [x] Function declarations

#### Type Language

- [x] Opaque types
- [x] Visible types
- [x] Union types
- [x] Aliases
- [x] Record types
- [x] Tuple types
- [x] Map types
- [x] Function types
- [x] Type constructors
- [x] Type variables
- [x] Function specs
- [X] Callback function specs

#### Pattern Language

- [x] Ignore pattern (`_`) -- AST, Printer
- [x] Constant matches (against literals)  -- AST, Printer
- [x] Tuple patterns -- AST, Printer
- [x] List patterns -- AST, Printer
  - [x] Empty list -- AST, Printer
  - [x] Head-tail patterns -- AST, Printer
  - [x] Exact list patterns -- AST, Printer
- [ ] Record patterns
- [x] Map patterns -- AST, Printer
- [x] Pattern binding -- AST, Printer

#### Expression Language

- [x] Constants:
  - [x] Integer -- AST, Printer
  - [x] Floats -- AST, Printer
  - [x] Binary Strings -- AST, Printer
  - [x] Strings -- AST, Printer
  - [x] IO Lists
  - [x] Characters -- AST, Printer
  - [x] Atoms -- AST, Printer
- [x] Let bindings -- AST, Printer
- [x] Passing Identifiers to other expressions -- AST, Printer
- [x] Function:
  - [x] Fun references -- AST, Printer
  - [x] Lambda application (`F()`) -- AST, Printer
  - [x] Unqualified application (`f()`) -- AST, Printer
  - [x] Qualified application (`m:f()`) -- AST, Printer
  - [x] Dynamic Qualified application (`M:F()`) -- AST, Printer
- [x] Map expressions
  - [x] New map -- AST, Printer
  - [x] Map updates
- [ ] List expressions:
  - [x] Empty list -- AST, Printer
  - [x] Lists with elements -- AST, Printer
  - [x] List consing (`[ H | T]`) -- AST, Printer
  - [ ] List comprehensions
- [x] Tuple expressions -- AST, Printer
- [ ] Record expressions
  - [ ] New record
  - [ ] Record update
- [X] Exception expressions:
  - [X] Try catch
  - [X] Catch
  - [X] Throw 
- [x] Case expressions -- AST, Printer
- [x] Messaging expressions
  - [x] Receive -- AST, Printer
  - [x] Receive .. After -- AST, Printer
  - [x] Send (although this is just an infix operator for `erlang:send/2`) -- AST, Printer
- [X] Clause Guards

#### Operators

- [X] *
- [X] /
- [X] div
- [X] rem
- [X] band
- [X] and
- [X] +
- [X] -
- [X] bor
- [X] bxor
- [X] bsl
- [X] bsr
- [X] or
- [X] xor
- [X] ++
- [X] --
- [X] ==
- [X] /=
- [X] =<
- [X] <
- [X] >=
- [X] >
- [X] =:=
- [X] =/=
- [X] <=
- [X] =>
- [X] :=
- [X] <<
- [X] >>
- [X] !

## Core Erlang

This libraries contain:

* a definition of an AST for Core Erlang,
* a parser that follows the [Core Erlang
  Spec](https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf)
  and,
* 2 printers: a debugging S-expr printer, and a Core Erlang printer

### Language Support

This list and its completeness is a work in progress. An item will be marked as
done when its supported at the 3 levels: Parsing, AST, and Printing.

#### Module Language

- [x] Module name
- [ ] Module attributes (built-in and custom ones)
- [ ] Type declarations
- [x] Function declarations

#### Type Language

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

#### Pattern Language

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

#### Expression Language

- [ ] Literals:
  - [x] Integer -- AST, Printer
  - [x] Floats -- AST
  - [x] Atoms -- AST, Printer
  - [x] Char -- AST
  - [x] Nil -- AST
  - [x] Cons -- AST
  - [x] Tuple -- AST
- [x] Let binding -- AST
- [x] Letrec binding -- AST
- [x] Sequencing -- AST
- [x] Variable -- AST
- [x] Case expression -- AST
- [x] Lambdas -- AST
- [x] Function application -- AST
- [x] Qualified function calls -- AST, Printer
- [x] Primitive Operations -- AST
- [x] Exception expressions:
  - [x] Try .. catch -- AST
  - [x] Catch -- AST
- [x] Receive expressions -- AST
