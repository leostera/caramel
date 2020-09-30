# Core Erlang libraries for OCaml

This library contains:

* a definition of an AST for Core Erlang,
* a parser that follows the [Core Erlang
  Spec](https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf)
  and,
* 2 printers: a debugging S-expr printer, and a Core Erlang printer

## Language Support

This list and its completeness is a work in progress. An item will be marked as
done when its supported at the 3 levels: Parsing, AST, and Printing.

### Module Language

- [x] Module name
- [ ] Module attributes (built-in and custom ones)
- [ ] Type declarations
- [x] Function declarations

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
