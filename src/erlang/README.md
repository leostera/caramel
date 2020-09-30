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

- [x] Module attributes (built-in and custom ones)
- [x] Type declarations
- [x] Function declarations

### Type Language

- [x] Opaque types -- AST, Printer
- [x] Visible types -- AST, Printer
- [x] Union types -- AST, Printer
- [x] Aliases -- AST, Printer
- [x] Record types -- AST, Printer
- [x] Tuple types -- AST, Printer
- [x] Map types -- AST, Printer
- [x] Function types -- AST, Printer
- [x] Type constructors -- AST, Printer
- [x] Type variables -- AST, Printer
- [x] Function specs -- AST, Printer
- [ ] Callback function specs

### Pattern Language

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

### Expression Language

- [ ] Constants:
  - [x] Integer -- AST, Printer
  - [x] Floats -- AST, Printer
  - [x] Binary Strings -- AST, Printer
  - [ ] IO Lists
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
- [ ] Map expressions
  - [x] New map -- AST, Printer
  - [ ] Map updates
- [ ] List expressions:
  - [x] Empty list -- AST, Printer
  - [x] Lists with elements -- AST, Printer
  - [x] List consing (`[ H | T]`) -- AST, Printer
  - [ ] List comprehensions
- [x] Tuple expressions -- AST, Printer
- [ ] Record expressions
  - [ ] New record
  - [ ] Record update
- [ ] Exception expressions:
  - [ ] Try catch
  - [ ] Catch
  - [ ] Throw 
- [x] Case expressions -- AST, Printer
- [x] Messaging expressions
  - [x] Receive -- AST, Printer
  - [x] Receive .. After -- AST, Printer
  - [x] Send (although this is just an infix operator for `erlang:send/2`) -- AST, Printer
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
