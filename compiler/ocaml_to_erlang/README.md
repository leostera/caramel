# Erlang Compiler Backend for OCaml

## Language Support

- [X] Better handling of externals / FFI
- [X] Example of type-safe process
- [ ] fully inferred message type in processes experiments
  - [X] passing a recv function for the unification to work from the inside out
  - [ ] ~~hack "receive" expression into the language~~ 
- [ ] Type Safe Send & Receive:
  - [X] ~~try universally quantified message types~~ -- nope, too permissive,
        got runtime crashes!
  - [ ] ~~try hiding the message type altogether in a GADT~~
  - [X] try fixing the type of the receiver pid's message instead -- needs a
        additional layering over process (contramap) to turn them into the
        current processes' acceptable message type
- [X] bind names from poly variants!!!
- [X] clean up tests to make it easier to see what's being changed where
- [ ] refactor fun refs to resolve as much data (M, F, Arity) at compile time

### Idioms

- [X] Empty tuple `()` is a common value in OCaml, this is translated to the
  atom `ok` in Erlang. This is not _entirely_ idiomatic, since some functions
  are generated that take for last argument the atom `ok`, but it is much nicer
  for the returned values. 

### Module System

The goal here is to support the OCaml module system, with its nesting, functors,
aliasing, and combination support, to provide a flexible and type-safe way of
structuring large amounts of code.

- [x] Modules
- [X] Modules that have no exports of any kind will not generate Erlang code
  and are thus zero-cost
- [x] Nested Modules are flattened out
- [x] Control Exports via Interfaces
- [x] Module Functions Declarations
- [ ] Functors

### Type Language

The goal here is to generate Dialyzer compatible type specs that can be used to
indicate the usage of the code that was generated. A decision neeeds to be made
as to whether we'll add a little overhead to make some of these typed
representations (specially the ones with phantom types), runtime safe too.

- [X] Opaque Types
- [x] Phantom Types
- [x] Variants Types
- [x] Polymorphic Variant types
- [x] Alias Types
- [x] Record Types
- [x] Function Types
- [X] Automatic Function Specs
- [x] Control Exports via Interfaces
- [X] Support qualified type constructor names

### Functions

- [ ] Explicitly distinguish between parameters that can be uncurried
      and curriable ones

### Expression Generation

Valid expressions in the Erlang language that are supported off the bad, or translated via FFI libraries or syntax extensions at the OCaml level.

- [ ] Literals:
  - [ ] Pid
  - [ ] Reference
  - [x] Integer
  - [x] Character
  - [x] String (OCaml strings turned into as binary string)
  - [x] Float
  - [x] Maps
  - [x] Lists
  - [x] Tuples
- [x] Let Bindings
- [x] Variables
  - [ ] Make sure all variable names translate to valid Erlang variable names
- [X] Lambdas
- [x] Function Reference
- [x] Function Calls
  - [x] Proper resolution of local nested modules
  - [ ] Proper calls for lambdas -- currently this prints `f(A, B)` for `f(A)(B)`
        since it has no way of knowing that `f` has arity 1.
- [x] Case Expressions
- [x] If Expressions (they get turned into case expressions)
- [ ] Receive Expression
- [ ] Guards

### Pattern Matching

These are all different possible pattern matchings that we can do in let bindings,
function headers, case branches, etc. Not everything is allowed on that side!

- [x] Ignore
- [x] Binding
- [x] Tuple
- [x] List
- [x] Map
- [x] Exact match
