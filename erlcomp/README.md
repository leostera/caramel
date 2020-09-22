# Erlang Compiler Backend for OCaml

This compiler backend is invoked by default on `.ml` and `.mli` inputs to
Caramel, and will output Erlang modules as `.erl` files.

Its focus is idiomatic Erlang output, althought it may not be very pretty at
the moment, with enough type information to make it easy to interop with
existing Erlang code.

## Rationale: why would you want this?

OCaml is a much safer language than Erlang, capable of preventing runtime errors 
by means of strict type checks. This means the language is also a lot more restricted.

There's a subset of OCaml that compiles just fine to the functional parts of
Erlang, making it an ideal language for defining the behavior of individual
type-safe actors.

Type-safety concurrency can also be provided within some constraints.

## Mapping: how does the generation work?

This is a Typedtree mapping, which means we hook into the OCaml compiler right
after all of the type checks have been done.

So Caramel will never output Erlang code that is not type-safe OCaml.

The compiler will strive for zero-cost abstractions, so using OCaml idioms
should have zero penalty on the final Erlang programs.

### Modules

Each OCaml module (not necessarily file) corresponds to a single
Erlang module. This means that instance of module Functors will get
monomorphized to single Erlang modules.

This avoids the problem of having a runtime representation of OCaml functors
backed into Erlang, so there's no overhead from it.

#### Module Signatures

The signature of a module, will regulate what functions are exported from that
module. If something does not appear on the signature, it will be kept
unexported.

#### Module Values

*Functions* at the module level will be translated to function declarations.
So that

```ocaml
let f x = x + 1
```

Becomes:

```erlang
f(X) -> X + 1.
```

*Values* that are not functions will not be supported at the module level, as
there are no similar construct that can preserve the semantics.

As an example, take:

```ocaml
let x = 
  print_string "hello";
  2

let f () = x + x + 1
```

`x` will have type `int` and value `2`, but we will only see `"hello"` printed
out once, when the module is first opened.

The translation of this to an arity 0 function will case every usage of `x` to
be turned into an application of `x()`, which would print `"hello"` several
times, this breaking the semantic equivalence.

```erlang
x() ->
  io:format("~p", ["hello"]),
  2.

f() -> x() + x() + 1.
```

### Types

The OCaml type system is fairly sophisticated, containing:

* aliases
* tuples
* records (product types)
* variants (union types)
* open variants (open union types)
* polymorphic variants (subtypable union types)
* GADTs (a form of variant where the specific output type is determined by the constructor used)
* objects (a form of row polymorphic records)
* abstract types

This mapping does not currently support:

* open variants
* GADTs
* objects

This mapping produces Dialyzer type specifications from OCaml types roughly following:

```erlang
% alias
% type t = int
-spec t() :: integer().

% tuples
% type t = (int, int)
-spec t() :: {integer(), integer()}.

% records
% type t = { hello: string };
-spec t() :: #{ hello => binary() }.

% variants
% type 'a t = | None | Some of 'a
-spec t(A) :: | none | {some, A}.

% polymorphic variants
% type t = [ | `Tag | `Tag of int ]
-spec t() :: | tag | {tag, integer()}.

% abstract types
% type t
-opaque t() :: any()
```

### Values

OCaml primitive values are mapped like this:

* `int` becomes `integer()`
* `float` becomes `float()`
* `bool` becomes `boolean()`
* `char` becomes `char()`
* `string` becomes `binary()`

Other OCaml values are mapped like this:

* tuples into tuples, so `(1, 2)` becomes `{1, 2}`
* records into maps, so `{ hello="world"; }` becomes `#{ hello => <<"world">> }`
* variants into atoms or tagged tuples, so that: `None` becomes `none`, and
  `Some 1` becomes `{some, 1}`
* polymorphic variants into atoms or tagged tuples, so that `\`Tag` becomes
  `tag` and `\`Tag 1` becomes `{tag, 1}`
