# Ideas and things to do

## Macro System

I'd like to be able to derive a lot of code from types, and to allow developers
to build their own macros that are evaluated at compile-time easily.

For example, I'd like to include a `@derive` mechanism that allows me to 
create debug printers, accessors, setters, default values, orderings, and
equality functions, pretty much without thinking about them. Like this:

```ocaml
@derive(Debug, Access, Setters, Default, Eq)
type user = { name: string }

(* generates: *)
let dbg = user => "User { name: #{user.name} }"
let name = user => user.name
let set_name = (user, name) => { user with name = name }
let default = { name: "" }
let equal = (a, b) => String.equal(a.name, b.name) 
```

I'd also like to allow arbitary code to be run with this type as an input, and
have it create more code that will be inserted _after_ it:

```ocaml
@clipper( name = "my cli" )
type cli =
  | Echo { message: string }

(* this will generate: *)
let from_args : list<string> -> result<cli, parse_error> =
  args => {
    ...
  }
```

So the way to define a macro like `@clipper` above would be:

```ocaml

(def-macro-derive
  (debug derive-input)
  :pub
  ())

@macro(kind = "derive")
pub fn clipper(derive_input) {
  switch derive_input {
  }
}

```



## New syntax

The OCaml syntax feels old and clunky in some areas, so a rework here that
incorporates some aspects of Rust, ReScript and Elixir would be interesting to
have.

#### Less Keywords

Ideally, we'd have no begin/end, struct/end, sig/end, object/end, and just use
braces.

#### Atoms

Clojure does a nice job with its `:atom-name` syntax.

#### Functions in Multiple Clauses

```ocaml
pub fn $id($arg: $type, ...) -> $type {
} 

```

#### Spawn, Receive, and Send

These 3 constructs are fundamental for creating concurrent systems in the actor
model, so supporting them as syntax constructs would be very helpful:

```ocaml
let pid = spawn {

};

pid ! message;

receive {
} after x {
}
```


#### Annotations

For example, I'd love a syntax that makes it easy to annotate things, like
Elixir's `@annotation`:

```elixir
@doc "Hello world!"
def hello, do: 1
```

Or Rust's macro's on types, like `structopt`:

```rust
#[
  structopt(
    name = "My Cli",
    description = "a description"
  )
]
struct Cli {
  print_hello: bool
}
```

## Internals

- [ ] Who should know what `ext_calls` to build? `Ir_0` or `B_builder` ?

- [ ] Handle multiple ignored arguments: right now they get converted to `Param`
      but Core ends up pattern matching them against each other, which ofc isn't
      what we want.

## CLI UX

- [ ] `caramel compile` could also compile the .core to .beam directly?


## The Expression Language

### Function References

Currently passing in a function `f/2` as a reference _does not_ turn it into a
`fun f/2`. Ex:

```ocaml
let g x = x + 1
let f args = List.map g args
```

Does not turn `g` into a function reference always. But we'd expect this to be:

```erlang
g(X) -> X + 1.
f(Args) -> lists:map(fun g/1, Args).
```

## The Module Language

### Module Attributes

Parts of what's possible in Erlang/Elixir is to define module-level attributes,
including the `use` macros and the behavior-compliance declarations.

For example, `[@@caramel.behavior "name"]` as a top-level annotation could
provide a way to tag a module as using a specific behavior.

We could also allow a more general `[@@caramel.module {...}]` that lets you
define a record of properties that we'd use for this.

Or a more flexible `[@@caramel.attribute "name" "value"]`

Something more to consider is that some of these options require translating
more OCaml into Core Erlang, whereas some others may be easier to work with by
writing Core Erlang directly.

### Module References

Passing callback modules is common on the BEAM, as it is the way to leverage
dynamic dispatch to build dependency injection and other decouplings.

We could use the first-class module syntax to indicate a module reference:

```ocaml
module(My_mod)
```

Would check that the module exists and translate it to an atom that can be used
as a reference to the module: `'Caramel.My_mod'` 


### First Class Modules

Support for First Class Modules is interesting, but also, I think for most
things you don't actually need them. Functors are enough.


## Interop

### Externals

The `external` keyword should allow us to just dump raw code to be included,
but this would sadly have to be Core Erlang unless we find a way of translating
a small Erlang snippet into Core.

It would also be convenitent for `external` to support annotations like
`[@@caramel.module "name"]` to avoid doing string manipulation on the external
symbol string.


#### Elixir Structs

Elixir structs are more like maps with a special field than anything else.

#### Erlang Records

How do we do record interop? Records are essentially tagged tuples, just like
our representation of variant constructors. Almost so that this record and
variant are equivalent at the representation level:
      
```erlang
-record(a, {fst, snd}).
```

```ocaml
type ('a, 'b) a = | A of 'a * 'b
```

The question is whether we would want these to be equivalent or not, because
Records are usually packing rather polymorphic data, but its all super dyamic.
