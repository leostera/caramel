# Calling Erlang code

One of the goals of Caramel is to make it easy and free to call Erlang code in
a type-safe way.

To do this, we have a special language feature inherited from OCaml to create
**external bindings**, or simply **bindings**, using the `external` keyword.

We can define a binding with this pattern:

```ocaml
external <name> : <type-signature> = "<foreign name>"
```

If the `"<foreign name>"` string is empty, Caramel will fallback to using `"<name>"`.

When Caramel compiles your code, it will make sure that any calls to
`Module.<name>` end up as calls to `Module:<foreign name>`.

> **WARNING**: This is a **safety escape hatch** and Caramel **DOES NOT**
> guarantee that foreign code will be safe, or will not crash your program.

There are 4 possible combinations of external bindings:

* Bindings to functions with the same name
* Bindings to functions with differet names
* Bindings for functions with many input types
* Bindings for functions with many return types


### Bindings to functions with the same name

Let's look at an example and bind the `erlang:is_pid/1` function.

```ocaml
(* file: Erlang.ml *)

external is_pid : 'a -> bool = ""
(* We don't care what the input type is, so we can use a type variable like
 * `'a` but we know that we will always return a boolean.
 * 
 * We leave the foreign name as empty, because the actual function is called
 * exactly the same: is_pid
 *)
```

Now from our Caramel code we can call it as:

```ocaml
Erlang.is_pid 1 (* <-- this will be true or false *)
```

### Bindings to functions with different names

Some bindings however, have other foreign names, or have foreign names that look 
or feel unidiomatic for Caramel. For those we can replace that string and still
be able to call them.

For example, the `new` keyword is inherited from OCaml as a reserved word, so we can't use it directly. Instead Caramel uses the `make` word as a convention for creating new values of a given type.

Here we make `ets:new/2` callable as `Ets.make/2`:

```ocaml
(* file: Ets.ml *)

external make : 'a -> make_opt list -> ('k, 'v) t = "new"
```

### Bindings for functions with many input types

Some functions can take many different input types, such as tuples or lists, or
single values, and work just the same. This is thanks to the BEAM's run-time
support for pattern matching on values of different kinds.

When doing static typing, we say these functions work on Union Types.

Caramel does not support Union Types, so functions like `ets:insert/2` that
work both with a single values and a list of values can't be mapped by a single
function type.

Instead, we split them in several bindings that all map to the same foreign function:

```ocaml
(*file: Ets.ml *)

external insert_one : ('k, 'v) t -> 'k * 'v -> unit = "insert"

external insert_many : ('k, 'v) t -> ('k * 'v) list -> unit = "insert"
```


### Bindings for functions with many output types

Some function can return many different output types, such as tuples or lists,
or different single values, and they expect you to handle them just the same.
This works thanks to the BEAM's run-time support for pattern-matching on values
of different kinds.

When doing static typing, we say these functions return Union Types.

Caramel does not support Union Types, so functions like this one:

```erlang
-spec f(integer()) :: none | {integer(), integer(), integer()} | binary().
f(0) -> none;
f(1) -> {1234, 5678, 9};
f(2) -> <<"TEN!">>.
```

That can return either an atom, like `none`, or a triple of integers, or a
binary string, can not be called _directly_.

Instead we have to choose to pay a small cost for runtime typing, or to
restrict the values coming from Erlang.

#### Restricting Values

If we know that the returned values from the Erlang code are _tagged tuples_,
on any level of nesting, and we can map them to a type in Caramel, then we can
simplify the process by declaring the type on the Caramel side and letting the
compilation process match the structures.

For example, if the function from before returned:

```erlang
-spec f(integer()) :: {atom, none}
                    | {triple, {integer(), integer(), integer()}}
                    | {string, binary()}.
f(0) -> none;
f(1) -> {1234, 5678, 9};
f(2) -> <<"TEN!">>.
```

We can bind to it by writing out the type first:

```ocaml
type none = None
type f_return =
  | Atom of None
  | Triple of int * int * int
  | String of string

external f : int -> f_return = ""
```

Fortunately plenty of Erlang code heavily relies on tagged tuples for runtime
pattern matching.

When it doesn't, we have to do the typing at runtime ourselves.

#### Runtime Typing

Paying a small cost means that we have to add some runtime inspection of values
to decide what types they actually have. This is not a big deal because it is
something we do in Erlang all the time anyways, but it certainly means that
calling these Erlang functions is not a _zero cost_ operation.

For example, we can use the functions in the `Erlang` module to check if the
return value is of some type:

```ocaml
type unknown
external __unsafe_f : int -> unknown = "f"

type f_values = 
  | Binary of string
  | Atom of string
  | Triple of (int * int * int)

let f : int -> f_values =
 fun i ->
  let x = __unsafe_f i in
  if Erlang.is_binary x then Binary (unsafe_cast x)
  else if Erlang.is_atom x then Atom (unsafe_cast x)
  else if Erlang.is_tuple x 3 then Triple (unsafe_cast x)
  else panic
```

At this point, we could add a lot more validation logic to our `f` function to
try to make 100% sure that the value we have is of the type we expect.

This process can be error prone, and certainly relies on the Erlang code never 
returning any value that we didn't account for.

It is worth noting that this can also be done _on the Erlang side_, perhaps
even more idiomatically, by tagging the values:

```erlang
tagged_f(X) ->
  case f(X) of
    none -> {atom, none};
    {_, _, _} -> {triple, X};
    Y when is_binary(Y) -> {string, Y}
  end.
```

After this we can use the [Restricting Values](#restricting-values) approach.
