# Caramel
> :candy: An Erlang backend to the OCaml compiler


## Getting started

You can bootstrap the system by running:

```sh
caramel $ make deps build
```

It relies on `opam` being installed, and `make deps` will take care of
installing all the necessary `opam` dependencies.

After `make` you should have a `caramelc` binary to play around with. 

## Hacking

The target `make watch` will start the Dune build watcher, and recompile the
project as you save files.

You can run tests with `make test`, and reformat the source code with `make
fmt`.

For a more in-depth description of what's happening, check the the [OCaml to
Erlang](./docs/ocaml_to_erlang.md) document, and the [Caramel arch
picture](./docs/caramel_arch.jpg).

## Examples

You can find several examples in [`./examples`](./examples), and in
[`./tests/compiler`].

In the examples, you can run `caramelc compile *.ml` to get them all built in
the right order.

Here's an OCaml module and the compiled Erlang module:

```ocaml
type msg = [ `Reset | `Add of int | `Hello of string ]

type state = string * int

let handle_message : state -> msg option -> state =
 fun state msg ->
  let x, y = state in
  match msg with
  | Some `Reset -> ("", 0)
  | Some (`Add z) -> (x, z)
  | Some (`Hello n) -> (n, y)
  | None -> state

let rec loop ~recv state =
  Io.format "current_state: ~p\n" [ state ];
  let msg = recv ~timeout:(Process.Bounded 5000) in
  let state2 = handle_message state msg in
  loop ~recv state2

let start x = Process.make (fun _self recv -> loop ~recv x)

let do_work () =
  let pid = start ("hi", 0) in
  Erlang.send pid (`Hello "joe")
```

```erlang
% Source code generated with Caramel.
-module(holder_annotated).
-export_type([msg/0]).
-export_type([state/0]).

-export([do_work/0]).
-export([handle_message/2]).
-export([loop/2]).
-export([start/1]).

-type msg() :: reset
             | {add, integer()}
             | {hello, binary()}
             .

-type state() :: {binary(), integer()}.

-spec handle_message(state(), option:t(msg())) -> state().
handle_message(State, Msg) ->
  {X, Y} = State,
  case Msg of
    {some, reset} -> {<<"">>, 0};
    {some, {add, Z}} -> {X, Z};
    {some, {hello, N}} -> {N, Y};
    none -> State
  end.

-spec loop(fun((process:after_time()) -> option:t(msg())), state()) -> ok.
loop(Recv, State) ->
  io:format(<<"current_state: ~p\n">>, [State | []]),
  Msg = Recv({bounded, 5000}),
  State2 = handle_message(State, Msg),
  loop(Recv, State2).

-spec start(state()) -> erlang:pid(msg()).
start(X) -> process:make(fun
  (_self, Recv) -> loop(Recv, X)
end).

-spec do_work() -> ok.
do_work() ->
  Pid = start({<<"hi">>, 0}),
  erlang:send(Pid, {hello, <<"joe">>}).
```

Running on the Erlang shell we get this output:

```erlang
examples/processes λ erl
Erlang/OTP 23 [erts-11.0.3] [source] [64-bit] [smp:64:64] [ds:64:64:10] [async-threads:1] [hipe]

Eshell V11.0.3  (abort with ^G)
1> c(holder_annotated).
{ok,holder_annotated}
2> holder_annotated:
do_work/0         handle_message/2  loop/2            module_info/0
module_info/1     start/1
2> holder_annotated:do_work().
current_state: {<<"hi">>,0}
{hello,<<"joe">>}
current_state: {<<"joe">>,0}
current_state: {<<"joe">>,0}
3>
BREAK: (a)bort (A)bort with dump (c)ontinue (p)roc info (i)nfo
       (l)oaded (v)ersion (k)ill (D)b-tables (d)istribution
```
