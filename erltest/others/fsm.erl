% Source code generated with Caramel.
-module(fsm).

-export_type([invalid/0]).
-export_type([state/1]).
-export_type([valid/0]).

-export([empty/1]).
-export([make/1]).
-export([run/1]).
-export([start/1]).
-export([validate/1]).

-type valid() :: ref().

-type invalid() :: ref().

-type state(A) :: #{ counter => int()
                   , flag => bool()
                   }.

empty({}) ->
  #{ counter => 0
   , flag => false
   }.

make({}) -> empty().

validate(#{ counter := Counter, flag := Flag }) -> {ok, #{ counter => Counter
 , flag => Flag
 }}.

run(#{ counter := Counter }) ->
  _ = print_string(<<"counter=">>),
  _ = print_int(Counter),
  _ = print_newline(),
  {}.

start({}) ->
  Empty = make(),
  case validate(Empty) of
    {ok, S} -> run(S);
    {error, Err} -> print_string(Err)
  end.


