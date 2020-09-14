% Source code generated with Caramel.
-module(fsm).

-export_type([invalid/0]).
-export_type([state/1]).
-export_type([valid/0]).

-export([empty/1]).
-export([make/1]).
-export([nested_lists/1]).
-export([run/1]).
-export([start/1]).
-export([validate/1]).

-type valid() :: reference().

-type invalid() :: reference().

-type state(_A) :: #{ counter => integer()
                    , flag => boolean()
                    }.

empty({}) ->
  #{ counter => 0
   , flag => false
   }.

make({}) -> empty({}).

validate(#{ counter := Counter, flag := Flag }) -> {ok, #{ counter => Counter
 , flag => Flag
 }}.

run(#{ counter := Counter }) ->
  _ = io:format(<<"counter=~p\n">>, [Counter | []]),
  {}.

nested_lists({}) -> [[1 | [2 | []]] | [[3 | [4 | []]] | []]].

start({}) ->
  Empty = make({}),
  case validate(Empty) of
    {ok, S} -> run(S);
    {error, Err} -> io:format(<<"~p">>, [Err | [Err | []]])
  end.


