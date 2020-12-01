% Source code generated with Caramel.
-module(runner).

-export([main/1]).

-spec main(any()) -> ok.
main(_) ->
  io:format(<<"\n\n# Advent Of Code 2020!\n\n">>, []),
  Days = [{1, fun
  () -> day_1:run()
end} | []],
  lists:foreach(fun
  ({N, Day}) ->
  io:format(<<"Running day ~p...">>, [N | []]),
  Result = Day(),
  case Result of
    {ok, _} -> io:format(<<"OK\n">>, []);
    {error, _} -> io:format(<<"ERR\n">>, [])
  end
end, Days),
  io:format(<<"\n\n">>, []).


