-module(result).

-export([
         exception_to_value/1
        ]).

exception_to_value(F) when is_function(F, 0) ->
  try {ok, F()}
  catch _:Reason:_ -> {error, Reason}
  end.
