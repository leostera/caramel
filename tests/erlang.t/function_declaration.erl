-module(function_declaration).

literal_atom() -> ok.
literal_integer() -> 1.
literal_float() -> 1.0.

tuple_empty() -> {}.
tuple_nested() -> {{}, {{}}}.
tuple_filled() -> {ok, {error, <<"hello">>, 1}, tuple_empty()}.

list_empty() -> [].
list_nested() -> [[], [[]]].
list_filled() -> [ok, [error, <<"hello">>, 1], list_empty()].
