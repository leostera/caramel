-module(function_declaration).

literal_atom() -> ok.
literal_quoted_atom() -> 'What.is_going:on!'.
literal_integer() -> 1.
literal_float() -> 1.0.

tuple_empty() -> {}.
tuple_nested() -> {{}, {{}}}.
tuple_filled() -> {ok, {error, <<"hello">>, 1}, tuple_empty()}.

list_empty() -> [].
list_nested() -> [[], [[]]].
list_filled() -> [ok, [error, <<"hello">>, 1], list_empty()].

list_cons() -> [a | [{b, c} | [f() | []]]].

fun_args(ok) -> ok.
fun_args('What.is_going:on!') -> ok.
fun_args(1) -> ok.
fun_args(1.0) -> ok.

fun_args({}) -> ok.
fun_args({{}, {{}}}) -> ok.
fun_args({ok, {error, <<"hello">>, 1}}) -> ok.

fun_args([]) -> ok.
fun_args([[], [[]]]) -> ok.
fun_args([ok, [error, <<"hello">>, 1]]) -> ok.
fun_args([a | [{b, c} | [f | []]]]) -> ok.
