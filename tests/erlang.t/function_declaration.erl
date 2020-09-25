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

fun_args_atom(ok) -> ok.
fun_args_quoted_atom('What.is_going:on!') -> ok.
fun_args_integer(1) -> ok.
fun_args_float(1.0) -> ok.

fun_args_tuple_empty({}) -> ok.
fun_args_tuple_nested({{}, {{}}}) -> ok.
fun_args_tuple_filled({ok, {error, <<"hello">>, 1}}) -> ok.

fun_args_list_empty([]) -> ok.
fun_args_list_nested([[], [[]]]) -> ok.
fun_args_list_filled([ok, [error, <<"hello">>, 1]]) -> ok.
fun_args_list_cons([a | [{b, c} | [f | []]]]) -> ok.

fun_arg_var_in_tuple({A}) -> A.
fun_arg_var_in_list([A]) -> A.
fun_arg_var(A) -> A.
fun_arg_var_ignore(_) -> ok.
fun_arg_var_ignore_in_tuple({A, _}) -> A.
fun_arg_var_ignore_in_list([_, B]) -> A.
fun_arg_var_ignore_in_cons([A | _]) -> A.

fun_args(A, B) -> {A, B}.
fun_args(A, B, C) -> {A, B, C}.
fun_args(A, B, C, D) -> {A, B, C, D}.
fun_args(A, B, C, D, E) -> {A, B, C, D, E}.
fun_args(A, B, C, D, E, F) -> {A, B, C, D, E, F}.
fun_args(A, B, C, D, E, F, G) -> {A, B, C, D, E, F, G}.
fun_args(A, B, C, D, E, F, G, H) -> {A, B, C, D, E, F, G, H}.
fun_args(A, B, C, D, E, F, G, H, I) -> {A, B, C, D, E, F, G, H, I}.

f() -> A = 1.
f() -> A = 1, A.

negate(A) ->
  case A of
    true -> false;
    {true} -> false;
    [false] -> true;
    false -> true
  end.
