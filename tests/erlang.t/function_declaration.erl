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

list_cons() -> [a | [{b, c} | [list_empty() | []]]].

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
fun_arg_var_ignore_in_list([_, B]) -> B.
fun_arg_var_ignore_in_cons([A | _]) -> A.

fun_args(A, B) -> {A, B}.
fun_args(A, B, C) -> {A, B, C}.
fun_args(A, B, C, D) -> {A, B, C, D}.
fun_args(A, B, C, D, E) -> {A, B, C, D, E}.
fun_args(A, B, C, D, E, F) -> {A, B, C, D, E, F}.
fun_args(A, B, C, D, E, F, G) -> {A, B, C, D, E, F, G}.
fun_args(A, B, C, D, E, F, G, H) -> {A, B, C, D, E, F, G, H}.
fun_args(A, B, C, D, E, F, G, H, I) -> {A, B, C, D, E, F, G, H, I}.

binding_return() -> A = 1.
binding_and_return() -> A = 1, A.

case_expr(A) ->
  case A of
    true -> false;
    {true} -> false;
    [false] -> true;
    [false|_] -> true;
    [false, B | T ] -> true;
    false -> true
  end.

fun_ref() -> fun fun_ref/0.

lambda() -> fun () -> ok end.
lambda_with_args() -> fun (A) -> A end.
lambda_in_var() -> F = fun (A) -> A end.
lambda_var_call() -> F = fun (A) -> A end, F(1).

send(A) -> A ! A.
send_chain(A) -> A ! A ! A.

recv() -> receive X -> X end.
recv_with_after() -> receive X -> X after infinity -> ok end.
recv_selectively() ->
  receive
    true -> false;
    {true} -> false;
    [false] -> true;
    [false|_] -> true;
    [false, B | T ] -> true;
    false -> true
  after
    infinity -> ok
  end.

fun_cases(1) -> ok;
fun_cases(2) -> ok;
fun_cases(3) -> ok;
fun_cases(_) -> false.

fib(0) -> 0;
fib(1) -> 1;
fib(N) -> erlang:'+'(fib(erlang:'-'(N,1)), fib(erlang:'-'(N,2))).
