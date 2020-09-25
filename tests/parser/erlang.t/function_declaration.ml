let rec literal_atom = function | () -> `ok
let rec literal_quoted_atom = function | () -> `What.is_going:on!
let rec literal_integer = function | () -> 1
let rec literal_float = function | () -> 1.0
let rec tuple_empty = function | () -> ()
let rec tuple_nested = function | () -> ((), ())
let rec tuple_filled = function | () -> (`ok, (`error, "hello", 1), (tuple_empty ()))
let rec list_empty = function | () -> []
let rec list_nested = function | () -> [[]; [[]]]
let rec list_filled = function | () -> [`ok; [`error; "hello"; 1]; list_empty ()]
let rec list_cons = function | () -> [`a; [(`b, `c); [list_empty (); []]]]
let rec fun_args_atom = function | `ok -> `ok
let rec fun_args_quoted_atom = function | `What.is_going:on! -> `ok
let rec fun_args_integer = function | 1 -> `ok
let rec fun_args_float = function | 1.0 -> `ok
let rec fun_args_tuple_empty = function | () -> `ok
let rec fun_args_tuple_nested = function | ((), ()) -> `ok
let rec fun_args_tuple_filled = function | `ok (`error ("hello", 1)) -> `ok
let rec fun_args_list_empty = function | [] -> `ok
let rec fun_args_list_nested = function | []::([]::[])::[] -> `ok
let rec fun_args_list_filled = function | `ok::(`error::"hello"::1::[])::[] -> `ok
let rec fun_args_list_cons = function | `a::((`b `c)::(`f::[]::[])::[])::[] -> `ok
let rec fun_arg_var_in_tuple = function | a -> a
let rec fun_arg_var_in_list = function | a::[] -> a
let rec fun_arg_var = function | a -> a
let rec fun_arg_var_ignore = function | _ -> `ok
let rec fun_arg_var_ignore_in_tuple = function | (a, _) -> a
let rec fun_arg_var_ignore_in_list = function | _::b::[] -> b
let rec fun_arg_var_ignore_in_cons = function | a::_::[] -> a
let rec fun_args = function | (a, b) -> (a, b)
let rec fun_args = function | (a, b, c) -> (a, b, c)
let rec fun_args = function | (a, b, c, d) -> (a, b, c, d)
let rec fun_args = function | (a, b, c, d, e) -> (a, b, c, d, e)
let rec fun_args = function | (a, b, c, d, e, f) -> (a, b, c, d, e, f)
let rec fun_args = function | (a, b, c, d, e, f, g) -> (a, b, c, d, e, f, g)
let rec fun_args = function | (a, b, c, d, e, f, g, h) -> (a, b, c, d, e, f, g, h)
let rec fun_args = function | (a, b, c, d, e, f, g, h, i) -> (a, b, c, d, e, f, g, h, i)
let rec binding_return = function | () -> let a = 1 in a
let rec binding_and_return = function | () -> let a = 1 in a
let rec case_expr =
	function
	| a ->
			(match a with
			 | `true -> `false
			 | `true -> `false
			 | `false::[] -> `true
			 | `false::_::[] -> `true
			 | `false::b::t::[] -> `true
			 | `false -> `true)
let rec fun_ref = function | () -> fun_ref
let rec lambda = function | () -> (function | () -> `ok)
let rec lambda_with_args = function | () -> (function | a -> a)
let rec lambda_in_var = function | () -> let f = function | a -> a in f
let rec lambda_var_call =
	function | () -> let f = function | a -> a in f 1
let rec send = function | a -> Stdlib.send a a
let rec send_chain = function | a -> Stdlib.send a (Stdlib.send a a)
let rec recv = function | () -> (match recv () with | x -> x)
let rec recv_with_after = function | () -> (match recv () with | x -> x)
let rec recv_selectively =
	function
	| () ->
			(match recv () with
			 | `true -> `false
			 | `true -> `false
			 | `false::[] -> `true
			 | `false::_::[] -> `true
			 | `false::b::t::[] -> `true
			 | `false -> `true)
let rec fun_cases =
	function | 1 -> `ok | 2 -> `ok | 3 -> `ok | _ -> `false
let rec fib =
	function
	| 0 -> 0
	| 1 -> 1
	| n -> Stdlib.(+) (fib (Stdlib.(-) n 1)) (fib (Stdlib.(-) n 2))
let rec sequence =
	function
	| () ->
			let _ = print_string "hello" in
			let a = fib 2 in let _ = print_int a in `ok
