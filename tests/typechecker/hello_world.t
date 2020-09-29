  $ cat >hello_world.erl <<EOF
  > -module(hello_world).
  > -export([hello_world/0]).
  > hello_world() -> print_int(<<"Hello world!">>).
  > EOF
  $ caramelc check --dump-ast hello_world.erl
  ((file_name hello_world.erl) (behaviours ()) (module_name (Atom hello_world))
    (attributes ())
    (exports
      (((exp_type Export_function) (exp_name (Atom hello_world)) (exp_arity 0))))
    (types ())
    (functions
      (((fd_name (Atom hello_world)) (fd_arity 0)
         (fd_cases
           (((c_lhs ()) (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom print_int))))
                    (fa_args ((Expr_literal (Lit_binary "Hello world!"))))))))))
         (fd_spec ())))))
  
  module Hello_world =
    struct let rec hello_world () = print_int "Hello world!" end
  
  File "_none_", line 1:
  Error: This expression has type string but an expression was expected of type
           int
  $ cat hello_world.ml
  cat: hello_world.ml: No such file or directory
  [1]
