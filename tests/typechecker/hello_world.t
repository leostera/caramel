  $ cat >hello_world.erl <<EOF
  > -module(hello_world).
  > -export([hello_world/0]).
  > hello_world() -> print_int(<<"Hello world!">>).
  > EOF
  $ caramelc compile --dump-ast hello_world.erl
  ((file_name hello_world.erl) (behaviours ()) (module_name hello_world)
    (ocaml_name Hello_world) (attributes ())
    (exports
      (((exp_type Export_function) (exp_name hello_world) (exp_arity 0))))
    (types ())
    (functions
      (((fd_name hello_world) (fd_arity 0)
         (fd_cases
           (((fc_name hello_world) (fc_lhs ()) (fc_guards ())
              (fc_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name print_int)))
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
