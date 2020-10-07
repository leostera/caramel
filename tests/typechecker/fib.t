  $ cat >fib.erl <<EOF
  > -module(fib).
  > -export([fib/1]).
  > fib(0) -> 0;
  > fib(1) -> 1;
  > fib(N) -> erlang:'+'(fib(erlang:'-'(N,1)), fib(erlang:'-'(N,2))).
  > fib() -> fib(no).
  > EOF
  $ caramelc check --dump-ast fib.erl
  ((file_name fib.erl) (behaviours ()) (module_name (Atom fib)) (attributes ())
    (exports
      (((exp_type Export_function) (exp_name (Atom fib)) (exp_arity 1))))
    (types ())
    (functions
      (((fd_name (Atom fib)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pattern_match (Lit_integer 0)))) (c_guard ())
              (c_rhs (Expr_literal (Lit_integer 0))))
             ((c_lhs ((Pattern_match (Lit_integer 1)))) (c_guard ())
               (c_rhs (Expr_literal (Lit_integer 1))))
             ((c_lhs ((Pattern_binding (Var_name N)))) (c_guard ())
               (c_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '+')))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom fib))))
                            (fa_args
                              ((Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod (Atom erlang))
                                        (n_name (Atom '-')))))
                                   (fa_args
                                     ((Expr_name (Var_name N))
                                       (Expr_literal (Lit_integer 1))))))))))
                         (Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom fib))))
                             (fa_args
                               ((Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod (Atom erlang))
                                         (n_name (Atom '-')))))
                                    (fa_args
                                      ((Expr_name (Var_name N))
                                        (Expr_literal (Lit_integer 2))))))))))))))))))
         (fd_spec ()))
        ((fd_name (Atom fib)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom fib))))
                     (fa_args ((Expr_literal (Lit_atom (Atom no)))))))))))
          (fd_spec ())))))
  
  module Fib =
    struct
      let rec fib =
        function
        | 0 -> 0
        | 1 -> 1
        | n ->
            Stdlib.(+) ((fib (Stdlib.(-) (n, 1))), (fib (Stdlib.(-) (n, 2))))
      let rec fib () = fib `no
    end
  
  File "_none_", line 1:
  Error: This expression has type 'a * 'b
         but an expression was expected of type int
