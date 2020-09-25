  $ cat >fib.erl <<EOF
  > -module(fib).
  > -export([fib/1]).
  > fib(0) -> 0;
  > fib(1) -> 1;
  > fib(N) -> erlang:'+'(fib(erlang:'-'(N,1)), fib(erlang:'-'(N,2))).
  > fib() -> fib(no).
  > EOF
  $ caramelc compile --dump-ast fib.erl
  ((file_name fib.erl) (behaviours ()) (module_name fib) (ocaml_name Fib)
    (attributes ())
    (exports (((exp_type Export_function) (exp_name fib) (exp_arity 1))))
    (types ())
    (functions
      (((fd_name fib) (fd_arity 1)
         (fd_cases
           (((fc_name fib) (fc_lhs ((Pattern_match (Lit_integer 0))))
              (fc_guards ()) (fc_rhs (Expr_literal (Lit_integer 0))))
             ((fc_name fib) (fc_lhs ((Pattern_match (Lit_integer 1))))
               (fc_guards ()) (fc_rhs (Expr_literal (Lit_integer 1))))
             ((fc_name fib) (fc_lhs ((Pattern_binding N))) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name (Qualified_name (n_mod erlang) (n_name +))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name fib)))
                            (fa_args
                              ((Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod erlang)
                                        (n_name -))))
                                   (fa_args
                                     ((Expr_name (Var_name N))
                                       (Expr_literal (Lit_integer 1))))))))))
                         (Expr_apply
                           ((fa_name (Expr_name (Atom_name fib)))
                             (fa_args
                               ((Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod erlang)
                                         (n_name -))))
                                    (fa_args
                                      ((Expr_name (Var_name N))
                                        (Expr_literal (Lit_integer 2)))))))))))))))))))
        ((fd_name fib) (fd_arity 0)
          (fd_cases
            (((fc_name fib) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name fib)))
                     (fa_args ((Expr_literal (Lit_atom no))))))))))))))
  
  module Fib =
    struct
      let rec fib =
        function
        | 0 -> 0
        | 1 -> 1
        | n -> Stdlib.(+) (fib (Stdlib.(-) n 1)) (fib (Stdlib.(-) n 2))
      let rec fib = function | () -> fib `no
    end
  
  File "_none_", line 1:
  Error: This expression has type [> `no ]
         but an expression was expected of type unit
