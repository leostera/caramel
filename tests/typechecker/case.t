  $ cat >case_test.erl <<EOF
  > -module(case_test).
  > -export([f/1]).
  > f(A) ->
  >   case A of
  >     {int, I} -> print_int(I);
  >     {str, B} -> print_string(B)
  >   end.
  > 
  > g() -> f(1).
  > 
  > EOF
  $ caramelc compile --dump-ast case_test.erl
  ((file_name case_test.erl) (behaviours ()) (module_name case_test)
    (ocaml_name Case_test) (attributes ())
    (exports (((exp_type Export_function) (exp_name f) (exp_arity 1))))
    (types ())
    (functions
      (((fd_name f) (fd_arity 1)
         (fd_cases
           (((fc_name f) (fc_lhs ((Pattern_binding A))) (fc_guards ())
              (fc_rhs
                (Expr_case (Expr_name (Var_name A))
                  (((cb_pattern
                      (Pattern_tuple
                        ((Pattern_match (Lit_atom int)) (Pattern_binding I))))
                     (cb_expr
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name print_int)))
                           (fa_args ((Expr_name (Var_name I))))))))
                    ((cb_pattern
                       (Pattern_tuple
                         ((Pattern_match (Lit_atom str)) (Pattern_binding B))))
                      (cb_expr
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name print_string)))
                            (fa_args ((Expr_name (Var_name B))))))))))))))
         (fd_spec ()))
        ((fd_name g) (fd_arity 0)
          (fd_cases
            (((fc_name g) (fc_lhs ()) (fc_guards ())
               (fc_rhs
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name f)))
                     (fa_args ((Expr_literal (Lit_integer 1))))))))))
          (fd_spec ())))))
  
  module Case_test =
    struct
      let rec f a =
        match a with | `int i -> print_int i | `str b -> print_string b
      let rec g () = f 1
    end
  
  File "_none_", line 1:
  Error: This expression has type int but an expression was expected of type
           [< `int of int | `str of string ]
