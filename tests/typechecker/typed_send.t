  $ cat >typed_process.erl <<EOF
  > -module(typed_process).
  > -export([spawn_int/1, start/0]).
  > loop(Recv, S) ->
  >   receive
  >     {replace, X} -> loop(Recv, X);
  >     print -> _ = print_int(S), loop(Recv, S)
  >   end.
  > spawn_int(S) -> erlang:spawn(fun () -> loop(S) end).
  > start() -> spawn_int(0) ! {replace, yes}.
  > EOF
  $ caramelc check --dump-ast erlang.ml typed_process.erl
  ((file_name typed_process.erl) (behaviours ())
    (module_name (Atom typed_process)) (attributes ())
    (exports
      (((exp_type Export_function) (exp_name (Atom spawn_int)) (exp_arity 1))
        ((exp_type Export_function) (exp_name (Atom start)) (exp_arity 0))))
    (types ())
    (functions
      (((fd_name (Atom loop)) (fd_arity 2)
         (fd_cases
           (((c_lhs
               ((Pattern_binding (Var_name Recv))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_recv
                  ((rcv_cases
                     (((c_lhs
                         ((Pattern_tuple
                            ((Pattern_match (Lit_atom (Atom replace)))
                              (Pattern_binding (Var_name X))))))
                        (c_guard ())
                        (c_rhs
                          (Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom loop))))
                              (fa_args
                                ((Expr_name (Var_name Recv))
                                  (Expr_name (Var_name X))))))))
                       ((c_lhs ((Pattern_match (Lit_atom (Atom print)))))
                         (c_guard ())
                         (c_rhs
                           (Expr_let
                             ((lb_lhs (Pattern_binding (Var_name _)))
                               (lb_rhs
                                 (Expr_apply
                                   ((fa_name
                                      (Expr_name (Atom_name (Atom print_int))))
                                     (fa_args ((Expr_name (Var_name S))))))))
                             (Expr_apply
                               ((fa_name (Expr_name (Atom_name (Atom loop))))
                                 (fa_args
                                   ((Expr_name (Var_name Recv))
                                     (Expr_name (Var_name S)))))))))))
                    (rcv_after ())))))))
         (fd_spec ()))
        ((fd_name (Atom spawn_int)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pattern_binding (Var_name S)))) (c_guard ())
               (c_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom spawn)))))
                     (fa_args
                       ((Expr_fun
                          (((c_lhs ()) (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name (Expr_name (Atom_name (Atom loop))))
                                   (fa_args ((Expr_name (Var_name S))))))))))))))))))
          (fd_spec ()))
        ((fd_name (Atom start)) (fd_arity 0)
          (fd_cases
            (((c_lhs ()) (c_guard ())
               (c_rhs
                 (Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom send)))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom spawn_int))))
                            (fa_args ((Expr_literal (Lit_integer 0))))))
                         (Expr_tuple
                           ((Expr_literal (Lit_atom (Atom replace)))
                             (Expr_literal (Lit_atom (Atom yes)))))))))))))
          (fd_spec ())))))
  
  module Typed_process =
    struct
      let rec loop (recv, s) =
        match recv () with
        | `replace x -> loop (recv, x)
        | `print -> let _ = print_int s in loop (recv, s)
      let rec spawn_int s = Erlang.spawn (fun recv -> loop (recv, s))
      let rec start () = Erlang.send ((spawn_int 0), (`replace `yes))
    end
  
  File "_none_", line 1:
  Error: This expression has type unit but an expression was expected of type
           unit -> [< `print | `replace of int ]
