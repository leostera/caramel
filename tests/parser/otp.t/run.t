  $ caramelc parse binary.erl
  ((Module_comment (Comment %)) (Module_comment (Comment "% %CopyrightBegin%"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment "% Copyright Ericsson AB 2010-2020. All Rights Reserved."))
    (Module_comment (Comment %))
    (Module_comment
      (Comment
        "% Licensed under the Apache License, Version 2.0 (the \"License\");"))
    (Module_comment
      (Comment
        "% you may not use this file except in compliance with the License."))
    (Module_comment (Comment "% You may obtain a copy of the License at"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment "%     http://www.apache.org/licenses/LICENSE-2.0"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment
        "% Unless required by applicable law or agreed to in writing, software"))
    (Module_comment
      (Comment
        "% distributed under the License is distributed on an \"AS IS\" BASIS,"))
    (Module_comment
      (Comment
        "% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."))
    (Module_comment
      (Comment
        "% See the License for the specific language governing permissions and"))
    (Module_comment (Comment "% limitations under the License."))
    (Module_comment (Comment %)) (Module_comment (Comment "% %CopyrightEnd%"))
    (Module_comment (Comment %))
    (Module_attribute
      ((atr_name (Atom module))
        (atr_value (Expr_literal (Lit_atom (Atom binary))))))
    (Module_comment (Comment %))
    (Module_comment (Comment "% Implemented in this module:"))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom replace)))
                 (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom replace)))
                  (Expr_literal (Lit_integer 4)))))))))
    (Module_attribute
      ((atr_name (Atom export_type))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom cp)))
                 (Expr_literal (Lit_integer 0)))))))))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variant
              ((Type_const (Lit_atom (Atom am)))
                (Type_const (Lit_atom (Atom bm)))))
             (Type_constr
               ((tc_name (Atom_name (Atom reference))) (tc_args ()))))))
        (typ_kind Opaque) (typ_name (Atom cp)) (typ_params ())))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_constr
              ((tc_name (Atom_name (Atom non_neg_integer))) (tc_args ())))
             (Type_constr ((tc_name (Atom_name (Atom integer))) (tc_args ()))))))
        (typ_kind Type) (typ_name (Atom part)) (typ_params ())))
    (Module_comment (Comment "%% BIFs."))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom at)))
                 (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom bin_to_list)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom bin_to_list)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom bin_to_list)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom compile_pattern)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom copy)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom copy)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom decode_unsigned)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom decode_unsigned)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom encode_unsigned)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom encode_unsigned)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom first)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom last)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom list_to_bin)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom longest_common_prefix)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom longest_common_suffix)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom match)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom match)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom matches)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom matches)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom part)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom part)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom referenced_byte_size)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom split)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom split)))
                  (Expr_literal (Lit_integer 3)))))))))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom at))
        (typ_params
          ((Type_variable (Var_name Subject)) (Type_variable (Var_name Pos))))))
    (Function_decl
      ((fd_name (Atom at)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ())))))
        (typ_kind Spec) (typ_name (Atom bin_to_list))
        (typ_params ((Type_variable (Var_name Subject))))))
    (Function_decl
      ((fd_name (Atom bin_to_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name Subject)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom binary_to_list))))
                   (fa_args ((Expr_name (Var_name Subject))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ())))))
        (typ_kind Spec) (typ_name (Atom bin_to_list))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name PosLen))))))
    (Function_decl
      ((fd_name (Atom bin_to_list)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Subject))
                (Pattern_tuple
                  ((Pattern_binding (Var_name Pos))
                    (Pattern_binding (Var_name Len))))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom bin_to_list))))
                   (fa_args
                     ((Expr_name (Var_name Subject)) (Expr_name (Var_name Pos))
                       (Expr_name (Var_name Len))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _Subject))
                 (Pattern_binding (Var_name _BadArg))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang))
                         (n_name (Atom error)))))
                    (fa_args ((Expr_literal (Lit_atom (Atom badarg)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ())))))
        (typ_kind Spec) (typ_name (Atom bin_to_list))
        (typ_params
          ((Type_variable (Var_name Subject)) (Type_variable (Var_name Pos))
            (Type_variable (Var_name Len))))))
    (Function_decl
      ((fd_name (Atom bin_to_list)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Subject))
                (Pattern_binding (Var_name Pos))
                (Pattern_binding (Var_name Len))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom not)))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom is_binary))))
                            (fa_args ((Expr_name (Var_name Subject))))))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom not)))))
                      (fa_args
                        ((Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                             (fa_args ((Expr_name (Var_name Pos))))))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom not)))))
                      (fa_args
                        ((Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                             (fa_args ((Expr_name (Var_name Len)))))))))))))
             (c_rhs
               (Expr_comment
                 (Comment
                   "% binary_to_list/3 allows bitstrings as long as the slice fits, and we")
                 (Expr_comment
                   (Comment
                     "% want to badarg when Pos/Len aren't integers instead of raising badarith")
                   (Expr_comment
                     (Comment "% when adjusting args for binary_to_list/3.")
                     (Expr_apply
                       ((fa_name
                          (Expr_name
                            (Qualified_name (n_mod (Atom erlang))
                              (n_name (Atom error)))))
                         (fa_args ((Expr_literal (Lit_atom (Atom badarg))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Subject))
                 (Pattern_binding (Var_name Pos))
                 (Pattern_match (Lit_integer 0))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name Pos))
                          (Expr_literal (Lit_integer 0))))))
                   (Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom erlang))
                            (n_name (Atom '=<')))))
                       (fa_args
                         ((Expr_name (Var_name Pos))
                           (Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom byte_size))))
                               (fa_args ((Expr_name (Var_name Subject)))))))))))))
              (c_rhs
                (Expr_comment
                  (Comment "% binary_to_list/3 doesn't handle this case.")
                  (Expr_list ()))))
            ((c_lhs
               ((Pattern_binding (Var_name _Subject))
                 (Pattern_binding (Var_name _Pos))
                 (Pattern_match (Lit_integer 0))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang))
                         (n_name (Atom error)))))
                    (fa_args ((Expr_literal (Lit_atom (Atom badarg)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Subject))
                 (Pattern_binding (Var_name Pos))
                 (Pattern_binding (Var_name Len))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name Len))
                          (Expr_literal (Lit_integer 0)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom bin_to_list))))
                    (fa_args
                      ((Expr_name (Var_name Subject))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '+')))))
                            (fa_args
                              ((Expr_name (Var_name Pos))
                                (Expr_name (Var_name Len))))))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '-')))))
                            (fa_args ((Expr_name (Var_name Len))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Subject))
                 (Pattern_binding (Var_name Pos))
                 (Pattern_binding (Var_name Len))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Len))
                          (Expr_literal (Lit_integer 0)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom binary_to_list))))
                    (fa_args
                      ((Expr_name (Var_name Subject))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '+')))))
                            (fa_args
                              ((Expr_name (Var_name Pos))
                                (Expr_literal (Lit_integer 1))))))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '+')))))
                            (fa_args
                              ((Expr_name (Var_name Pos))
                                (Expr_name (Var_name Len))))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_constr ((tc_name (Atom_name (Atom cp))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom compile_pattern))
        (typ_params ((Type_variable (Var_name Pattern))))))
    (Function_decl
      ((fd_name (Atom compile_pattern)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom copy))
        (typ_params ((Type_variable (Var_name Subject))))))
    (Function_decl
      ((fd_name (Atom copy)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom copy))
        (typ_params
          ((Type_variable (Var_name Subject)) (Type_variable (Var_name N))))))
    (Function_decl
      ((fd_name (Atom copy)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Unsigned))) (typ_kind Spec)
        (typ_name (Atom decode_unsigned))
        (typ_params ((Type_variable (Var_name Subject))))))
    (Function_decl
      ((fd_name (Atom decode_unsigned)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Unsigned))) (typ_kind Spec)
        (typ_name (Atom decode_unsigned))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Endianness))))))
    (Function_decl
      ((fd_name (Atom decode_unsigned)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom encode_unsigned))
        (typ_params ((Type_variable (Var_name Unsigned))))))
    (Function_decl
      ((fd_name (Atom encode_unsigned)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom encode_unsigned))
        (typ_params
          ((Type_variable (Var_name Unsigned))
            (Type_variable (Var_name Endianness))))))
    (Function_decl
      ((fd_name (Atom encode_unsigned)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom first))
        (typ_params ((Type_variable (Var_name Subject))))))
    (Function_decl
      ((fd_name (Atom first)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom byte))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom last))
        (typ_params ((Type_variable (Var_name Subject))))))
    (Function_decl
      ((fd_name (Atom last)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom list_to_bin))
        (typ_params ((Type_variable (Var_name ByteList))))))
    (Function_decl
      ((fd_name (Atom list_to_bin)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr
           ((tc_name (Atom_name (Atom non_neg_integer))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom longest_common_prefix))
        (typ_params ((Type_variable (Var_name Binaries))))))
    (Function_decl
      ((fd_name (Atom longest_common_prefix)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr
           ((tc_name (Atom_name (Atom non_neg_integer))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom longest_common_suffix))
        (typ_params ((Type_variable (Var_name Binaries))))))
    (Function_decl
      ((fd_name (Atom longest_common_suffix)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_variable (Var_name Found))
             (Type_const (Lit_atom (Atom nomatch))))))
        (typ_kind Spec) (typ_name (Atom match))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))))))
    (Function_decl
      ((fd_name (Atom match)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_variable (Var_name Found))
             (Type_const (Lit_atom (Atom nomatch))))))
        (typ_kind Spec) (typ_name (Atom match))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))
            (Type_variable (Var_name Options))))))
    (Function_decl
      ((fd_name (Atom match)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Found))) (typ_kind Spec)
        (typ_name (Atom matches))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))))))
    (Function_decl
      ((fd_name (Atom matches)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Found))) (typ_kind Spec)
        (typ_name (Atom matches))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))
            (Type_variable (Var_name Options))))))
    (Function_decl
      ((fd_name (Atom matches)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom part))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name PosLen))))))
    (Function_decl
      ((fd_name (Atom part)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom binary))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom part))
        (typ_params
          ((Type_variable (Var_name Subject)) (Type_variable (Var_name Pos))
            (Type_variable (Var_name Len))))))
    (Function_decl
      ((fd_name (Atom part)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr
           ((tc_name (Atom_name (Atom non_neg_integer))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom referenced_byte_size))
        (typ_params ((Type_variable (Var_name Binary))))))
    (Function_decl
      ((fd_name (Atom referenced_byte_size)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Parts))) (typ_kind Spec)
        (typ_name (Atom split))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))))))
    (Function_decl
      ((fd_name (Atom split)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Parts))) (typ_kind Spec)
        (typ_name (Atom split))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))
            (Type_variable (Var_name Options))))))
    (Function_decl
      ((fd_name (Atom split)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "%% End of BIFs."))
    (Module_comment (Comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))
    (Module_comment (Comment "% replace"))
    (Module_comment (Comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Result))) (typ_kind Spec)
        (typ_name (Atom replace))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))
            (Type_variable (Var_name Replacement))))))
    (Function_decl
      ((fd_name (Atom replace)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H)) (Pattern_binding (Var_name N))
                (Pattern_binding (Var_name R))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom replace))))
                   (fa_args
                     ((Expr_name (Var_name H)) (Expr_name (Var_name N))
                       (Expr_name (Var_name R)) (Expr_list ())))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Result))) (typ_kind Spec)
        (typ_name (Atom replace))
        (typ_params
          ((Type_variable (Var_name Subject))
            (Type_variable (Var_name Pattern))
            (Type_variable (Var_name Replacement))
            (Type_variable (Var_name Options))))))
    (Function_decl
      ((fd_name (Atom replace)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Haystack))
                (Pattern_binding (Var_name Needles))
                (Pattern_binding (Var_name Replacement))
                (Pattern_binding (Var_name Options))))
             (c_guard ())
             (c_rhs
               (Expr_try
                 ((try_expr
                    (Expr_let
                      ((lb_lhs (Pattern_match (Lit_atom (Atom true))))
                        (lb_rhs
                          (Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom is_binary))))
                              (fa_args ((Expr_name (Var_name Replacement))))))))
                      (Expr_comment
                        (Comment " Make badarg instead of function clause")
                        (Expr_let
                          ((lb_lhs
                             (Pattern_tuple
                               ((Pattern_binding (Var_name Part))
                                 (Pattern_binding (Var_name Global))
                                 (Pattern_binding (Var_name Insert)))))
                            (lb_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Atom_name (Atom get_opts_replace))))
                                  (fa_args
                                    ((Expr_name (Var_name Options))
                                      (Expr_tuple
                                        ((Expr_literal (Lit_atom (Atom no)))
                                          (Expr_literal
                                            (Lit_atom (Atom false)))
                                          (Expr_list ())))))))))
                          (Expr_let
                            ((lb_lhs (Pattern_binding (Var_name Moptlist)))
                              (lb_rhs
                                (Expr_case (Expr_name (Var_name Part))
                                  (((c_lhs
                                      ((Pattern_match (Lit_atom (Atom no)))))
                                     (c_guard ()) (c_rhs (Expr_list ())))
                                    ((c_lhs
                                       ((Pattern_tuple
                                          ((Pattern_binding (Var_name A))
                                            (Pattern_binding (Var_name B))))))
                                      (c_guard ())
                                      (c_rhs
                                        (Expr_list
                                          ((Expr_tuple
                                             ((Expr_literal
                                                (Lit_atom (Atom scope)))
                                               (Expr_tuple
                                                 ((Expr_name (Var_name A))
                                                   (Expr_name (Var_name B))))))))))))))
                            (Expr_let
                              ((lb_lhs (Pattern_binding (Var_name MList)))
                                (lb_rhs
                                  (Expr_if
                                    (((((Expr_name (Var_name Global))))
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Qualified_name
                                                (n_mod (Atom binary))
                                                (n_name (Atom matches)))))
                                           (fa_args
                                             ((Expr_name (Var_name Haystack))
                                               (Expr_name (Var_name Needles))
                                               (Expr_name (Var_name Moptlist)))))))
                                      ((((Expr_literal (Lit_atom (Atom true)))))
                                        (Expr_case
                                          (Expr_apply
                                            ((fa_name
                                               (Expr_name
                                                 (Qualified_name
                                                   (n_mod (Atom binary))
                                                   (n_name (Atom match)))))
                                              (fa_args
                                                ((Expr_name
                                                   (Var_name Haystack))
                                                  (Expr_name
                                                    (Var_name Needles))
                                                  (Expr_name
                                                    (Var_name Moptlist))))))
                                          (((c_lhs
                                              ((Pattern_match
                                                 (Lit_atom (Atom nomatch)))))
                                             (c_guard ())
                                             (c_rhs (Expr_list ())))
                                            ((c_lhs
                                               ((Pattern_binding
                                                  (Var_name Match))))
                                              (c_guard ())
                                              (c_rhs
                                                (Expr_list
                                                  ((Expr_name (Var_name Match)))))))))))))
                              (Expr_let
                                ((lb_lhs (Pattern_binding (Var_name ReplList)))
                                  (lb_rhs
                                    (Expr_case (Expr_name (Var_name Insert))
                                      (((c_lhs ((Pattern_list ())))
                                         (c_guard ())
                                         (c_rhs
                                           (Expr_name (Var_name Replacement))))
                                        ((c_lhs
                                           ((Pattern_binding (Var_name Y))))
                                          (c_guard
                                            (((Expr_apply
                                                ((fa_name
                                                   (Expr_name
                                                     (Atom_name
                                                       (Atom is_integer))))
                                                  (fa_args
                                                    ((Expr_name (Var_name Y)))))))))
                                          (c_rhs
                                            (Expr_apply
                                              ((fa_name
                                                 (Expr_name
                                                   (Atom_name (Atom splitat))))
                                                (fa_args
                                                  ((Expr_name
                                                     (Var_name Replacement))
                                                    (Expr_literal
                                                      (Lit_integer 0))
                                                    (Expr_list
                                                      ((Expr_name (Var_name Y))))))))))
                                        ((c_lhs
                                           ((Pattern_binding (Var_name Li))))
                                          (c_guard
                                            (((Expr_apply
                                                ((fa_name
                                                   (Expr_name
                                                     (Atom_name (Atom is_list))))
                                                  (fa_args
                                                    ((Expr_name (Var_name Li)))))))))
                                          (c_rhs
                                            (Expr_apply
                                              ((fa_name
                                                 (Expr_name
                                                   (Atom_name (Atom splitat))))
                                                (fa_args
                                                  ((Expr_name
                                                     (Var_name Replacement))
                                                    (Expr_literal
                                                      (Lit_integer 0))
                                                    (Expr_apply
                                                      ((fa_name
                                                         (Expr_name
                                                           (Qualified_name
                                                             (n_mod
                                                               (Atom lists))
                                                             (n_name
                                                               (Atom sort)))))
                                                        (fa_args
                                                          ((Expr_name
                                                             (Var_name Li))))))))))))))))
                                (Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod (Atom erlang))
                                         (n_name (Atom iolist_to_binary)))))
                                    (fa_args
                                      ((Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom do_replace))))
                                           (fa_args
                                             ((Expr_name (Var_name Haystack))
                                               (Expr_name (Var_name MList))
                                               (Expr_name (Var_name ReplList))
                                               (Expr_literal (Lit_integer 0)))))))))))))))))
                   (try_catch
                     (((c_lhs
                         ((Pattern_catch (Class_throw)
                            (Pattern_binding (Var_name _)) ())))
                        (c_guard ())
                        (c_rhs
                          (Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom error)))))
                              (fa_args
                                ((Expr_literal (Lit_atom (Atom badarg)))))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom do_replace)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H)) (Pattern_list ())
                (Pattern_binding (Var_name _)) (Pattern_binding (Var_name N))))
             (c_guard ())
             (c_rhs
               (Expr_list
                 ((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom binary))
                           (n_name (Atom part)))))
                      (fa_args
                        ((Expr_name (Var_name H))
                          (Expr_tuple
                            ((Expr_name (Var_name N))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name (n_mod (Atom erlang))
                                       (n_name (Atom '-')))))
                                  (fa_args
                                    ((Expr_apply
                                       ((fa_name
                                          (Expr_name
                                            (Atom_name (Atom byte_size))))
                                         (fa_args ((Expr_name (Var_name H))))))
                                      (Expr_name (Var_name N))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H))
                 (Pattern_cons
                   ((Pattern_tuple
                      ((Pattern_binding (Var_name A))
                        (Pattern_binding (Var_name B)))))
                   (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name Replacement))
                 (Pattern_binding (Var_name N))))
              (c_guard ())
              (c_rhs
                (Expr_cons
                  ((Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom binary))
                            (n_name (Atom part)))))
                       (fa_args
                         ((Expr_name (Var_name H))
                           (Expr_tuple
                             ((Expr_name (Var_name N))
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod (Atom erlang))
                                        (n_name (Atom '-')))))
                                   (fa_args
                                     ((Expr_name (Var_name A))
                                       (Expr_name (Var_name N))))))))))))
                    (Expr_if
                      (((((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom is_list))))
                              (fa_args ((Expr_name (Var_name Replacement))))))))
                         (Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom do_insert))))
                             (fa_args
                               ((Expr_name (Var_name Replacement))
                                 (Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom binary))
                                          (n_name (Atom part)))))
                                     (fa_args
                                       ((Expr_name (Var_name H))
                                         (Expr_tuple
                                           ((Expr_name (Var_name A))
                                             (Expr_name (Var_name B)))))))))))))
                        ((((Expr_literal (Lit_atom (Atom true)))))
                          (Expr_name (Var_name Replacement))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom do_replace))))
                      (fa_args
                        ((Expr_name (Var_name H)) (Expr_name (Var_name T))
                          (Expr_name (Var_name Replacement))
                          (Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '+')))))
                              (fa_args
                                ((Expr_name (Var_name A))
                                  (Expr_name (Var_name B)))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom do_insert)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_list ((Pattern_binding (Var_name X))))
                (Pattern_binding (Var_name _))))
             (c_guard ()) (c_rhs (Expr_list ((Expr_name (Var_name X))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)) (Expr_name (Var_name R)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom do_insert))))
                      (fa_args
                        ((Expr_name (Var_name T)) (Expr_name (Var_name R)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom splitat)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H)) (Pattern_binding (Var_name N))
                (Pattern_list ())))
             (c_guard ())
             (c_rhs
               (Expr_list
                 ((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom binary))
                           (n_name (Atom part)))))
                      (fa_args
                        ((Expr_name (Var_name H))
                          (Expr_tuple
                            ((Expr_name (Var_name N))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name (n_mod (Atom erlang))
                                       (n_name (Atom '-')))))
                                  (fa_args
                                    ((Expr_apply
                                       ((fa_name
                                          (Expr_name
                                            (Atom_name (Atom byte_size))))
                                         (fa_args ((Expr_name (Var_name H))))))
                                      (Expr_name (Var_name N))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H)) (Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name I)))
                   (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_cons
                  ((Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom binary))
                            (n_name (Atom part)))))
                       (fa_args
                         ((Expr_name (Var_name H))
                           (Expr_tuple
                             ((Expr_name (Var_name N))
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod (Atom erlang))
                                        (n_name (Atom '-')))))
                                   (fa_args
                                     ((Expr_name (Var_name I))
                                       (Expr_name (Var_name N)))))))))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom splitat))))
                      (fa_args
                        ((Expr_name (Var_name H)) (Expr_name (Var_name I))
                          (Expr_name (Var_name T)))))))))))
        (fd_spec ())))
    (Module_comment (Comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))
    (Module_comment (Comment "% Simple helper functions"))
    (Module_comment (Comment %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%))
    (Function_decl
      ((fd_name (Atom get_opts_replace)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_list ())
                (Pattern_tuple
                  ((Pattern_binding (Var_name Part))
                    (Pattern_binding (Var_name Global))
                    (Pattern_binding (Var_name Insert))))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_name (Var_name Part)) (Expr_name (Var_name Global))
                   (Expr_name (Var_name Insert))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_tuple
                     ((Pattern_match (Lit_atom (Atom scope)))
                       (Pattern_tuple
                         ((Pattern_binding (Var_name A))
                           (Pattern_binding (Var_name B)))))))
                  (Pattern_binding (Var_name T)))
                 (Pattern_tuple
                   ((Pattern_binding (Var_name _Part))
                     (Pattern_binding (Var_name Global))
                     (Pattern_binding (Var_name Insert))))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom get_opts_replace))))
                    (fa_args
                      ((Expr_name (Var_name T))
                        (Expr_tuple
                          ((Expr_tuple
                             ((Expr_name (Var_name A))
                               (Expr_name (Var_name B))))
                            (Expr_name (Var_name Global))
                            (Expr_name (Var_name Insert))))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_match (Lit_atom (Atom global))))
                  (Pattern_binding (Var_name T)))
                 (Pattern_tuple
                   ((Pattern_binding (Var_name Part))
                     (Pattern_binding (Var_name _Global))
                     (Pattern_binding (Var_name Insert))))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom get_opts_replace))))
                    (fa_args
                      ((Expr_name (Var_name T))
                        (Expr_tuple
                          ((Expr_name (Var_name Part))
                            (Expr_literal (Lit_atom (Atom true)))
                            (Expr_name (Var_name Insert))))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_tuple
                     ((Pattern_match (Lit_atom (Atom insert_replaced)))
                       (Pattern_binding (Var_name N)))))
                  (Pattern_binding (Var_name T)))
                 (Pattern_tuple
                   ((Pattern_binding (Var_name Part))
                     (Pattern_binding (Var_name Global))
                     (Pattern_binding (Var_name _Insert))))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom get_opts_replace))))
                    (fa_args
                      ((Expr_name (Var_name T))
                        (Expr_tuple
                          ((Expr_name (Var_name Part))
                            (Expr_name (Var_name Global))
                            (Expr_name (Var_name N))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang))
                         (n_name (Atom throw)))))
                    (fa_args ((Expr_literal (Lit_atom (Atom badopt)))))))))))
        (fd_spec ()))))
  $ caramelc parse lists.erl
  ((Module_comment (Comment %)) (Module_comment (Comment "% %CopyrightBegin%"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment "% Copyright Ericsson AB 1996-2018. All Rights Reserved."))
    (Module_comment (Comment %))
    (Module_comment
      (Comment
        "% Licensed under the Apache License, Version 2.0 (the \"License\");"))
    (Module_comment
      (Comment
        "% you may not use this file except in compliance with the License."))
    (Module_comment (Comment "% You may obtain a copy of the License at"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment "%     http://www.apache.org/licenses/LICENSE-2.0"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment
        "% Unless required by applicable law or agreed to in writing, software"))
    (Module_comment
      (Comment
        "% distributed under the License is distributed on an \"AS IS\" BASIS,"))
    (Module_comment
      (Comment
        "% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied."))
    (Module_comment
      (Comment
        "% See the License for the specific language governing permissions and"))
    (Module_comment (Comment "% limitations under the License."))
    (Module_comment (Comment %)) (Module_comment (Comment "% %CopyrightEnd%"))
    (Module_comment (Comment %))
    (Module_attribute
      ((atr_name (Atom module))
        (atr_value (Expr_literal (Lit_atom (Atom lists))))))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom no_auto_import)))
              (Expr_list
                ((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '/')))))
                     (fa_args
                       ((Expr_literal (Lit_atom (Atom max)))
                         (Expr_literal (Lit_integer 2)))))))))))))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom no_auto_import)))
              (Expr_list
                ((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '/')))))
                     (fa_args
                       ((Expr_literal (Lit_atom (Atom min)))
                         (Expr_literal (Lit_integer 2)))))))))))))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom append)))
                 (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom append)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom subtract)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom reverse)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom nth)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom nthtail)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom prefix)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom suffix)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom droplast)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom last)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom seq)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom seq)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom sum)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom duplicate)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom min)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom max)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom sublist)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom sublist)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom delete)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom unzip)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom unzip3)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom zip)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom zip3)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom zipwith)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom zipwith3)))
                  (Expr_literal (Lit_integer 4))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom sort)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom merge)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom merge)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rmerge)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom merge3)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rmerge3)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom usort)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom umerge)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom umerge3)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom umerge)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rumerge3)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rumerge)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom concat)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom flatten)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom flatten)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom flatlength)))
                  (Expr_literal (Lit_integer 1))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keydelete)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keyreplace)))
                  (Expr_literal (Lit_integer 4))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keytake)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keystore)))
                  (Expr_literal (Lit_integer 4))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keysort)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keymerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rkeymerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rukeymerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom ukeysort)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom ukeymerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keymap)))
                  (Expr_literal (Lit_integer 3)))))))))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom merge)))
                 (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rmerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom sort)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom umerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom rumerge)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom usort)))
                  (Expr_literal (Lit_integer 2)))))))))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom all)))
                 (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom any)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom map)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom flatmap)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom foldl)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom foldr)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom filter)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom partition)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom zf)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom filtermap)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom mapfoldl)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom mapfoldr)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom foreach)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom takewhile)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom dropwhile)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom search)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom splitwith)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom split)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom join)))
                  (Expr_literal (Lit_integer 2)))))))))
    (Module_comment (Comment "%% BIFs"))
    (Module_attribute
      ((atr_name (Atom export))
        (atr_value
          (Expr_list
            ((Expr_tuple
               ((Expr_literal (Lit_atom (Atom keyfind)))
                 (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keymember)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom keysearch)))
                  (Expr_literal (Lit_integer 3))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom member)))
                  (Expr_literal (Lit_integer 2))))
              (Expr_tuple
                ((Expr_literal (Lit_atom (Atom reverse)))
                  (Expr_literal (Lit_integer 2)))))))))
    (Module_comment (Comment "% Shadowed by erl_bif_types: lists:keyfind/3"))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_variable (Var_name Tuple))
             (Type_const (Lit_atom (Atom false))))))
        (typ_kind Spec) (typ_name (Atom keyfind))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList))))))
    (Function_decl
      ((fd_name (Atom keyfind)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Shadowed by erl_bif_types: lists:keymember/3"))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom keymember))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList))))))
    (Function_decl
      ((fd_name (Atom keymember)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Shadowed by erl_bif_types: lists:keysearch/3"))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_tuple
              ((Type_const (Lit_atom (Atom value)))
                (Type_variable (Var_name Tuple))))
             (Type_const (Lit_atom (Atom false))))))
        (typ_kind Spec) (typ_name (Atom keysearch))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList))))))
    (Function_decl
      ((fd_name (Atom keysearch)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Shadowed by erl_bif_types: lists:member/2"))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom member))
        (typ_params
          ((Type_variable (Var_name Elem)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom member)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Shadowed by erl_bif_types: lists:reverse/2"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom reverse))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name Tail))))))
    (Function_decl
      ((fd_name (Atom reverse)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom nif_error)))))
                   (fa_args ((Expr_literal (Lit_atom (Atom undef)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "%% End of BIFs"))
    (Module_comment (Comment "% member(X, L) -> (true | false)"))
    (Module_comment (Comment "%  test if X is a member of the list L"))
    (Module_comment (Comment "%  Now a BIF!"))
    (Module_comment (Comment "member(X, [X|_]) -> true;"))
    (Module_comment (Comment "member(X, [_|Y]) ->"))
    (Module_comment (Comment "\tmember(X, Y);"))
    (Module_comment (Comment "member(X, []) -> false."))
    (Module_comment (Comment "% append(X, Y) appends lists X and Y"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom append))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom append)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom '++')))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% append(L) appends the list of lists L"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List1))) (typ_kind Spec)
        (typ_name (Atom append))
        (typ_params ((Type_variable (Var_name ListOfLists))))))
    (Function_decl
      ((fd_name (Atom append)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ((Pattern_binding (Var_name E))))))
             (c_guard ()) (c_rhs (Expr_name (Var_name E))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang))
                         (n_name (Atom '++')))))
                    (fa_args
                      ((Expr_name (Var_name H))
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom append))))
                            (fa_args ((Expr_name (Var_name T))))))))))))
            ((c_lhs ((Pattern_list ()))) (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% subtract(List1, List2) subtract elements in List2 form List1."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom subtract))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom subtract)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom '--')))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% reverse(L) reverse all elements in the list L. reverse/2 is now a BIF!"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom reverse))
        (typ_params ((Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom reverse)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_with_name (Pattern_list ()) (Var_name L))))
             (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_with_name
                  (Pattern_list ((Pattern_binding (Var_name _)))) (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_list
                  ((Pattern_binding (Var_name A))
                    (Pattern_binding (Var_name B))))))
              (c_guard ())
              (c_rhs
                (Expr_list ((Expr_name (Var_name B)) (Expr_name (Var_name A))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name A))
                    (Pattern_binding (Var_name B)))
                  (Pattern_binding (Var_name L)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_list
                          ((Expr_name (Var_name B)) (Expr_name (Var_name A))))))))))))
        (fd_spec ())))
    (Module_comment (Comment "reverse([H|T], Y) ->"))
    (Module_comment (Comment "    reverse(T, [H|Y]);"))
    (Module_comment (Comment "reverse([], X) -> X."))
    (Module_comment
      (Comment "% nth(N, L) returns the N`th element of the list L"))
    (Module_comment
      (Comment "% nthtail(N, L) returns the N`th tail of the list L"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Elem))) (typ_kind Spec)
        (typ_name (Atom nth))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom nth)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_match (Lit_integer 1))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name _)))))
             (c_guard ()) (c_rhs (Expr_name (Var_name H))))
            ((c_lhs
               ((Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name _)))
                   (Pattern_binding (Var_name T)))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom nth))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name N))
                               (Expr_literal (Lit_integer 1))))))
                        (Expr_name (Var_name T))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Tail))) (typ_kind Spec)
        (typ_name (Atom nthtail))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom nthtail)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_match (Lit_integer 1))
                (Pattern_cons ((Pattern_binding (Var_name _)))
                  (Pattern_binding (Var_name T)))))
             (c_guard ()) (c_rhs (Expr_name (Var_name T))))
            ((c_lhs
               ((Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name _)))
                   (Pattern_binding (Var_name T)))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom nthtail))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name N))
                               (Expr_literal (Lit_integer 1))))))
                        (Expr_name (Var_name T))))))))
            ((c_lhs
               ((Pattern_match (Lit_integer 0)) (Pattern_binding (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name L)))))))))
              (c_rhs (Expr_name (Var_name L))))))
        (fd_spec ())))
    (Module_comment (Comment "% prefix(Prefix, List) -> (true | false)"))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom prefix))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom prefix)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name X)))
                 (Pattern_binding (Var_name PreTail)))
                (Pattern_cons ((Pattern_binding (Var_name X)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom prefix))))
                   (fa_args
                     ((Expr_name (Var_name PreTail))
                       (Expr_name (Var_name Tail))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name List))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name List)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom true)))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name _)))
                  (Pattern_binding (Var_name _)))
                 (Pattern_binding (Var_name List))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name List)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom false)))))))
        (fd_spec ())))
    (Module_comment (Comment "% suffix(Suffix, List) -> (true | false)"))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom suffix))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom suffix)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Suffix))
                (Pattern_binding (Var_name List))))
             (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name Delta)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name
                          (Expr_name
                            (Qualified_name (n_mod (Atom erlang))
                              (n_name (Atom '-')))))
                         (fa_args
                           ((Expr_apply
                              ((fa_name (Expr_name (Atom_name (Atom length))))
                                (fa_args ((Expr_name (Var_name List))))))
                             (Expr_apply
                               ((fa_name (Expr_name (Atom_name (Atom length))))
                                 (fa_args ((Expr_name (Var_name Suffix))))))))))))
                 (Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>=')))))
                     (fa_args
                       ((Expr_name (Var_name Delta))
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom andalso)))))
                             (fa_args
                               ((Expr_literal (Lit_integer 0))
                                 (Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom erlang))
                                          (n_name (Atom '=:=')))))
                                     (fa_args
                                       ((Expr_apply
                                          ((fa_name
                                             (Expr_name
                                               (Atom_name (Atom nthtail))))
                                            (fa_args
                                              ((Expr_name (Var_name Delta))
                                                (Expr_name (Var_name List))))))
                                         (Expr_name (Var_name Suffix)))))))))))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment "% droplast(List) returns the list dropping its last element"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name InitList))) (typ_kind Spec)
        (typ_name (Atom droplast))
        (typ_params ((Type_variable (Var_name List))))))
    (Module_comment (Comment "% This is the simple recursive implementation"))
    (Module_comment
      (Comment "% reverse(tl(reverse(L))) is faster on average,"))
    (Module_comment (Comment "% but creates more garbage."))
    (Function_decl
      ((fd_name (Atom droplast)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_list ((Pattern_binding (Var_name _T))))))
             (c_guard ()) (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom droplast))))
                      (fa_args ((Expr_name (Var_name T)))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment "% last(List) returns the last element in a list."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Last))) (typ_kind Spec)
        (typ_name (Atom last)) (typ_params ((Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom last)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name E)))
                 (Pattern_binding (Var_name Es)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom last))))
                   (fa_args
                     ((Expr_name (Var_name E)) (Expr_name (Var_name Es))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom last)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name _))
                (Pattern_cons ((Pattern_binding (Var_name E)))
                  (Pattern_binding (Var_name Es)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom last))))
                   (fa_args
                     ((Expr_name (Var_name E)) (Expr_name (Var_name Es))))))))
            ((c_lhs ((Pattern_binding (Var_name E)) (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_name (Var_name E))))))
        (fd_spec ())))
    (Module_comment (Comment "% seq(Min, Max) -> [Min,Min+1, ..., Max]"))
    (Module_comment
      (Comment "% seq(Min, Max, Incr) -> [Min,Min+Incr, ..., Max]"))
    (Module_comment (Comment "%  returns the sequence Min..Max"))
    (Module_comment (Comment "%  Min <= Max and Min and Max must be integers"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Seq))) (typ_kind Spec)
        (typ_name (Atom seq))
        (typ_params
          ((Type_variable (Var_name From)) (Type_variable (Var_name To))))))
    (Function_decl
      ((fd_name (Atom seq)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name First))
                (Pattern_binding (Var_name Last))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name First))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Last))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '-')))))
                      (fa_args
                        ((Expr_name (Var_name First))
                          (Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '=<')))))
                              (fa_args
                                ((Expr_literal (Lit_integer 1))
                                  (Expr_name (Var_name Last)))))))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '-')))))
                          (fa_args
                            ((Expr_name (Var_name Last))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name (n_mod (Atom erlang))
                                       (n_name (Atom '+')))))
                                  (fa_args
                                    ((Expr_name (Var_name First))
                                      (Expr_literal (Lit_integer 1))))))))))
                       (Expr_name (Var_name Last)) (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom seq_loop)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>=')))))
                     (fa_args
                       ((Expr_name (Var_name N))
                         (Expr_literal (Lit_integer 4)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '-')))))
                          (fa_args
                            ((Expr_name (Var_name N))
                              (Expr_literal (Lit_integer 4))))))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name X))
                               (Expr_literal (Lit_integer 4))))))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '-')))))
                              (fa_args
                                ((Expr_name (Var_name X))
                                  (Expr_literal (Lit_integer 3))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom erlang))
                                    (n_name (Atom '-')))))
                               (fa_args
                                 ((Expr_name (Var_name X))
                                   (Expr_literal (Lit_integer 2))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom erlang))
                                    (n_name (Atom '-')))))
                               (fa_args
                                 ((Expr_name (Var_name X))
                                   (Expr_literal (Lit_integer 1))))))
                           (Expr_name (Var_name X)))
                         (Expr_name (Var_name L)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name N))
                               (Expr_literal (Lit_integer 2))))))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '-')))))
                            (fa_args
                              ((Expr_name (Var_name X))
                                (Expr_literal (Lit_integer 2))))))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom erlang))
                                    (n_name (Atom '-')))))
                               (fa_args
                                 ((Expr_name (Var_name X))
                                   (Expr_literal (Lit_integer 1))))))
                            (Expr_name (Var_name X)))
                          (Expr_name (Var_name L)))))))))
            ((c_lhs
               ((Pattern_match (Lit_integer 1)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name L))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name X))) (Expr_name (Var_name L)))))
            ((c_lhs
               ((Pattern_match (Lit_integer 0)) (Pattern_binding (Var_name _))
                 (Pattern_binding (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Seq))) (typ_kind Spec)
        (typ_name (Atom seq))
        (typ_params
          ((Type_variable (Var_name From)) (Type_variable (Var_name To))
            (Type_variable (Var_name Incr))))))
    (Function_decl
      ((fd_name (Atom seq)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name First))
                (Pattern_binding (Var_name Last))
                (Pattern_binding (Var_name Inc))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name First))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Last))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Inc)))))))))
             (c_rhs
               (Expr_if
                 (((((Expr_apply
                       ((fa_name
                          (Expr_name
                            (Qualified_name (n_mod (Atom erlang))
                              (n_name (Atom '>')))))
                         (fa_args
                           ((Expr_name (Var_name Inc))
                             (Expr_literal (Lit_integer 0))))))
                      (Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '-')))))
                          (fa_args
                            ((Expr_name (Var_name First))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name
                                     (Qualified_name (n_mod (Atom erlang))
                                       (n_name (Atom '=<')))))
                                  (fa_args
                                    ((Expr_name (Var_name Inc))
                                      (Expr_name (Var_name Last)))))))))))
                     ((Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '<')))))
                          (fa_args
                            ((Expr_name (Var_name Inc))
                              (Expr_literal (Lit_integer 0))))))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name First))
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Qualified_name (n_mod (Atom erlang))
                                        (n_name (Atom '>=')))))
                                   (fa_args
                                     ((Expr_name (Var_name Inc))
                                       (Expr_name (Var_name Last))))))))))))
                    (Expr_comment
                      (Comment
                        "% FIXME: the next 3 lines used to look like this:")
                      (Expr_comment
                        (Comment "%   N = (Last - First + Inc) div Inc,")
                        (Expr_comment
                          (Comment
                            "%   seq_loop(N, Inc*(N-1)+First, Inc, []);")
                          (Expr_comment
                            (Comment "% but parenthesizing doesn't work yet.")
                            (Expr_let
                              ((lb_lhs (Pattern_binding (Var_name N)))
                                (lb_rhs
                                  (Expr_apply
                                    ((fa_name
                                       (Expr_name
                                         (Qualified_name (n_mod (Atom erlang))
                                           (n_name (Atom '-')))))
                                      (fa_args
                                        ((Expr_name (Var_name Last))
                                          (Expr_apply
                                            ((fa_name
                                               (Expr_name
                                                 (Qualified_name
                                                   (n_mod (Atom erlang))
                                                   (n_name (Atom '+')))))
                                              (fa_args
                                                ((Expr_name (Var_name First))
                                                  (Expr_name (Var_name Inc))))))))))))
                              (Expr_let
                                ((lb_lhs (Pattern_binding (Var_name N2)))
                                  (lb_rhs
                                    (Expr_apply
                                      ((fa_name
                                         (Expr_name
                                           (Qualified_name
                                             (n_mod (Atom erlang))
                                             (n_name (Atom div)))))
                                        (fa_args
                                          ((Expr_name (Var_name N))
                                            (Expr_name (Var_name Inc))))))))
                                (Expr_let
                                  ((lb_lhs (Pattern_binding (Var_name N3)))
                                    (lb_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Qualified_name
                                               (n_mod (Atom erlang))
                                               (n_name (Atom '-')))))
                                          (fa_args
                                            ((Expr_name (Var_name N))
                                              (Expr_literal (Lit_integer 1))))))))
                                  (Expr_apply
                                    ((fa_name
                                       (Expr_name (Atom_name (Atom seq_loop))))
                                      (fa_args
                                        ((Expr_name (Var_name N))
                                          (Expr_apply
                                            ((fa_name
                                               (Expr_name
                                                 (Qualified_name
                                                   (n_mod (Atom erlang))
                                                   (n_name (Atom '*')))))
                                              (fa_args
                                                ((Expr_name (Var_name Inc))
                                                  (Expr_apply
                                                    ((fa_name
                                                       (Expr_name
                                                         (Qualified_name
                                                           (n_mod
                                                             (Atom erlang))
                                                           (n_name (Atom '+')))))
                                                      (fa_args
                                                        ((Expr_name
                                                           (Var_name N3))
                                                          (Expr_name
                                                            (Var_name First))))))))))
                                          (Expr_name (Var_name Inc))
                                          (Expr_list ())))))))))))))
                   ((((Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '=:=')))))
                          (fa_args
                            ((Expr_name (Var_name Inc))
                              (Expr_literal (Lit_integer 0))))))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '=:=')))))
                           (fa_args
                             ((Expr_name (Var_name First))
                               (Expr_name (Var_name Last))))))))
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                         (fa_args
                           ((Expr_literal (Lit_integer 1))
                             (Expr_name (Var_name First))
                             (Expr_name (Var_name Inc)) (Expr_list ()))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom seq_loop)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name D)) (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>=')))))
                     (fa_args
                       ((Expr_name (Var_name N))
                         (Expr_literal (Lit_integer 4)))))))))
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name Y)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name
                          (Expr_name
                            (Qualified_name (n_mod (Atom erlang))
                              (n_name (Atom '-')))))
                         (fa_args
                           ((Expr_name (Var_name X)) (Expr_name (Var_name D))))))))
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name Z)))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name D))))))))
                   (Expr_let
                     ((lb_lhs (Pattern_binding (Var_name W)))
                       (lb_rhs
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '-')))))
                             (fa_args
                               ((Expr_name (Var_name Z))
                                 (Expr_name (Var_name D))))))))
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                         (fa_args
                           ((Expr_apply
                              ((fa_name
                                 (Expr_name
                                   (Qualified_name (n_mod (Atom erlang))
                                     (n_name (Atom '-')))))
                                (fa_args
                                  ((Expr_name (Var_name N))
                                    (Expr_literal (Lit_integer 4))))))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name
                                    (Qualified_name (n_mod (Atom erlang))
                                      (n_name (Atom '-')))))
                                 (fa_args
                                   ((Expr_name (Var_name W))
                                     (Expr_name (Var_name D))))))
                             (Expr_name (Var_name D))
                             (Expr_cons
                               ((Expr_name (Var_name W))
                                 (Expr_name (Var_name Z))
                                 (Expr_name (Var_name Y))
                                 (Expr_name (Var_name X)))
                               (Expr_name (Var_name L))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name D)) (Pattern_binding (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs
                (Expr_let
                  ((lb_lhs (Pattern_binding (Var_name Y)))
                    (lb_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name
                             (Qualified_name (n_mod (Atom erlang))
                               (n_name (Atom '-')))))
                          (fa_args
                            ((Expr_name (Var_name X)) (Expr_name (Var_name D))))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom seq_loop))))
                      (fa_args
                        ((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '-')))))
                             (fa_args
                               ((Expr_name (Var_name N))
                                 (Expr_literal (Lit_integer 2))))))
                          (Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '-')))))
                              (fa_args
                                ((Expr_name (Var_name Y))
                                  (Expr_name (Var_name D))))))
                          (Expr_name (Var_name D))
                          (Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name L))))))))))
            ((c_lhs
               ((Pattern_match (Lit_integer 1)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _)) (Pattern_binding (Var_name L))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name X))) (Expr_name (Var_name L)))))
            ((c_lhs
               ((Pattern_match (Lit_integer 0)) (Pattern_binding (Var_name _))
                 (Pattern_binding (Var_name _)) (Pattern_binding (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))))
        (fd_spec ())))
    (Module_comment (Comment "% sum(L) returns the sum of the elements in L"))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom number))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom sum))
        (typ_params ((Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom sum)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name L)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom sum))))
                   (fa_args
                     ((Expr_name (Var_name L)) (Expr_literal (Lit_integer 0))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom sum)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name Sum))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom sum))))
                   (fa_args
                     ((Expr_name (Var_name T))
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '+')))))
                           (fa_args
                             ((Expr_name (Var_name Sum))
                               (Expr_name (Var_name H))))))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Sum))))
              (c_guard ()) (c_rhs (Expr_name (Var_name Sum))))))
        (fd_spec ())))
    (Module_comment
      (Comment "% duplicate(N, X) -> [X,X,X,.....,X]  (N times)"))
    (Module_comment (Comment "%   return N copies of X"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List))) (typ_kind Spec)
        (typ_name (Atom duplicate))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name Elem))))))
    (Function_decl
      ((fd_name (Atom duplicate)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom duplicate))))
                   (fa_args
                     ((Expr_name (Var_name N)) (Expr_name (Var_name X))
                       (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom duplicate)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_match (Lit_integer 0)) (Pattern_binding (Var_name _))
                (Pattern_binding (Var_name L))))
             (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name N)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name L))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom duplicate))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name N))
                               (Expr_literal (Lit_integer 1))))))
                        (Expr_name (Var_name X))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name L)))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment "% min(L) -> returns the minimum element of the list L"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Min))) (typ_kind Spec)
        (typ_name (Atom min)) (typ_params ((Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom min)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom min))))
                   (fa_args
                     ((Expr_name (Var_name T)) (Expr_name (Var_name H))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom min)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name Min))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '<')))))
                     (fa_args
                       ((Expr_name (Var_name H)) (Expr_name (Var_name Min)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom min))))
                   (fa_args
                     ((Expr_name (Var_name T)) (Expr_name (Var_name H))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name _)))
                  (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name Min))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom min))))
                    (fa_args
                      ((Expr_name (Var_name T)) (Expr_name (Var_name Min))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Min))))
              (c_guard ()) (c_rhs (Expr_name (Var_name Min))))))
        (fd_spec ())))
    (Module_comment
      (Comment "% max(L) -> returns the maximum element of the list L"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Max))) (typ_kind Spec)
        (typ_name (Atom max)) (typ_params ((Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom max)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom max))))
                   (fa_args
                     ((Expr_name (Var_name T)) (Expr_name (Var_name H))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom max)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name Max))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>')))))
                     (fa_args
                       ((Expr_name (Var_name H)) (Expr_name (Var_name Max)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom max))))
                   (fa_args
                     ((Expr_name (Var_name T)) (Expr_name (Var_name H))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name _)))
                  (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name Max))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom max))))
                    (fa_args
                      ((Expr_name (Var_name T)) (Expr_name (Var_name Max))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Max))))
              (c_guard ()) (c_rhs (Expr_name (Var_name Max))))))
        (fd_spec ())))
    (Module_comment (Comment "% sublist(List, Start, Length)"))
    (Module_comment
      (Comment "%  Returns the sub-list starting at Start of length Length."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom sublist))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name Start))
            (Type_variable (Var_name Len))))))
    (Function_decl
      ((fd_name (Atom sublist)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name List)) (Pattern_binding (Var_name S))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name L))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name L))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom sublist))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom nthtail))))
                          (fa_args
                            ((Expr_apply
                               ((fa_name
                                  (Expr_name
                                    (Qualified_name (n_mod (Atom erlang))
                                      (n_name (Atom '-')))))
                                 (fa_args
                                   ((Expr_name (Var_name S))
                                     (Expr_literal (Lit_integer 1))))))
                              (Expr_name (Var_name List))))))
                       (Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom sublist))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name Len))))))
    (Function_decl
      ((fd_name (Atom sublist)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name List))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name L))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name List)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom sublist_2))))
                   (fa_args
                     ((Expr_name (Var_name List)) (Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom sublist_2)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>')))))
                     (fa_args
                       ((Expr_name (Var_name L))
                         (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_cons ((Expr_name (Var_name H)))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom sublist_2))))
                     (fa_args
                       ((Expr_name (Var_name T))
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '-')))))
                             (fa_args
                               ((Expr_name (Var_name L))
                                 (Expr_literal (Lit_integer 1)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _)) (Pattern_match (Lit_integer 0))))
              (c_guard ()) (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_binding (Var_name List))
                 (Pattern_binding (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name List))))))
                   (Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom erlang))
                            (n_name (Atom '>')))))
                       (fa_args
                         ((Expr_name (Var_name L))
                           (Expr_literal (Lit_integer 0)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment (Comment "% delete(Item, List) -> List'"))
    (Module_comment
      (Comment "%  Delete the first occurrence of Item from the list L."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom delete))
        (typ_params
          ((Type_variable (Var_name Elem)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom delete)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Item))
                (Pattern_cons ((Pattern_binding (Var_name Item)))
                  (Pattern_binding (Var_name Rest)))))
             (c_guard ()) (c_rhs (Expr_name (Var_name Rest))))
            ((c_lhs
               ((Pattern_binding (Var_name Item))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name Rest)))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom delete))))
                      (fa_args
                        ((Expr_name (Var_name Item))
                          (Expr_name (Var_name Rest)))))))))
            ((c_lhs ((Pattern_binding (Var_name _)) (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return [{X0, Y0}, {X1, Y1}, ..., {Xn, Yn}] for lists [X0, X1, ...,"))
    (Module_comment (Comment "% Xn] and [Y0, Y1, ..., Yn]."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom zip))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom zip)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name X)))
                 (Pattern_binding (Var_name Xs)))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name Ys)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_tuple
                    ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom zip))))
                     (fa_args
                       ((Expr_name (Var_name Xs)) (Expr_name (Var_name Ys)))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_list ()))) (c_guard ())
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn]}, for a list [{X0, Y0},"))
    (Module_comment (Comment "% {X1, Y1}, ..., {Xn, Yn}]."))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List2)) (Type_variable (Var_name List3)))))
        (typ_kind Spec) (typ_name (Atom unzip))
        (typ_params ((Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom unzip)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name Ts)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom unzip))))
                   (fa_args
                     ((Expr_name (Var_name Ts)) (Expr_list ()) (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom unzip)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_tuple
                    ((Pattern_binding (Var_name X))
                      (Pattern_binding (Var_name Y)))))
                 (Pattern_binding (Var_name Ts)))
                (Pattern_binding (Var_name Xs))
                (Pattern_binding (Var_name Ys))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom unzip))))
                   (fa_args
                     ((Expr_name (Var_name Ts))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name Xs)))
                       (Expr_cons ((Expr_name (Var_name Y)))
                         (Expr_name (Var_name Ys)))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Xs))
                 (Pattern_binding (Var_name Ys))))
              (c_guard ())
              (c_rhs
                (Expr_tuple
                  ((Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom reverse))))
                       (fa_args ((Expr_name (Var_name Xs))))))
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name (Atom reverse))))
                        (fa_args ((Expr_name (Var_name Ys))))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}] for lists [X0,"))
    (Module_comment
      (Comment "% X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn]."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List4))) (typ_kind Spec)
        (typ_name (Atom zip3))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))
            (Type_variable (Var_name List3))))))
    (Function_decl
      ((fd_name (Atom zip3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name X)))
                 (Pattern_binding (Var_name Xs)))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name Ys)))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name Zs)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_tuple
                    ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                      (Expr_name (Var_name Z)))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom zip3))))
                     (fa_args
                       ((Expr_name (Var_name Xs)) (Expr_name (Var_name Ys))
                         (Expr_name (Var_name Zs)))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_list ()) (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn], [Z0, Z1, ..., Zn]}, for"))
    (Module_comment
      (Comment "% a list [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}]."))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List2)) (Type_variable (Var_name List3))
             (Type_variable (Var_name List4)))))
        (typ_kind Spec) (typ_name (Atom unzip3))
        (typ_params ((Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom unzip3)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name Ts)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom unzip3))))
                   (fa_args
                     ((Expr_name (Var_name Ts)) (Expr_list ()) (Expr_list ())
                       (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom unzip3)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_tuple
                    ((Pattern_binding (Var_name X))
                      (Pattern_binding (Var_name Y))
                      (Pattern_binding (Var_name Z)))))
                 (Pattern_binding (Var_name Ts)))
                (Pattern_binding (Var_name Xs)) (Pattern_binding (Var_name Ys))
                (Pattern_binding (Var_name Zs))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom unzip3))))
                   (fa_args
                     ((Expr_name (Var_name Ts))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name Xs)))
                       (Expr_cons ((Expr_name (Var_name Y)))
                         (Expr_name (Var_name Ys)))
                       (Expr_cons ((Expr_name (Var_name Z)))
                         (Expr_name (Var_name Zs)))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Xs))
                 (Pattern_binding (Var_name Ys))
                 (Pattern_binding (Var_name Zs))))
              (c_guard ())
              (c_rhs
                (Expr_tuple
                  ((Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom reverse))))
                       (fa_args ((Expr_name (Var_name Xs))))))
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name (Atom reverse))))
                        (fa_args ((Expr_name (Var_name Ys))))))
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name (Atom reverse))))
                        (fa_args ((Expr_name (Var_name Zs))))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return [F(X0, Y0), F(X1, Y1), ..., F(Xn, Yn)] for lists [X0, X1, ...,"))
    (Module_comment (Comment "% Xn] and [Y0, Y1, ..., Yn]."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom zipwith))
        (typ_params
          ((Type_variable (Var_name Combine)) (Type_variable (Var_name List1))
            (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom zipwith)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name X)))
                  (Pattern_binding (Var_name Xs)))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name Ys)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_apply
                    ((fa_name (Expr_name (Var_name F)))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom zipwith))))
                     (fa_args
                       ((Expr_name (Var_name F)) (Expr_name (Var_name Xs))
                         (Expr_name (Var_name Ys)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F)) (Pattern_list ())
                 (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% Return [F(X0, Y0, Z0), F(X1, Y1, Z1), ..., F(Xn, Yn, Zn)] for lists"))
    (Module_comment
      (Comment "% [X0, X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn]."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List4))) (typ_kind Spec)
        (typ_name (Atom zipwith3))
        (typ_params
          ((Type_variable (Var_name Combine)) (Type_variable (Var_name List1))
            (Type_variable (Var_name List2)) (Type_variable (Var_name List3))))))
    (Function_decl
      ((fd_name (Atom zipwith3)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name X)))
                  (Pattern_binding (Var_name Xs)))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name Ys)))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name Zs)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_apply
                    ((fa_name (Expr_name (Var_name F)))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                          (Expr_name (Var_name Z)))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom zipwith3))))
                     (fa_args
                       ((Expr_name (Var_name F)) (Expr_name (Var_name Xs))
                         (Expr_name (Var_name Ys)) (Expr_name (Var_name Zs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F)) (Pattern_list ())
                 (Pattern_list ()) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 3)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment (Comment "% sort(List) -> L"))
    (Module_comment (Comment "%  sorts the list L"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom sort)) (typ_params ((Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom sort)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_with_name
                 (Pattern_cons
                   ((Pattern_binding (Var_name X))
                     (Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))
                 (Var_name L0))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name L0))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name Y))
                                 (Expr_name (Var_name Z)))))))))
                     (c_rhs (Expr_name (Var_name L0))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z)))))))))
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name X)) (Expr_name (Var_name Z))
                           (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard ())
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name Z)) (Expr_name (Var_name X))
                           (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_binding (Var_name _))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Y)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom sort_1))))
                           (fa_args
                             ((Expr_name (Var_name Y)) (Expr_name (Var_name L))
                               (Expr_list ((Expr_name (Var_name X))))))))))
                   ((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom split_1))))
                           (fa_args
                             ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_list ())))))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name X))
                    (Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))))
              (c_guard ())
              (c_rhs
                (Expr_case (Expr_name (Var_name L))
                  (((c_lhs ((Pattern_list ()))) (c_guard ())
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name Y)) (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '=<')))))
                              (fa_args
                                ((Expr_name (Var_name X))
                                  (Expr_name (Var_name Z)))))))))
                      (c_rhs
                        (Expr_cons
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                          (Expr_name (Var_name L)))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '=<')))))
                              (fa_args
                                ((Expr_name (Var_name Y))
                                  (Expr_name (Var_name Z)))))))))
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                            (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard ())
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                            (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom split_2))))
                            (fa_args
                              ((Expr_name (Var_name X))
                                (Expr_name (Var_name Y))
                                (Expr_name (Var_name L)) (Expr_list ())
                                (Expr_list ())))))))))))
            ((c_lhs
               ((Pattern_with_name
                  (Pattern_list ((Pattern_binding (Var_name _)))) (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs ((Pattern_with_name (Pattern_list ()) (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom sort_1)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom sort_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X))
                 (Pattern_cons ((Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name X))
                 (Pattern_cons ((Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_list ())
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name R))
                        (Expr_list ((Expr_name (Var_name X))))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge(List) -> L"))
    (Module_comment (Comment "%  merges a list of sorted lists"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List1))) (typ_kind Spec)
        (typ_name (Atom merge))
        (typ_params ((Type_variable (Var_name ListOfLists))))))
    (Function_decl
      ((fd_name (Atom merge)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name L)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom mergel))))
                   (fa_args ((Expr_name (Var_name L)) (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge3(X, Y, Z) -> L"))
    (Module_comment (Comment "%  merges three sorted lists X, Y and Z"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List4))) (typ_kind Spec)
        (typ_name (Atom merge3))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))
            (Type_variable (Var_name List3))))))
    (Function_decl
      ((fd_name (Atom merge3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_list ())
                (Pattern_binding (Var_name L3))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge))))
                    (fa_args
                      ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom merge3_1))))
                           (fa_args
                             ((Expr_name (Var_name L1)) (Expr_list ())
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rmerge3(X, Y, Z) -> L"))
    (Module_comment
      (Comment "%  merges three reversed sorted lists X, Y and Z"))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X))
               (Type_variant
                 ((Type_variable (Var_name Y)) (Type_variable (Var_name Z))))))))
        (typ_kind Spec) (typ_name (Atom rmerge3))
        (typ_params
          ((Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))
            (Type_list (Type_variable (Var_name Z)))))))
    (Function_decl
      ((fd_name (Atom rmerge3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_list ())
                (Pattern_binding (Var_name L3))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge))))
                    (fa_args
                      ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rmerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name L1)) (Expr_list ())
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge(X, Y) -> L"))
    (Module_comment (Comment "%  merges two sorted lists X and Y"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom merge))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom merge)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name T1)) (Pattern_list ())))
             (c_guard ()) (c_rhs (Expr_name (Var_name T1))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2)) (Expr_list ())))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rmerge(X, Y) -> L"))
    (Module_comment (Comment "%  merges two reversed sorted lists X and Y"))
    (Module_comment
      (Comment
        "% reverse(rmerge(reverse(A),reverse(B))) is equal to merge(I,A,B)."))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))))
        (typ_kind Spec) (typ_name (Atom rmerge))
        (typ_params
          ((Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rmerge)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name T1)) (Pattern_list ())))
             (c_guard ()) (c_rhs (Expr_name (Var_name T1))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2)) (Expr_list ())))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% concat(L) concatenate the list representation of the elements"))
    (Module_comment
      (Comment "%  in L - the elements in L can be atoms, numbers of strings."))
    (Module_comment (Comment "%  Returns a list of characters."))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom string))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom concat))
        (typ_params ((Type_variable (Var_name Things))))))
    (Function_decl
      ((fd_name (Atom concat)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name List)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom flatmap))))
                   (fa_args
                     ((Expr_fun_ref (fref_name (Atom thing_to_list))
                        (fref_arity 1))
                       (Expr_name (Var_name List))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom thing_to_list)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name X))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name X)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom integer_to_list))))
                   (fa_args ((Expr_name (Var_name X))))))))
            ((c_lhs ((Pattern_binding (Var_name X))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_float))))
                      (fa_args ((Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom float_to_list))))
                    (fa_args ((Expr_name (Var_name X))))))))
            ((c_lhs ((Pattern_binding (Var_name X))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_atom))))
                      (fa_args ((Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom atom_to_list))))
                    (fa_args ((Expr_name (Var_name X))))))))
            ((c_lhs ((Pattern_binding (Var_name X))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name X)))))))))
              (c_rhs (Expr_name (Var_name X))))))
        (fd_spec ())))
    (Module_comment (Comment "Assumed to be a string"))
    (Module_comment (Comment "% flatten(List)"))
    (Module_comment (Comment "% flatten(List, Tail)"))
    (Module_comment (Comment "%  Flatten a list, adding optional tail."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List))) (typ_kind Spec)
        (typ_name (Atom flatten))
        (typ_params ((Type_variable (Var_name DeepList))))))
    (Function_decl
      ((fd_name (Atom flatten)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name List))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_list))))
                     (fa_args ((Expr_name (Var_name List)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom do_flatten))))
                   (fa_args ((Expr_name (Var_name List)) (Expr_list ())))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List))) (typ_kind Spec)
        (typ_name (Atom flatten))
        (typ_params
          ((Type_variable (Var_name DeepList)) (Type_variable (Var_name Tail))))))
    (Function_decl
      ((fd_name (Atom flatten)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name List))
                (Pattern_binding (Var_name Tail))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_list))))
                     (fa_args ((Expr_name (Var_name List))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name Tail)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom do_flatten))))
                   (fa_args
                     ((Expr_name (Var_name List)) (Expr_name (Var_name Tail))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom do_flatten)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name Tail))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_list))))
                     (fa_args ((Expr_name (Var_name H)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom do_flatten))))
                   (fa_args
                     ((Expr_name (Var_name H))
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom do_flatten))))
                           (fa_args
                             ((Expr_name (Var_name T))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name Tail))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom do_flatten))))
                      (fa_args
                        ((Expr_name (Var_name T)) (Expr_name (Var_name Tail)))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Tail))))
              (c_guard ()) (c_rhs (Expr_name (Var_name Tail))))))
        (fd_spec ())))
    (Module_comment (Comment "% flatlength(List)"))
    (Module_comment (Comment "%  Calculate the length of a list of lists."))
    (Type_decl
      ((typ_expr
         (Type_constr
           ((tc_name (Atom_name (Atom non_neg_integer))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom flatlength))
        (typ_params ((Type_variable (Var_name DeepList))))))
    (Function_decl
      ((fd_name (Atom flatlength)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name List)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom flatlength))))
                   (fa_args
                     ((Expr_name (Var_name List))
                       (Expr_literal (Lit_integer 0))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom flatlength)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H)))
                 (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_list))))
                     (fa_args ((Expr_name (Var_name H)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom flatlength))))
                   (fa_args
                     ((Expr_name (Var_name H))
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom flatlength))))
                           (fa_args
                             ((Expr_name (Var_name T))
                               (Expr_name (Var_name L))))))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name _)))
                  (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name L))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom flatlength))))
                    (fa_args
                      ((Expr_name (Var_name T))
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '+')))))
                            (fa_args
                              ((Expr_name (Var_name L))
                                (Expr_literal (Lit_integer 1))))))))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))))
        (fd_spec ())))
    (Module_comment (Comment "% keymember(Key, Index, [Tuple]) Now a BIF!"))
    (Module_comment (Comment "% keyfind(Key, Index, [Tuple]) A BIF!"))
    (Module_comment (Comment "% keysearch(Key, Index, [Tuple]) Now a BIF!"))
    (Module_comment (Comment "% keydelete(Key, Index, [Tuple])"))
    (Module_comment (Comment "% keyreplace(Key, Index, [Tuple], NewTuple)"))
    (Module_comment (Comment "% keytake(Key, Index, [Tuple])"))
    (Module_comment (Comment "% keystore(Key, Index, [Tuple], NewTuple)"))
    (Module_comment (Comment "% keysort(Index, [Tuple])"))
    (Module_comment (Comment "% keymerge(Index, [Tuple], [Tuple])"))
    (Module_comment (Comment "% ukeysort(Index, [Tuple])"))
    (Module_comment (Comment "% ukeymerge(Index, [Tuple], [Tuple])"))
    (Module_comment (Comment "% keymap(Function, Index, [Tuple])"))
    (Module_comment (Comment "% keymap(Function, ExtraArgs, Index, [Tuple])"))
    (Module_comment (Comment "keymember(K,N,L) when is_integer(N), N > 0 ->"))
    (Module_comment (Comment "    keymember3(K,N,L)."))
    (Module_comment
      (Comment "keymember3(Key, N, [T|Ts]) when element(N, T) == Key -> true;"))
    (Module_comment (Comment "keymember3(Key, N, [T|Ts]) ->"))
    (Module_comment (Comment "    keymember3(Key, N, Ts);"))
    (Module_comment (Comment "keymember3(Key, N, []) -> false."))
    (Module_comment
      (Comment "keysearch(K, N, L) when is_integer(N), N > 0 ->"))
    (Module_comment (Comment "    keysearch3(K, N, L)."))
    (Module_comment
      (Comment "keysearch3(Key, N, [H|T]) when element(N, H) == Key ->"))
    (Module_comment (Comment "    {value, H};"))
    (Module_comment (Comment "keysearch3(Key, N, [H|T]) ->"))
    (Module_comment (Comment "    keysearch3(Key, N, T);"))
    (Module_comment (Comment "keysearch3(Key, N, []) -> false."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom keydelete))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList1))))))
    (Function_decl
      ((fd_name (Atom keydelete)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name K)) (Pattern_binding (Var_name N))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keydelete3))))
                   (fa_args
                     ((Expr_name (Var_name K)) (Expr_name (Var_name N))
                       (Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keydelete3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom element))))
                            (fa_args
                              ((Expr_name (Var_name N))
                                (Expr_name (Var_name H))))))
                         (Expr_name (Var_name Key)))))))))
             (c_rhs (Expr_name (Var_name T))))
            ((c_lhs
               ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom keydelete3))))
                      (fa_args
                        ((Expr_name (Var_name Key)) (Expr_name (Var_name N))
                          (Expr_name (Var_name T)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                 (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom keyreplace))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList1))
            (Type_variable (Var_name NewTuple))))))
    (Function_decl
      ((fd_name (Atom keyreplace)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name K)) (Pattern_binding (Var_name N))
                (Pattern_binding (Var_name L))
                (Pattern_binding (Var_name New))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_tuple))))
                      (fa_args ((Expr_name (Var_name New)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keyreplace3))))
                   (fa_args
                     ((Expr_name (Var_name K)) (Expr_name (Var_name N))
                       (Expr_name (Var_name L)) (Expr_name (Var_name New))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keyreplace3)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Key))
                (Pattern_binding (Var_name Pos))
                (Pattern_cons ((Pattern_binding (Var_name Tup)))
                  (Pattern_binding (Var_name Tail)))
                (Pattern_binding (Var_name New))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom element))))
                            (fa_args
                              ((Expr_name (Var_name Pos))
                                (Expr_name (Var_name Tup))))))
                         (Expr_name (Var_name Key)))))))))
             (c_rhs
               (Expr_cons ((Expr_name (Var_name New)))
                 (Expr_name (Var_name Tail)))))
            ((c_lhs
               ((Pattern_binding (Var_name Key))
                 (Pattern_binding (Var_name Pos))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name New))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom keyreplace3))))
                      (fa_args
                        ((Expr_name (Var_name Key)) (Expr_name (Var_name Pos))
                          (Expr_name (Var_name T)) (Expr_name (Var_name New)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _)) (Pattern_binding (Var_name _))
                 (Pattern_list ()) (Pattern_binding (Var_name _))))
              (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_tuple
              ((Type_const (Lit_atom (Atom value)))
                (Type_variable (Var_name Tuple))
                (Type_variable (Var_name TupleList2))))
             (Type_const (Lit_atom (Atom false))))))
        (typ_kind Spec) (typ_name (Atom keytake))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList1))))))
    (Function_decl
      ((fd_name (Atom keytake)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keytake))))
                   (fa_args
                     ((Expr_name (Var_name Key)) (Expr_name (Var_name N))
                       (Expr_name (Var_name L)) (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keytake)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom element))))
                            (fa_args
                              ((Expr_name (Var_name N))
                                (Expr_name (Var_name H))))))
                         (Expr_name (Var_name Key)))))))))
             (c_rhs
               (Expr_tuple
                 ((Expr_literal (Lit_atom (Atom value)))
                   (Expr_name (Var_name H))
                   (Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom lists))
                            (n_name (Atom reverse)))))
                       (fa_args
                         ((Expr_name (Var_name L)) (Expr_name (Var_name T))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name L))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keytake))))
                    (fa_args
                      ((Expr_name (Var_name Key)) (Expr_name (Var_name N))
                        (Expr_name (Var_name T))
                        (Expr_cons ((Expr_name (Var_name H)))
                          (Expr_name (Var_name L)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _K)) (Pattern_binding (Var_name _N))
                 (Pattern_list ()) (Pattern_binding (Var_name _L))))
              (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom false)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom keystore))
        (typ_params
          ((Type_variable (Var_name Key)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList1))
            (Type_variable (Var_name NewTuple))))))
    (Function_decl
      ((fd_name (Atom keystore)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name K)) (Pattern_binding (Var_name N))
                (Pattern_binding (Var_name L))
                (Pattern_binding (Var_name New))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_tuple))))
                      (fa_args ((Expr_name (Var_name New)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keystore2))))
                   (fa_args
                     ((Expr_name (Var_name K)) (Expr_name (Var_name N))
                       (Expr_name (Var_name L)) (Expr_name (Var_name New))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keystore2)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name New))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom element))))
                            (fa_args
                              ((Expr_name (Var_name N))
                                (Expr_name (Var_name H))))))
                         (Expr_name (Var_name Key)))))))))
             (c_rhs
               (Expr_cons ((Expr_name (Var_name New)))
                 (Expr_name (Var_name T)))))
            ((c_lhs
               ((Pattern_binding (Var_name Key)) (Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name New))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom keystore2))))
                      (fa_args
                        ((Expr_name (Var_name Key)) (Expr_name (Var_name N))
                          (Expr_name (Var_name T)) (Expr_name (Var_name New)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _Key))
                 (Pattern_binding (Var_name _N)) (Pattern_list ())
                 (Pattern_binding (Var_name New))))
              (c_guard ()) (c_rhs (Expr_list ((Expr_name (Var_name New))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom keysort))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name TupleList1))))))
    (Function_decl
      ((fd_name (Atom keysort)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name I))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name I))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name L))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name _))))))
                     (c_guard ()) (c_rhs (Expr_name (Var_name L))))
                   ((c_lhs
                      ((Pattern_cons
                         ((Pattern_binding (Var_name X))
                           (Pattern_binding (Var_name Y)))
                         (Pattern_binding (Var_name T)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_tuple
                           ((Expr_apply
                              ((fa_name (Expr_name (Atom_name (Atom element))))
                                (fa_args
                                  ((Expr_name (Var_name I))
                                    (Expr_name (Var_name X))))))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name Y))))))))
                         (((c_lhs
                             ((Pattern_tuple
                                ((Pattern_binding (Var_name EX))
                                  (Pattern_binding (Var_name EY))))))
                            (c_guard
                              (((Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod (Atom erlang))
                                         (n_name (Atom '=<')))))
                                    (fa_args
                                      ((Expr_name (Var_name EX))
                                        (Expr_name (Var_name EY)))))))))
                            (c_rhs
                              (Expr_case (Expr_name (Var_name T))
                                (((c_lhs ((Pattern_list ()))) (c_guard ())
                                   (c_rhs (Expr_name (Var_name L))))
                                  ((c_lhs
                                     ((Pattern_list
                                        ((Pattern_binding (Var_name Z))))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_case
                                        (Expr_apply
                                          ((fa_name
                                             (Expr_name
                                               (Atom_name (Atom element))))
                                            (fa_args
                                              ((Expr_name (Var_name I))
                                                (Expr_name (Var_name Z))))))
                                        (((c_lhs
                                            ((Pattern_binding (Var_name EZ))))
                                           (c_guard
                                             (((Expr_apply
                                                 ((fa_name
                                                    (Expr_name
                                                      (Qualified_name
                                                        (n_mod (Atom erlang))
                                                        (n_name (Atom '=<')))))
                                                   (fa_args
                                                     ((Expr_name (Var_name EY))
                                                       (Expr_name
                                                         (Var_name EZ)))))))))
                                           (c_rhs (Expr_name (Var_name L))))
                                          ((c_lhs
                                             ((Pattern_binding (Var_name EZ))))
                                            (c_guard
                                              (((Expr_apply
                                                  ((fa_name
                                                     (Expr_name
                                                       (Qualified_name
                                                         (n_mod (Atom erlang))
                                                         (n_name (Atom '=<')))))
                                                    (fa_args
                                                      ((Expr_name
                                                         (Var_name EX))
                                                        (Expr_name
                                                          (Var_name EZ)))))))))
                                            (c_rhs
                                              (Expr_list
                                                ((Expr_name (Var_name X))
                                                  (Expr_name (Var_name Z))
                                                  (Expr_name (Var_name Y))))))
                                          ((c_lhs
                                             ((Pattern_binding (Var_name _EZ))))
                                            (c_guard ())
                                            (c_rhs
                                              (Expr_list
                                                ((Expr_name (Var_name Z))
                                                  (Expr_name (Var_name X))
                                                  (Expr_name (Var_name Y))))))))))
                                  ((c_lhs ((Pattern_binding (Var_name _))))
                                    (c_guard
                                      (((Expr_apply
                                          ((fa_name
                                             (Expr_name
                                               (Qualified_name
                                                 (n_mod (Atom erlang))
                                                 (n_name (Atom '==')))))
                                            (fa_args
                                              ((Expr_name (Var_name X))
                                                (Expr_name (Var_name Y)))))))))
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom keysort_1))))
                                          (fa_args
                                            ((Expr_name (Var_name I))
                                              (Expr_name (Var_name Y))
                                              (Expr_name (Var_name EY))
                                              (Expr_name (Var_name T))
                                              (Expr_list
                                                ((Expr_name (Var_name X))))))))))
                                  ((c_lhs ((Pattern_binding (Var_name _))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom keysplit_1))))
                                          (fa_args
                                            ((Expr_name (Var_name I))
                                              (Expr_name (Var_name X))
                                              (Expr_name (Var_name EX))
                                              (Expr_name (Var_name Y))
                                              (Expr_name (Var_name EY))
                                              (Expr_name (Var_name T))
                                              (Expr_list ()) (Expr_list ())))))))))))
                           ((c_lhs
                              ((Pattern_tuple
                                 ((Pattern_binding (Var_name EX))
                                   (Pattern_binding (Var_name EY))))))
                             (c_guard ())
                             (c_rhs
                               (Expr_case (Expr_name (Var_name T))
                                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                                    (c_rhs
                                      (Expr_list
                                        ((Expr_name (Var_name Y))
                                          (Expr_name (Var_name X))))))
                                   ((c_lhs
                                      ((Pattern_list
                                         ((Pattern_binding (Var_name Z))))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_case
                                         (Expr_apply
                                           ((fa_name
                                              (Expr_name
                                                (Atom_name (Atom element))))
                                             (fa_args
                                               ((Expr_name (Var_name I))
                                                 (Expr_name (Var_name Z))))))
                                         (((c_lhs
                                             ((Pattern_binding (Var_name EZ))))
                                            (c_guard
                                              (((Expr_apply
                                                  ((fa_name
                                                     (Expr_name
                                                       (Qualified_name
                                                         (n_mod (Atom erlang))
                                                         (n_name (Atom '=<')))))
                                                    (fa_args
                                                      ((Expr_name
                                                         (Var_name EX))
                                                        (Expr_name
                                                          (Var_name EZ)))))))))
                                            (c_rhs
                                              (Expr_cons
                                                ((Expr_name (Var_name Y))
                                                  (Expr_name (Var_name X)))
                                                (Expr_name (Var_name T)))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '=<')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EY))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Y))
                                                   (Expr_name (Var_name Z))
                                                   (Expr_name (Var_name X))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name _EZ))))
                                             (c_guard ())
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Z))
                                                   (Expr_name (Var_name Y))
                                                   (Expr_name (Var_name X))))))))))
                                   ((c_lhs ((Pattern_binding (Var_name _))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom keysplit_2))))
                                           (fa_args
                                             ((Expr_name (Var_name I))
                                               (Expr_name (Var_name X))
                                               (Expr_name (Var_name EX))
                                               (Expr_name (Var_name Y))
                                               (Expr_name (Var_name EY))
                                               (Expr_name (Var_name T))
                                               (Expr_list ()) (Expr_list ())))))))))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keysort_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keysort_1))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                       (Expr_name (Var_name EX)) (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name EX))
                 (Pattern_cons ((Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_case
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom element))))
                      (fa_args
                        ((Expr_name (Var_name I)) (Expr_name (Var_name Y))))))
                  (((c_lhs ((Pattern_binding (Var_name EY))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EY)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_name (Var_name R)) (Expr_list ())))))))
                    ((c_lhs ((Pattern_binding (Var_name EY)))) (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                            (fa_args
                              ((Expr_name (Var_name I))
                                (Expr_name (Var_name X))
                                (Expr_name (Var_name EX))
                                (Expr_name (Var_name Y))
                                (Expr_name (Var_name EY))
                                (Expr_name (Var_name L))
                                (Expr_name (Var_name R)) (Expr_list ())))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX)) (Pattern_list ())
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name R))
                        (Expr_list ((Expr_name (Var_name X))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList3))) (typ_kind Spec)
        (typ_name (Atom keymerge))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name TupleList1))
            (Type_variable (Var_name TupleList2))))))
    (Function_decl
      ((fd_name (Atom keymerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Index))
                (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name L2))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name Index))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Index))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L2))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name T1))))
                   ((c_lhs
                      ((Pattern_cons ((Pattern_binding (Var_name H2)))
                         (Pattern_binding (Var_name T2)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_let
                         ((lb_lhs (Pattern_binding (Var_name E2)))
                           (lb_rhs
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name Index))
                                     (Expr_name (Var_name H2))))))))
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name M)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom keymerge2_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Index))
                                       (Expr_name (Var_name T1))
                                       (Expr_name (Var_name E2))
                                       (Expr_name (Var_name H2))
                                       (Expr_name (Var_name T2))
                                       (Expr_list ())))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name M)) (Expr_list ())))))))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% reverse(rkeymerge(I,reverse(A),reverse(B))) is equal to keymerge(I,A,B)."))
    (Type_decl
      ((typ_expr (Type_list (Type_variable (Var_name R)))) (typ_kind Spec)
        (typ_name (Atom rkeymerge))
        (typ_params
          ((Type_constr
             ((tc_name (Atom_name (Atom pos_integer))) (tc_args ())))
            (Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rkeymerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Index))
                (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name L2))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name Index))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Index))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L2))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name T1))))
                   ((c_lhs
                      ((Pattern_cons ((Pattern_binding (Var_name H2)))
                         (Pattern_binding (Var_name T2)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_let
                         ((lb_lhs (Pattern_binding (Var_name E2)))
                           (lb_rhs
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name Index))
                                     (Expr_name (Var_name H2))))))))
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name M)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom rkeymerge2_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Index))
                                       (Expr_name (Var_name T1))
                                       (Expr_name (Var_name E2))
                                       (Expr_name (Var_name H2))
                                       (Expr_name (Var_name T2))
                                       (Expr_list ())))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name M)) (Expr_list ())))))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom ukeysort))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name TupleList1))))))
    (Function_decl
      ((fd_name (Atom ukeysort)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name I))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name I))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name L))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name _))))))
                     (c_guard ()) (c_rhs (Expr_name (Var_name L))))
                   ((c_lhs
                      ((Pattern_cons
                         ((Pattern_binding (Var_name X))
                           (Pattern_binding (Var_name Y)))
                         (Pattern_binding (Var_name T)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_tuple
                           ((Expr_apply
                              ((fa_name (Expr_name (Atom_name (Atom element))))
                                (fa_args
                                  ((Expr_name (Var_name I))
                                    (Expr_name (Var_name X))))))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name Y))))))))
                         (((c_lhs
                             ((Pattern_tuple
                                ((Pattern_binding (Var_name EX))
                                  (Pattern_binding (Var_name EY))))))
                            (c_guard
                              (((Expr_apply
                                  ((fa_name
                                     (Expr_name
                                       (Qualified_name (n_mod (Atom erlang))
                                         (n_name (Atom '==')))))
                                    (fa_args
                                      ((Expr_name (Var_name EX))
                                        (Expr_name (Var_name EY)))))))))
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom ukeysort_1))))
                                  (fa_args
                                    ((Expr_name (Var_name I))
                                      (Expr_name (Var_name X))
                                      (Expr_name (Var_name EX))
                                      (Expr_name (Var_name T))))))))
                           ((c_lhs
                              ((Pattern_tuple
                                 ((Pattern_binding (Var_name EX))
                                   (Pattern_binding (Var_name EY))))))
                             (c_guard
                               (((Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom erlang))
                                          (n_name (Atom '<')))))
                                     (fa_args
                                       ((Expr_name (Var_name EX))
                                         (Expr_name (Var_name EY)))))))))
                             (c_rhs
                               (Expr_case (Expr_name (Var_name T))
                                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                                    (c_rhs (Expr_name (Var_name L))))
                                   ((c_lhs
                                      ((Pattern_list
                                         ((Pattern_binding (Var_name Z))))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_case
                                         (Expr_apply
                                           ((fa_name
                                              (Expr_name
                                                (Atom_name (Atom element))))
                                             (fa_args
                                               ((Expr_name (Var_name I))
                                                 (Expr_name (Var_name Z))))))
                                         (((c_lhs
                                             ((Pattern_binding (Var_name EZ))))
                                            (c_guard
                                              (((Expr_apply
                                                  ((fa_name
                                                     (Expr_name
                                                       (Qualified_name
                                                         (n_mod (Atom erlang))
                                                         (n_name (Atom '==')))))
                                                    (fa_args
                                                      ((Expr_name
                                                         (Var_name EY))
                                                        (Expr_name
                                                          (Var_name EZ)))))))))
                                            (c_rhs
                                              (Expr_list
                                                ((Expr_name (Var_name X))
                                                  (Expr_name (Var_name Y))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '<')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EY))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name X))
                                                   (Expr_name (Var_name Y))
                                                   (Expr_name (Var_name Z))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '==')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EZ))
                                                         (Expr_name
                                                           (Var_name EX)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name X))
                                                   (Expr_name (Var_name Y))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '=<')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EX))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name X))
                                                   (Expr_name (Var_name Z))
                                                   (Expr_name (Var_name Y))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name _EZ))))
                                             (c_guard ())
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Z))
                                                   (Expr_name (Var_name X))
                                                   (Expr_name (Var_name Y))))))))))
                                   ((c_lhs ((Pattern_binding (Var_name _))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom ukeysplit_1))))
                                           (fa_args
                                             ((Expr_name (Var_name I))
                                               (Expr_name (Var_name X))
                                               (Expr_name (Var_name EX))
                                               (Expr_name (Var_name Y))
                                               (Expr_name (Var_name EY))
                                               (Expr_name (Var_name T))
                                               (Expr_list ()) (Expr_list ())))))))))))
                           ((c_lhs
                              ((Pattern_tuple
                                 ((Pattern_binding (Var_name EX))
                                   (Pattern_binding (Var_name EY))))))
                             (c_guard ())
                             (c_rhs
                               (Expr_case (Expr_name (Var_name T))
                                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                                    (c_rhs
                                      (Expr_list
                                        ((Expr_name (Var_name Y))
                                          (Expr_name (Var_name X))))))
                                   ((c_lhs
                                      ((Pattern_list
                                         ((Pattern_binding (Var_name Z))))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_case
                                         (Expr_apply
                                           ((fa_name
                                              (Expr_name
                                                (Atom_name (Atom element))))
                                             (fa_args
                                               ((Expr_name (Var_name I))
                                                 (Expr_name (Var_name Z))))))
                                         (((c_lhs
                                             ((Pattern_binding (Var_name EZ))))
                                            (c_guard
                                              (((Expr_apply
                                                  ((fa_name
                                                     (Expr_name
                                                       (Qualified_name
                                                         (n_mod (Atom erlang))
                                                         (n_name (Atom '==')))))
                                                    (fa_args
                                                      ((Expr_name
                                                         (Var_name EX))
                                                        (Expr_name
                                                          (Var_name EZ)))))))))
                                            (c_rhs
                                              (Expr_list
                                                ((Expr_name (Var_name Y))
                                                  (Expr_name (Var_name X))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '<')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EX))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Y))
                                                   (Expr_name (Var_name X))
                                                   (Expr_name (Var_name Z))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '==')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EY))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Y))
                                                   (Expr_name (Var_name X))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name EZ))))
                                             (c_guard
                                               (((Expr_apply
                                                   ((fa_name
                                                      (Expr_name
                                                        (Qualified_name
                                                          (n_mod (Atom erlang))
                                                          (n_name (Atom '=<')))))
                                                     (fa_args
                                                       ((Expr_name
                                                          (Var_name EY))
                                                         (Expr_name
                                                           (Var_name EZ)))))))))
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Y))
                                                   (Expr_name (Var_name Z))
                                                   (Expr_name (Var_name X))))))
                                           ((c_lhs
                                              ((Pattern_binding (Var_name _EZ))))
                                             (c_guard ())
                                             (c_rhs
                                               (Expr_list
                                                 ((Expr_name (Var_name Z))
                                                   (Expr_name (Var_name Y))
                                                   (Expr_name (Var_name X))))))))))
                                   ((c_lhs ((Pattern_binding (Var_name _))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom ukeysplit_2))))
                                           (fa_args
                                             ((Expr_name (Var_name I))
                                               (Expr_name (Var_name Y))
                                               (Expr_name (Var_name EY))
                                               (Expr_name (Var_name T))
                                               (Expr_list
                                                 ((Expr_name (Var_name X))))))))))))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom ukeysort_1)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Y))))))
                 (((c_lhs ((Pattern_binding (Var_name EY))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '==')))))
                            (fa_args
                              ((Expr_name (Var_name EX))
                                (Expr_name (Var_name EY)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeysort_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                              (Expr_name (Var_name EX))
                              (Expr_name (Var_name L))))))))
                   ((c_lhs ((Pattern_binding (Var_name EY))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EY)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_list ())))))))
                   ((c_lhs ((Pattern_binding (Var_name EY)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_list ((Expr_name (Var_name X))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX)) (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_list ((Expr_name (Var_name X))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList3))) (typ_kind Spec)
        (typ_name (Atom ukeymerge))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name TupleList1))
            (Type_variable (Var_name TupleList2))))))
    (Function_decl
      ((fd_name (Atom ukeymerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Index))
                (Pattern_binding (Var_name L1))
                (Pattern_binding (Var_name T2))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name Index))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Index))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L1))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name T2))))
                   ((c_lhs
                      ((Pattern_cons ((Pattern_binding (Var_name H1)))
                         (Pattern_binding (Var_name T1)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_let
                         ((lb_lhs (Pattern_binding (Var_name E1)))
                           (lb_rhs
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name Index))
                                     (Expr_name (Var_name H1))))))))
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name M)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ukeymerge2_2))))
                                   (fa_args
                                     ((Expr_name (Var_name Index))
                                       (Expr_name (Var_name T1))
                                       (Expr_name (Var_name E1))
                                       (Expr_name (Var_name H1))
                                       (Expr_name (Var_name T2))
                                       (Expr_list ())))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name M)) (Expr_list ())))))))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% reverse(rukeymerge(I,reverse(A),reverse(B))) is equal to ukeymerge(I,A,B)."))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))))
        (typ_kind Spec) (typ_name (Atom rukeymerge))
        (typ_params
          ((Type_constr
             ((tc_name (Atom_name (Atom pos_integer))) (tc_args ())))
            (Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rukeymerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Index))
                (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name L2))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name Index))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Index))
                          (Expr_literal (Lit_integer 0)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L2))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name T1))))
                   ((c_lhs
                      ((Pattern_cons ((Pattern_binding (Var_name H2)))
                         (Pattern_binding (Var_name T2)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_let
                         ((lb_lhs (Pattern_binding (Var_name E2)))
                           (lb_rhs
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name Index))
                                     (Expr_name (Var_name H2))))))))
                         (Expr_let
                           ((lb_lhs (Pattern_binding (Var_name M)))
                             (lb_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name
                                      (Atom_name (Atom rukeymerge2_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Index))
                                       (Expr_name (Var_name T1))
                                       (Expr_name (Var_name E2))
                                       (Expr_name (Var_name T2)) (Expr_list ())
                                       (Expr_name (Var_name H2))))))))
                           (Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name M)) (Expr_list ())))))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name TupleList2))) (typ_kind Spec)
        (typ_name (Atom keymap))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name N))
            (Type_variable (Var_name TupleList1))))))
    (Function_decl
      ((fd_name (Atom keymap)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name Index))
                (Pattern_cons ((Pattern_binding (Var_name Tup)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom setelement))))
                      (fa_args
                        ((Expr_name (Var_name Index))
                          (Expr_name (Var_name Tup))
                          (Expr_apply
                            ((fa_name (Expr_name (Var_name Fun)))
                              (fa_args
                                ((Expr_apply
                                   ((fa_name
                                      (Expr_name (Atom_name (Atom element))))
                                     (fa_args
                                       ((Expr_name (Var_name Index))
                                         (Expr_name (Var_name Tup)))))))))))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom keymap))))
                     (fa_args
                       ((Expr_name (Var_name Fun)) (Expr_name (Var_name Index))
                         (Expr_name (Var_name Tail)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name Index)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Index))))))
                   (Expr_apply
                     ((fa_name
                        (Expr_name
                          (Qualified_name (n_mod (Atom erlang))
                            (n_name (Atom '>=')))))
                       (fa_args
                         ((Expr_name (Var_name Index))
                           (Expr_literal (Lit_integer 1))))))
                   (Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom is_function))))
                       (fa_args
                         ((Expr_name (Var_name Fun))
                           (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Module_comment
      (Comment "%% Suggestion from OTP-2948: sort and merge with Fun."))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom sort))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom sort)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name Fun)) (Pattern_list ())))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_with_name
                   (Pattern_list ((Pattern_binding (Var_name _))))
                   (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_cons
                   ((Pattern_binding (Var_name X))
                     (Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_case
                  (Expr_apply
                    ((fa_name (Expr_name (Var_name Fun)))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y))))))
                  (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom fsplit_1))))
                           (fa_args
                             ((Expr_name (Var_name Y)) (Expr_name (Var_name X))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T)) (Expr_list ())
                               (Expr_list ())))))))
                    ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                      (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom fsplit_2))))
                            (fa_args
                              ((Expr_name (Var_name Y))
                                (Expr_name (Var_name X))
                                (Expr_name (Var_name Fun))
                                (Expr_name (Var_name T)) (Expr_list ())
                                (Expr_list ())))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom merge))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))
            (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom merge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun)) (Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom lists))
                        (n_name (Atom reverse)))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2)) (Expr_list ())))))
                       (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name T1))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% reverse(rmerge(F,reverse(A),reverse(B))) is equal to merge(F,A,B)."))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))))
        (typ_kind Spec) (typ_name (Atom rmerge))
        (typ_params
          ((Type_function
             (tyfun_args
               ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))
             (tyfun_return
               (Type_constr
                 ((tc_name (Atom_name (Atom boolean))) (tc_args ())))))
            (Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rmerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun)) (Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom lists))
                        (n_name (Atom reverse)))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rfmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2)) (Expr_list ())))))
                       (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name T1))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom usort))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom usort)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun))
                (Pattern_with_name
                  (Pattern_list ((Pattern_binding (Var_name _)))) (Var_name L))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_with_name (Pattern_list ()) (Var_name L))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_cons ((Pattern_binding (Var_name X)))
                   (Pattern_binding (Var_name L)))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usort_1))))
                    (fa_args
                      ((Expr_name (Var_name Fun)) (Expr_name (Var_name X))
                        (Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom usort_1)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun)) (Pattern_binding (Var_name X))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_case
                        (Expr_apply
                          ((fa_name (Expr_name (Var_name Fun)))
                            (fa_args
                              ((Expr_name (Var_name Y))
                                (Expr_name (Var_name X))))))
                        (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                           (c_guard ())
                           (c_rhs
                             (Expr_comment (Comment " X equal to Y")
                               (Expr_case (Expr_name (Var_name L))
                                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                                    (c_rhs
                                      (Expr_list ((Expr_name (Var_name X))))))
                                   ((c_lhs ((Pattern_binding (Var_name _))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom usort_1))))
                                           (fa_args
                                             ((Expr_name (Var_name Fun))
                                               (Expr_name (Var_name X))
                                               (Expr_name (Var_name L)))))))))))))
                          ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom ufsplit_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L)) (Expr_list ())
                                      (Expr_list ())))))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ufsplit_2))))
                           (fa_args
                             ((Expr_name (Var_name Y)) (Expr_name (Var_name L))
                               (Expr_name (Var_name Fun))
                               (Expr_list ((Expr_name (Var_name X))))))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom umerge))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))
            (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom umerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun)) (Pattern_list ())
                (Pattern_binding (Var_name T2))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs (Expr_name (Var_name T2))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_cons ((Pattern_binding (Var_name H1)))
                   (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ufmerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2)) (Expr_list ())))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "% reverse(rumerge(F,reverse(A),reverse(B))) is equal to umerge(F,A,B)."))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))))
        (typ_kind Spec) (typ_name (Atom rumerge))
        (typ_params
          ((Type_function
             (tyfun_args
               ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))
             (tyfun_return
               (Type_constr
                 ((tc_name (Atom_name (Atom boolean))) (tc_args ())))))
            (Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rumerge)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Fun)) (Pattern_binding (Var_name T1))
                (Pattern_list ())))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Fun))
                         (Expr_literal (Lit_integer 2)))))))))
             (c_rhs (Expr_name (Var_name T1))))
            ((c_lhs
               ((Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Fun))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rufmerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2)) (Expr_list ())))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% usort(List) -> L"))
    (Module_comment (Comment "%  sorts the list L, removes duplicates"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom usort))
        (typ_params ((Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom usort)) (fd_arity 1)
        (fd_cases
          (((c_lhs
              ((Pattern_with_name
                 (Pattern_cons
                   ((Pattern_binding (Var_name X))
                     (Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))
                 (Var_name L0))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '<')))))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_case (Expr_name (Var_name L))
                 (((c_lhs ((Pattern_list ()))) (c_guard ())
                    (c_rhs (Expr_name (Var_name L0))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name Y))
                                 (Expr_name (Var_name Z)))))))))
                     (c_rhs (Expr_name (Var_name L0))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name Y))
                                 (Expr_name (Var_name Z)))))))))
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name X)) (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name Z))
                                 (Expr_name (Var_name X)))))))))
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name Z)) (Expr_name (Var_name X))
                           (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name Z))
                                 (Expr_name (Var_name X)))))))))
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name X)) (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                     (c_guard ())
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name X)) (Expr_name (Var_name Z))
                           (Expr_name (Var_name Y))))))
                   ((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                           (fa_args
                             ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_list ())))))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name X))
                    (Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_case (Expr_name (Var_name L))
                  (((c_lhs ((Pattern_list ()))) (c_guard ())
                     (c_rhs
                       (Expr_list
                         ((Expr_name (Var_name Y)) (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '<')))))
                              (fa_args
                                ((Expr_name (Var_name X))
                                  (Expr_name (Var_name Z)))))))))
                      (c_rhs
                        (Expr_cons
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                          (Expr_name (Var_name L)))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '==')))))
                              (fa_args
                                ((Expr_name (Var_name X))
                                  (Expr_name (Var_name Z)))))))))
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '<')))))
                              (fa_args
                                ((Expr_name (Var_name Z))
                                  (Expr_name (Var_name Y)))))))))
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                            (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard
                        (((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom erlang))
                                   (n_name (Atom '==')))))
                              (fa_args
                                ((Expr_name (Var_name Z))
                                  (Expr_name (Var_name Y)))))))))
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_list ((Pattern_binding (Var_name Z))))))
                      (c_guard ())
                      (c_rhs
                        (Expr_list
                          ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                            (Expr_name (Var_name X))))))
                    ((c_lhs ((Pattern_binding (Var_name _)))) (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                            (fa_args
                              ((Expr_name (Var_name X))
                                (Expr_name (Var_name Y))
                                (Expr_name (Var_name L)) (Expr_list ())
                                (Expr_list ())))))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name X))
                    (Pattern_binding (Var_name _Y)))
                  (Pattern_binding (Var_name L)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usort_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name L))))))))
            ((c_lhs
               ((Pattern_with_name
                  (Pattern_list ((Pattern_binding (Var_name _)))) (Var_name L))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs ((Pattern_list ()))) (c_guard ()) (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom usort_1)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X))
                (Pattern_cons ((Pattern_binding (Var_name Y)))
                  (Pattern_binding (Var_name L)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom usort_1))))
                   (fa_args
                     ((Expr_name (Var_name X)) (Expr_name (Var_name L))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X))
                 (Pattern_cons ((Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name X)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_list ()) (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name X))
                 (Pattern_cons ((Pattern_binding (Var_name Y)))
                   (Pattern_binding (Var_name L)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_list ()) (Expr_list ())))))))
            ((c_lhs ((Pattern_binding (Var_name X)) (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_list ((Expr_name (Var_name X))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge(List) -> L"))
    (Module_comment
      (Comment
        "%  merges a list of sorted lists without duplicates, removes duplicates"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List1))) (typ_kind Spec)
        (typ_name (Atom umerge))
        (typ_params ((Type_variable (Var_name ListOfLists))))))
    (Function_decl
      ((fd_name (Atom umerge)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name L)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umergel))))
                   (fa_args ((Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge3(X, Y, Z) -> L"))
    (Module_comment
      (Comment "%  merges three sorted lists X, Y and Z without duplicates, "))
    (Module_comment (Comment "%  removes duplicates"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List4))) (typ_kind Spec)
        (typ_name (Atom umerge3))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))
            (Type_variable (Var_name List3))))))
    (Function_decl
      ((fd_name (Atom umerge3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_list ())
                (Pattern_binding (Var_name L3))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge))))
                    (fa_args
                      ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom umerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name L1))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name H3)))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H2)) (Expr_list ())
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name H3))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rumerge3(X, Y, Z) -> L"))
    (Module_comment
      (Comment
        "%  merges three reversed sorted lists X, Y and Z without duplicates,"))
    (Module_comment (Comment "%  removes duplicates"))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X))
               (Type_variant
                 ((Type_variable (Var_name Y)) (Type_variable (Var_name Z))))))))
        (typ_kind Spec) (typ_name (Atom rumerge3))
        (typ_params
          ((Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))
            (Type_list (Type_variable (Var_name Z)))))))
    (Function_decl
      ((fd_name (Atom rumerge3)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name L1)) (Pattern_list ())
                (Pattern_binding (Var_name L3))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge))))
                   (fa_args
                     ((Expr_name (Var_name L1)) (Expr_name (Var_name L3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1)) (Pattern_binding (Var_name L2))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge))))
                    (fa_args
                      ((Expr_name (Var_name L1)) (Expr_name (Var_name L2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name L1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name L1))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H2)) (Expr_list ())
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name H3))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge(X, Y) -> L"))
    (Module_comment
      (Comment
        "%  merges two sorted lists X and Y without duplicates, removes duplicates"))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List3))) (typ_kind Spec)
        (typ_name (Atom umerge))
        (typ_params
          ((Type_variable (Var_name List1)) (Type_variable (Var_name List2))))))
    (Function_decl
      ((fd_name (Atom umerge)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name T2))))
             (c_guard ()) (c_rhs (Expr_name (Var_name T2))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name T2)) (Expr_list ())
                               (Expr_name (Var_name H1))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rumerge(X, Y) -> L"))
    (Module_comment
      (Comment
        "%  merges two reversed sorted lists X and Y without duplicates,"))
    (Module_comment (Comment "%  removes duplicates"))
    (Module_comment
      (Comment
        "% reverse(rumerge(reverse(A),reverse(B))) is equal to umerge(I,A,B)."))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name X)) (Type_variable (Var_name Y))))))
        (typ_kind Spec) (typ_name (Atom rumerge))
        (typ_params
          ((Type_list (Type_variable (Var_name X)))
            (Type_list (Type_variable (Var_name Y)))))))
    (Function_decl
      ((fd_name (Atom rumerge)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name T1)) (Pattern_list ())))
             (c_guard ()) (c_rhs (Expr_name (Var_name T1))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name T2)) (Expr_list ())
                               (Expr_name (Var_name H2))))))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% all(Predicate, List)"))
    (Module_comment (Comment "% any(Predicate, List)"))
    (Module_comment (Comment "% map(Function, List)"))
    (Module_comment (Comment "% flatmap(Function, List)"))
    (Module_comment (Comment "% foldl(Function, First, List)"))
    (Module_comment (Comment "% foldr(Function, Last, List)"))
    (Module_comment (Comment "% filter(Predicate, List)"))
    (Module_comment (Comment "% zf(Function, List)"))
    (Module_comment (Comment "% mapfoldl(Function, First, List)"))
    (Module_comment (Comment "% mapfoldr(Function, Last, List)"))
    (Module_comment (Comment "% foreach(Function, List)"))
    (Module_comment (Comment "% takewhile(Predicate, List)"))
    (Module_comment (Comment "% dropwhile(Predicate, List)"))
    (Module_comment (Comment "% splitwith(Predicate, List)"))
    (Module_comment
      (Comment "%  for list programming. Function here is a 'fun'."))
    (Module_comment (Comment "% "))
    (Module_comment (Comment "%  The name zf is a joke!"))
    (Module_comment (Comment %))
    (Module_comment
      (Comment
        "%  N.B. Unless where the functions actually needs it only foreach/2/3,"))
    (Module_comment
      (Comment
        "%  which is meant to be used for its side effects, has a defined order"))
    (Module_comment (Comment "%  of evaluation.")) (Module_comment (Comment %))
    (Module_comment
      (Comment
        "%  There are also versions with an extra argument, ExtraArgs, which is a"))
    (Module_comment (Comment "%  list of extra arguments to each call."))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom all))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom all)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom all))))
                          (fa_args
                            ((Expr_name (Var_name Pred))
                              (Expr_name (Var_name Tail))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs (Expr_literal (Lit_atom (Atom false)))))))))
            ((c_lhs ((Pattern_binding (Var_name Pred)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom true)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_constr ((tc_name (Atom_name (Atom boolean))) (tc_args ()))))
        (typ_kind Spec) (typ_name (Atom any))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom any)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom true)))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom any))))
                           (fa_args
                             ((Expr_name (Var_name Pred))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs ((Pattern_binding (Var_name Pred)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom false)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom map))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom map)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))))
             (c_guard ())
             (c_rhs
               (Expr_cons
                 ((Expr_apply
                    ((fa_name (Expr_name (Var_name F)))
                      (fa_args ((Expr_name (Var_name H)))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom map))))
                     (fa_args
                       ((Expr_name (Var_name F)) (Expr_name (Var_name T)))))))))
            ((c_lhs ((Pattern_binding (Var_name F)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom flatmap))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom flatmap)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name
                    (Expr_name
                      (Qualified_name (n_mod (Atom erlang))
                        (n_name (Atom '++')))))
                   (fa_args
                     ((Expr_apply
                        ((fa_name (Expr_name (Var_name F)))
                          (fa_args ((Expr_name (Var_name Hd))))))
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom flatmap))))
                           (fa_args
                             ((Expr_name (Var_name F))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs ((Pattern_binding (Var_name F)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Acc1))) (typ_kind Spec)
        (typ_name (Atom foldl))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name Acc0))
            (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom foldl)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F)) (Pattern_binding (Var_name Accu))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom foldl))))
                   (fa_args
                     ((Expr_name (Var_name F))
                       (Expr_apply
                         ((fa_name (Expr_name (Var_name F)))
                           (fa_args
                             ((Expr_name (Var_name Hd))
                               (Expr_name (Var_name Accu))))))
                       (Expr_name (Var_name Tail))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F))
                 (Pattern_binding (Var_name Accu)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name Accu))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name Acc1))) (typ_kind Spec)
        (typ_name (Atom foldr))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name Acc0))
            (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom foldr)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F)) (Pattern_binding (Var_name Accu))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Var_name F)))
                   (fa_args
                     ((Expr_name (Var_name Hd))
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom foldr))))
                           (fa_args
                             ((Expr_name (Var_name F))
                               (Expr_name (Var_name Accu))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F))
                 (Pattern_binding (Var_name Accu)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_name (Var_name Accu))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom filter))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List1))))))
    (Module_comment (Comment " FIXME: no support for list comprehensions yet"))
    (Module_comment
      (Comment " filter(Pred, List) when is_function(Pred, 1) ->"))
    (Module_comment (Comment "    [ E || E <- List, Pred(E) ]."))
    (Module_comment
      (Comment
        "% Equivalent to {filter(F, L), filter(NotF, L)}, if NotF = 'fun(X) ->"))
    (Module_comment (Comment "% not F(X) end'."))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name Satisfying))
             (Type_variable (Var_name NotSatisfying)))))
        (typ_kind Spec) (typ_name (Atom partition))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom partition)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_binding (Var_name L))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom partition))))
                   (fa_args
                     ((Expr_name (Var_name Pred)) (Expr_name (Var_name L))
                       (Expr_list ()) (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom partition)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name H)))
                  (Pattern_binding (Var_name T)))
                (Pattern_binding (Var_name As))
                (Pattern_binding (Var_name Bs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name H))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom partition))))
                          (fa_args
                            ((Expr_name (Var_name Pred))
                              (Expr_name (Var_name T))
                              (Expr_cons ((Expr_name (Var_name H)))
                                (Expr_name (Var_name As)))
                              (Expr_name (Var_name Bs))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom partition))))
                           (fa_args
                             ((Expr_name (Var_name Pred))
                               (Expr_name (Var_name T))
                               (Expr_name (Var_name As))
                               (Expr_cons ((Expr_name (Var_name H)))
                                 (Expr_name (Var_name Bs)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Pred)) (Pattern_list ())
                 (Pattern_binding (Var_name As))
                 (Pattern_binding (Var_name Bs))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs
                (Expr_tuple
                  ((Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom reverse))))
                       (fa_args ((Expr_name (Var_name As))))))
                    (Expr_apply
                      ((fa_name (Expr_name (Atom_name (Atom reverse))))
                        (fa_args ((Expr_name (Var_name Bs))))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom filtermap))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom filtermap)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name F)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_cons ((Expr_name (Var_name Hd)))
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom filtermap))))
                            (fa_args
                              ((Expr_name (Var_name F))
                                (Expr_name (Var_name Tail)))))))))
                   ((c_lhs
                      ((Pattern_tuple
                         ((Pattern_match (Lit_atom (Atom true)))
                           (Pattern_binding (Var_name Val))))))
                     (c_guard ())
                     (c_rhs
                       (Expr_cons ((Expr_name (Var_name Val)))
                         (Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom filtermap))))
                             (fa_args
                               ((Expr_name (Var_name F))
                                 (Expr_name (Var_name Tail)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom filtermap))))
                           (fa_args
                             ((Expr_name (Var_name F))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs ((Pattern_binding (Var_name F)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_list
           (Type_variant
             ((Type_variable (Var_name T)) (Type_variable (Var_name X))))))
        (typ_kind Spec) (typ_name (Atom zf))
        (typ_params
          ((Type_function (tyfun_args ((Type_variable (Var_name T))))
             (tyfun_return
               (Type_variant
                 ((Type_constr
                    ((tc_name (Atom_name (Atom boolean))) (tc_args ())))
                   (Type_tuple
                     ((Type_const (Lit_atom (Atom true)))
                       (Type_variable (Var_name X))))))))
            (Type_list (Type_variable (Var_name T)))))))
    (Function_decl
      ((fd_name (Atom zf)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F)) (Pattern_binding (Var_name L))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom filtermap))))
                   (fa_args
                     ((Expr_name (Var_name F)) (Expr_name (Var_name L))))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_const (Lit_atom (Atom ok)))) (typ_kind Spec)
        (typ_name (Atom foreach))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom foreach)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_comment (Comment " FIXME: this should be just F(Hd),")
                 (Expr_let
                   ((lb_lhs (Pattern_binding (Var_name _)))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Var_name F)))
                           (fa_args ((Expr_name (Var_name Hd))))))))
                   (Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom foreach))))
                       (fa_args
                         ((Expr_name (Var_name F)) (Expr_name (Var_name Tail))))))))))
            ((c_lhs ((Pattern_binding (Var_name F)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom ok)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List2)) (Type_variable (Var_name Acc1)))))
        (typ_kind Spec) (typ_name (Atom mapfoldl))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name Acc0))
            (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom mapfoldl)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_binding (Var_name Accu0))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs
                    (Pattern_tuple
                      ((Pattern_binding (Var_name R))
                        (Pattern_binding (Var_name Accu1)))))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name (Expr_name (Var_name F)))
                         (fa_args
                           ((Expr_name (Var_name Hd))
                             (Expr_name (Var_name Accu0))))))))
                 (Expr_let
                   ((lb_lhs
                      (Pattern_tuple
                        ((Pattern_binding (Var_name Rs))
                          (Pattern_binding (Var_name Accu2)))))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom mapfoldl))))
                           (fa_args
                             ((Expr_name (Var_name F))
                               (Expr_name (Var_name Accu1))
                               (Expr_name (Var_name Tail))))))))
                   (Expr_tuple
                     ((Expr_cons ((Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)))
                       (Expr_name (Var_name Accu2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F))
                 (Pattern_binding (Var_name Accu)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_tuple ((Expr_list ()) (Expr_name (Var_name Accu))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List2)) (Type_variable (Var_name Acc1)))))
        (typ_kind Spec) (typ_name (Atom mapfoldr))
        (typ_params
          ((Type_variable (Var_name Fun)) (Type_variable (Var_name Acc0))
            (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom mapfoldr)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name F))
                (Pattern_binding (Var_name Accu0))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs
                    (Pattern_tuple
                      ((Pattern_binding (Var_name Rs))
                        (Pattern_binding (Var_name Accu1)))))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom mapfoldr))))
                         (fa_args
                           ((Expr_name (Var_name F))
                             (Expr_name (Var_name Accu0))
                             (Expr_name (Var_name Tail))))))))
                 (Expr_let
                   ((lb_lhs
                      (Pattern_tuple
                        ((Pattern_binding (Var_name R))
                          (Pattern_binding (Var_name Accu2)))))
                     (lb_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Var_name F)))
                           (fa_args
                             ((Expr_name (Var_name Hd))
                               (Expr_name (Var_name Accu1))))))))
                   (Expr_tuple
                     ((Expr_cons ((Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)))
                       (Expr_name (Var_name Accu2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name F))
                 (Pattern_binding (Var_name Accu)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name F))
                          (Expr_literal (Lit_integer 2)))))))))
              (c_rhs (Expr_tuple ((Expr_list ()) (Expr_name (Var_name Accu))))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom takewhile))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom takewhile)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_cons ((Expr_name (Var_name Hd)))
                        (Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom takewhile))))
                            (fa_args
                              ((Expr_name (Var_name Pred))
                                (Expr_name (Var_name Tail)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ()) (c_rhs (Expr_list ())))))))
            ((c_lhs ((Pattern_binding (Var_name Pred)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom dropwhile))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom dropwhile)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_with_name
                  (Pattern_cons ((Pattern_binding (Var_name Hd)))
                    (Pattern_binding (Var_name Tail)))
                  (Var_name Rest))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom dropwhile))))
                          (fa_args
                            ((Expr_name (Var_name Pred))
                              (Expr_name (Var_name Tail))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ()) (c_rhs (Expr_name (Var_name Rest))))))))
            ((c_lhs ((Pattern_binding (Var_name Pred)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_list ())))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_variant
           ((Type_tuple
              ((Type_const (Lit_atom (Atom value)))
                (Type_variable (Var_name Value))))
             (Type_const (Lit_atom (Atom false))))))
        (typ_kind Spec) (typ_name (Atom search))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom search)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_tuple
                        ((Expr_literal (Lit_atom (Atom value)))
                          (Expr_name (Var_name Hd))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom search))))
                           (fa_args
                             ((Expr_name (Var_name Pred))
                               (Expr_name (Var_name Tail))))))))))))
            ((c_lhs ((Pattern_binding (Var_name Pred)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs (Expr_literal (Lit_atom (Atom false)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List1)) (Type_variable (Var_name List2)))))
        (typ_kind Spec) (typ_name (Atom splitwith))
        (typ_params
          ((Type_variable (Var_name Pred)) (Type_variable (Var_name List))))))
    (Function_decl
      ((fd_name (Atom splitwith)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_binding (Var_name List))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_function))))
                     (fa_args
                       ((Expr_name (Var_name Pred))
                         (Expr_literal (Lit_integer 1)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom splitwith))))
                   (fa_args
                     ((Expr_name (Var_name Pred)) (Expr_name (Var_name List))
                       (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom splitwith)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Pred))
                (Pattern_cons ((Pattern_binding (Var_name Hd)))
                  (Pattern_binding (Var_name Tail)))
                (Pattern_binding (Var_name Taken))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Pred)))
                     (fa_args ((Expr_name (Var_name Hd))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom splitwith))))
                          (fa_args
                            ((Expr_name (Var_name Pred))
                              (Expr_name (Var_name Tail))
                              (Expr_cons ((Expr_name (Var_name Hd)))
                                (Expr_name (Var_name Taken)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_tuple
                         ((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom reverse))))
                              (fa_args ((Expr_name (Var_name Taken))))))
                           (Expr_cons ((Expr_name (Var_name Hd)))
                             (Expr_name (Var_name Tail)))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Pred)) (Pattern_list ())
                 (Pattern_binding (Var_name Taken))))
              (c_guard
                (((Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_function))))
                      (fa_args
                        ((Expr_name (Var_name Pred))
                          (Expr_literal (Lit_integer 1)))))))))
              (c_rhs
                (Expr_tuple
                  ((Expr_apply
                     ((fa_name (Expr_name (Atom_name (Atom reverse))))
                       (fa_args ((Expr_name (Var_name Taken))))))
                    (Expr_list ())))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr
         (Type_tuple
           ((Type_variable (Var_name List2)) (Type_variable (Var_name List3)))))
        (typ_kind Spec) (typ_name (Atom split))
        (typ_params
          ((Type_variable (Var_name N)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom split)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name N))
                (Pattern_binding (Var_name List))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                     (fa_args ((Expr_name (Var_name N))))))
                  (Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name N))
                          (Expr_literal (Lit_integer 0))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_list))))
                      (fa_args ((Expr_name (Var_name List)))))))))
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom split))))
                     (fa_args
                       ((Expr_name (Var_name N)) (Expr_name (Var_name List))
                         (Expr_list ())))))
                 (((c_lhs
                     ((Pattern_with_name
                        (Pattern_tuple
                          ((Pattern_binding (Var_name _))
                            (Pattern_binding (Var_name _))))
                        (Var_name Result))))
                    (c_guard ()) (c_rhs (Expr_name (Var_name Result))))
                   ((c_lhs ((Pattern_binding (Var_name Fault))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name (Expr_name (Atom_name (Atom is_atom))))
                             (fa_args ((Expr_name (Var_name Fault)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom error)))))
                           (fa_args
                             ((Expr_name (Var_name Fault))
                               (Expr_list
                                 ((Expr_name (Var_name N))
                                   (Expr_name (Var_name List))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name N))
                 (Pattern_binding (Var_name List))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom erlang))
                         (n_name (Atom error)))))
                    (fa_args
                      ((Expr_literal (Lit_atom (Atom badarg)))
                        (Expr_list
                          ((Expr_name (Var_name N))
                            (Expr_name (Var_name List))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom split)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_match (Lit_integer 0)) (Pattern_binding (Var_name L))
                (Pattern_binding (Var_name R))))
             (c_guard ())
             (c_rhs
               (Expr_tuple
                 ((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom lists))
                           (n_name (Atom reverse)))))
                      (fa_args ((Expr_name (Var_name R)) (Expr_list ())))))
                   (Expr_name (Var_name L))))))
            ((c_lhs
               ((Pattern_binding (Var_name N))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split))))
                    (fa_args
                      ((Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom erlang))
                                (n_name (Atom '-')))))
                           (fa_args
                             ((Expr_name (Var_name N))
                               (Expr_literal (Lit_integer 1))))))
                        (Expr_name (Var_name T))
                        (Expr_cons ((Expr_name (Var_name H)))
                          (Expr_name (Var_name R)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _)) (Pattern_list ())
                 (Pattern_binding (Var_name _))))
              (c_guard ()) (c_rhs (Expr_literal (Lit_atom (Atom badarg)))))))
        (fd_spec ())))
    (Type_decl
      ((typ_expr (Type_variable (Var_name List2))) (typ_kind Spec)
        (typ_name (Atom join))
        (typ_params
          ((Type_variable (Var_name Sep)) (Type_variable (Var_name List1))))))
    (Function_decl
      ((fd_name (Atom join)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _Sep)) (Pattern_list ())))
             (c_guard ()) (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_binding (Var_name Sep))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom join_prepend))))
                      (fa_args
                        ((Expr_name (Var_name Sep)) (Expr_name (Var_name T)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom join_prepend)) (fd_arity 2)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name _Sep)) (Pattern_list ())))
             (c_guard ()) (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_binding (Var_name Sep))
                 (Pattern_cons ((Pattern_binding (Var_name H)))
                   (Pattern_binding (Var_name T)))))
              (c_guard ())
              (c_rhs
                (Expr_cons
                  ((Expr_name (Var_name Sep)) (Expr_name (Var_name H)))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom join_prepend))))
                      (fa_args
                        ((Expr_name (Var_name Sep)) (Expr_name (Var_name T)))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "%% ================================================================="))
    (Module_comment
      (Comment "%% Here follows the implementation of the sort functions."))
    (Module_comment (Comment %%))
    (Module_comment
      (Comment
        "%% These functions used to be in their own module (lists_sort),"))
    (Module_comment
      (Comment
        "%% but have now been placed here to allow Dialyzer to produce better"))
    (Module_comment (Comment "%% type information."))
    (Module_comment
      (Comment
        "%% ================================================================="))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom inline)))
              (Expr_list
                ((Expr_tuple
                   ((Expr_literal (Lit_atom (Atom merge3_12)))
                     (Expr_literal (Lit_integer 7))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom merge3_21)))
                      (Expr_literal (Lit_integer 7))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rmerge3_12)))
                      (Expr_literal (Lit_integer 7))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rmerge3_21)))
                      (Expr_literal (Lit_integer 7)))))))))))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom inline)))
              (Expr_list
                ((Expr_tuple
                   ((Expr_literal (Lit_atom (Atom umerge3_12)))
                     (Expr_literal (Lit_integer 8))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom umerge3_21)))
                      (Expr_literal (Lit_integer 8))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rumerge3_12a)))
                      (Expr_literal (Lit_integer 7))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rumerge3_12b)))
                      (Expr_literal (Lit_integer 8)))))))))))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom inline)))
              (Expr_list
                ((Expr_tuple
                   ((Expr_literal (Lit_atom (Atom keymerge3_12)))
                     (Expr_literal (Lit_integer 12))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom keymerge3_21)))
                      (Expr_literal (Lit_integer 12))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rkeymerge3_12)))
                      (Expr_literal (Lit_integer 12))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rkeymerge3_21)))
                      (Expr_literal (Lit_integer 12)))))))))))
    (Module_attribute
      ((atr_name (Atom compile))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom inline)))
              (Expr_list
                ((Expr_tuple
                   ((Expr_literal (Lit_atom (Atom ukeymerge3_12)))
                     (Expr_literal (Lit_integer 13))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom ukeymerge3_21)))
                      (Expr_literal (Lit_integer 13))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rukeymerge3_12a)))
                      (Expr_literal (Lit_integer 11))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rukeymerge3_21a)))
                      (Expr_literal (Lit_integer 13))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rukeymerge3_12b)))
                      (Expr_literal (Lit_integer 12))))
                  (Expr_tuple
                    ((Expr_literal (Lit_atom (Atom rukeymerge3_21b)))
                      (Expr_literal (Lit_integer 12)))))))))))
    (Module_comment (Comment "% sort/1"))
    (Module_comment (Comment "% Ascending."))
    (Function_decl
      ((fd_name (Atom split_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>=')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom split_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_list ()) (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_list ((Expr_name (Var_name Z))))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name Z))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom split_1_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>=')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom split_1_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>=')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name S)) (Expr_name (Var_name Z)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1))))
                    (fa_args
                      ((Expr_name (Var_name S)) (Expr_name (Var_name Z))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name S))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom split_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom split_2))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_list ()) (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_list ((Expr_name (Var_name Z))))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name Z))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom split_2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom split_2_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name S)) (Expr_name (Var_name Z)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2))))
                    (fa_args
                      ((Expr_name (Var_name S)) (Expr_name (Var_name Z))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom split_2))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name S))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge/1"))
    (Function_decl
      ((fd_name (Atom mergel)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_list ()))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom mergel))))
                   (fa_args
                     ((Expr_name (Var_name L)) (Expr_name (Var_name Acc))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name (Expr_name (Atom_name (Atom merge3_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1)) (Expr_list ())
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2))
                                   (Expr_name (Var_name H3))
                                   (Expr_name (Var_name T3)))))))
                          (Expr_name (Var_name Acc)))))))))
            ((c_lhs
               ((Pattern_list
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs ((Pattern_list ()) (Pattern_list ()))) (c_guard ())
              (c_rhs (Expr_list ())))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                    (fa_args ((Expr_name (Var_name Acc)) (Expr_list ())))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name A)) (Pattern_list ()))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons ((Expr_name (Var_name A)))
                         (Expr_name (Var_name L)))
                        (Expr_name (Var_name Acc))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name A))
                    (Pattern_binding (Var_name B)) (Pattern_list ()))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_name (Var_name A)) (Expr_name (Var_name B)))
                         (Expr_name (Var_name L)))
                        (Expr_name (Var_name Acc))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rmergel)) (fd_arity 2)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_cons ((Pattern_binding (Var_name H3)))
                    (Pattern_binding (Var_name T3)))
                   (Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2)))
                   (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom rmerge3_1))))
                              (fa_args
                                ((Expr_name (Var_name T1)) (Expr_list ())
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name T2))
                                  (Expr_name (Var_name H3))
                                  (Expr_name (Var_name T3)))))))
                         (Expr_name (Var_name Acc)))))))))
            ((c_lhs
               ((Pattern_list
                  ((Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs ((Pattern_list ()) (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom mergel))))
                    (fa_args ((Expr_name (Var_name Acc)) (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge3/3"))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom merge3_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom merge3_2)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M)) (Pattern_list ())
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M)) (Pattern_list ())
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2. Inlined."))
    (Function_decl
      ((fd_name (Atom merge3_12)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_1))))
                   (fa_args
                     ((Expr_name (Var_name T1))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom merge3_12_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_1))))
                   (fa_args
                     ((Expr_name (Var_name T1))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2. Inlined."))
    (Function_decl
      ((fd_name (Atom merge3_21)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom merge3_21_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge3_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rmerge/3"))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom rmerge3_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom rmerge3_2)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M)) (Pattern_list ())
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name M)) (Pattern_list ())
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2. Inlined."))
    (Function_decl
      ((fd_name (Atom rmerge3_12)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rmerge3_12_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2. Inlined."))
    (Function_decl
      ((fd_name (Atom rmerge3_21)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rmerge3_21_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge/2"))
    (Function_decl
      ((fd_name (Atom merge2_1)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom merge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name HdM))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom merge2_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons
                         ((Expr_name (Var_name H1)) (Expr_name (Var_name HdM)))
                         (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom merge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name HdM)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rmerge/2"))
    (Function_decl
      ((fd_name (Atom rmerge2_1)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge2_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rmerge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name HdM))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rmerge2_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name HdM)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rmerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name HdM)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% usort/1"))
    (Module_comment (Comment "% Ascending."))
    (Function_decl
      ((fd_name (Atom usplit_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_list ()) (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_list ((Expr_name (Var_name Z))))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name Z))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom usplit_1_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '>')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '>')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name S)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name S)) (Expr_name (Var_name Z))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name S)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name S))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom usplit_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '<')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_list ()) (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_list ((Expr_name (Var_name Z))))
                        (Expr_name (Var_name Rs))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name Z))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom usplit_2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '<')))))
                     (fa_args
                       ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                   (fa_args
                     ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))
                       (Expr_name (Var_name L))
                       (Expr_cons ((Expr_name (Var_name X)))
                         (Expr_name (Var_name R)))
                       (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name Y)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L))
                        (Expr_cons ((Expr_name (Var_name X)))
                          (Expr_name (Var_name R)))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name X)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '<')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name S)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name S)) (Expr_name (Var_name Z))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name Z)) (Expr_name (Var_name S)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2_1))))
                    (fa_args
                      ((Expr_name (Var_name X)) (Expr_name (Var_name Y))
                        (Expr_name (Var_name L)) (Expr_name (Var_name R))
                        (Expr_name (Var_name Rs)) (Expr_name (Var_name S))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_cons ((Pattern_binding (Var_name Z)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom usplit_2))))
                    (fa_args
                      ((Expr_name (Var_name Z)) (Expr_name (Var_name S))
                        (Expr_name (Var_name L)) (Expr_list ())
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name X)) (Pattern_binding (Var_name Y))
                 (Pattern_list ()) (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge/1"))
    (Function_decl
      ((fd_name (Atom umergel)) (fd_arity 1)
        (fd_cases
          (((c_lhs ((Pattern_binding (Var_name L)))) (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umergel))))
                   (fa_args
                     ((Expr_name (Var_name L)) (Expr_list ())
                       (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom umergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_list ()))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name O))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umergel))))
                   (fa_args
                     ((Expr_name (Var_name L)) (Expr_name (Var_name Acc))
                       (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom asc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom umerge3_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_cons ((Expr_name (Var_name H2)))
                                     (Expr_name (Var_name H3)))
                                   (Expr_name (Var_name T2))
                                   (Expr_name (Var_name H2)) (Expr_list ())
                                   (Expr_name (Var_name T3))
                                   (Expr_name (Var_name H3)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H3)))
                     (Pattern_binding (Var_name T3)))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1)))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom umerge3_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_cons ((Expr_name (Var_name H2)))
                                     (Expr_name (Var_name H3)))
                                   (Expr_name (Var_name T2))
                                   (Expr_name (Var_name H2)) (Expr_list ())
                                   (Expr_name (Var_name T3))
                                   (Expr_name (Var_name H3)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name A)) (Pattern_list ()))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_cons ((Expr_name (Var_name A)))
                         (Expr_name (Var_name L)))
                        (Expr_name (Var_name Acc)) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name A))
                    (Pattern_binding (Var_name B)) (Pattern_list ()))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_name (Var_name A)) (Expr_name (Var_name B)))
                         (Expr_name (Var_name L)))
                        (Expr_name (Var_name Acc)) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H1)))
                     (Pattern_binding (Var_name T1)))
                    (Pattern_binding (Var_name T2)))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom asc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom umerge2_2))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_name (Var_name H1)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T2))
                    (Pattern_cons ((Pattern_binding (Var_name H1)))
                      (Pattern_binding (Var_name T1))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom umerge2_2))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_name (Var_name H1)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ()) (Pattern_binding (Var_name _O))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_list ())
                 (Pattern_binding (Var_name _O))))
              (c_guard ()) (c_rhs (Expr_list ())))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rumergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_cons ((Pattern_binding (Var_name H3)))
                    (Pattern_binding (Var_name T3)))
                   (Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2)))
                   (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_match (Lit_atom (Atom asc)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name (Atom_name (Atom rumerge3_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name T2))
                                  (Expr_name (Var_name H2)) (Expr_list ())
                                  (Expr_name (Var_name T3))
                                  (Expr_name (Var_name H3)))))))
                         (Expr_name (Var_name Acc)))
                       (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rumerge3_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name T2))
                                   (Expr_name (Var_name H2)) (Expr_list ())
                                   (Expr_name (Var_name T3))
                                   (Expr_name (Var_name H3)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1)))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom asc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rumerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_name (Var_name H2)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rumerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_name (Var_name H2)))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge3/3"))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom umerge3_1)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name HdM))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name HdM))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name HdM))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M)) (Expr_name (Var_name HdM))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M)) (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom umerge3_2)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name HdM)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_12))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name HdM))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_21))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name HdM))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_list ()) (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_list ()) (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M)) (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_list ()) (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2. Inlined."))
    (Function_decl
      ((fd_name (Atom umerge3_12)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name _HdM))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name HdM))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name _HdM))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom umerge3_12_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2. Inlined."))
    (Function_decl
      ((fd_name (Atom umerge3_21)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name _HdM))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name HdM))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name _HdM))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom umerge3_21_3)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge3_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom rumerge3_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge3_12a))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                       (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2. Inlined."))
    (Function_decl
      ((fd_name (Atom rumerge3_12a)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name M))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart. H2M > H3. H2M > H2."))
    (Function_decl
      ((fd_name (Atom rumerge3_2)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name H2M)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_comment (Comment " H2M > H1.")
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom rumerge3_12b))))
                     (fa_args
                       ((Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                         (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                         (Expr_name (Var_name M)) (Expr_name (Var_name T3))
                         (Expr_name (Var_name H3)) (Expr_name (Var_name H2M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_comment (Comment " H2M > H1.")
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom rumerge3_21_3))))
                      (fa_args
                        ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                          (Expr_name (Var_name H2))
                          (Expr_cons ((Expr_name (Var_name H2M)))
                            (Expr_name (Var_name M)))
                          (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                          (Expr_name (Var_name H1)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_comment (Comment " H2M > H1.")
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                      (fa_args
                        ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                          (Expr_name (Var_name H2))
                          (Expr_cons
                            ((Expr_name (Var_name H1))
                              (Expr_name (Var_name H2M)))
                            (Expr_name (Var_name M)))
                          (Expr_name (Var_name T3)) (Expr_name (Var_name H3)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T3))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2. Inlined."))
    (Function_decl
      ((fd_name (Atom rumerge3_12b)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name H2M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H2M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name H2M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rumerge3_12_3)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name H3M))
                (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H2)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H3M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))
                 (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))
                 (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ()) (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name H3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ()) (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 > H2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rumerge3_21_3)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name H3M))
                (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name H2))
                       (Expr_cons ((Expr_name (Var_name H3M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name T3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))
                 (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_cons ((Pattern_binding (Var_name H3)))
                   (Pattern_binding (Var_name T3)))
                 (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name H2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ()) (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name H2)) (Pattern_binding (Var_name M))
                 (Pattern_list ()) (Pattern_binding (Var_name H3M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge/2"))
    (Module_comment
      (Comment "% Elements from the first list are kept and prioritized."))
    (Function_decl
      ((fd_name (Atom umerge2_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name _HdM))
                (Pattern_binding (Var_name H2))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name H2))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name H2))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom umerge2_2)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom umerge2_1))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom umerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rumerge/2"))
    (Module_comment
      (Comment "% Elements from the first list are kept and prioritized."))
    (Function_decl
      ((fd_name (Atom rumerge2_1)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name H2))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name M)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " H1 =< H2M."))
    (Function_decl
      ((fd_name (Atom rumerge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2M))
                (Pattern_binding (Var_name H1))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rumerge2_2))))
                   (fa_args
                     ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H2M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1))
                 (Pattern_cons ((Pattern_binding (Var_name H2)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rumerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name T1)) (Expr_name (Var_name T2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name H1)) (Expr_name (Var_name H2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% keysort/2"))
    (Module_comment (Comment "% Ascending."))
    (Function_decl
      ((fd_name (Atom keysplit_1)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name Z))
                              (Expr_name (Var_name EZ))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name _EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name R)) (Expr_list ()))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_list ((Expr_name (Var_name Z))))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name L))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keysplit_1_1)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY)) (Pattern_binding (Var_name ES))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keysplit_1_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name Z))
                              (Expr_name (Var_name EZ))
                              (Expr_name (Var_name ES))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))
                              (Expr_name (Var_name S))
                              (Expr_name (Var_name L))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name ES))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name L))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '=<')))))
                             (fa_args
                               ((Expr_name (Var_name ES))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY))
                 (Pattern_binding (Var_name _ES))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S)) (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_list ((Expr_name (Var_name S))))
                            (Expr_cons
                              ((Expr_name (Var_name Y))
                                (Expr_name (Var_name X)))
                              (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom keysplit_2)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '>')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name Z))
                              (Expr_name (Var_name EZ))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '>')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name _EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name R)) (Expr_list ()))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_list ((Expr_name (Var_name Z))))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name L))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keysplit_2_1)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY)) (Pattern_binding (Var_name ES))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '>')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keysplit_2_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name Z))
                              (Expr_name (Var_name EZ))
                              (Expr_name (Var_name ES))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))
                              (Expr_name (Var_name S))
                              (Expr_name (Var_name L))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '>')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name ES))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name L))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '>')))))
                             (fa_args
                               ((Expr_name (Var_name ES))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY))
                 (Pattern_binding (Var_name _ES))
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S)) (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_list ((Expr_name (Var_name S))))
                            (Expr_cons
                              ((Expr_name (Var_name Y))
                                (Expr_name (Var_name X)))
                              (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keymergel)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3))))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name O))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_name (Var_name O))
                         (Expr_literal (Lit_atom (Atom asc))))))))))
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name M)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom keymerge3_1))))
                         (fa_args
                           ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                             (Expr_list ()) (Expr_name (Var_name O))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H2))))))
                             (Expr_name (Var_name H2))
                             (Expr_name (Var_name T2))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H3))))))
                             (Expr_name (Var_name H3))
                             (Expr_name (Var_name T3))))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                         (Expr_cons ((Expr_name (Var_name M)))
                           (Expr_name (Var_name Acc)))
                         (Expr_name (Var_name O)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3)))
                     (Pattern_cons ((Pattern_binding (Var_name H2)))
                       (Pattern_binding (Var_name T2)))
                     (Pattern_binding (Var_name T1)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name O))
                          (Expr_literal (Lit_atom (Atom desc))))))))))
              (c_rhs
                (Expr_let
                  ((lb_lhs (Pattern_binding (Var_name M)))
                    (lb_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge3_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_list ()) (Expr_name (Var_name O))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom element))))
                                  (fa_args
                                    ((Expr_name (Var_name I))
                                      (Expr_name (Var_name H2))))))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom element))))
                                  (fa_args
                                    ((Expr_name (Var_name I))
                                      (Expr_name (Var_name H3))))))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                      (fa_args
                        ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                          (Expr_cons ((Expr_name (Var_name M)))
                            (Expr_name (Var_name Acc)))
                          (Expr_name (Var_name O)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_binding (Var_name T1))
                     (Pattern_cons ((Pattern_binding (Var_name H2)))
                       (Pattern_binding (Var_name T2))))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom asc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom keymerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                     (Pattern_binding (Var_name T1)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom keymerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ()) (Pattern_binding (Var_name _O))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name L)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name Acc))
                        (Expr_list ()) (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rkeymergel)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H3)))
                     (Pattern_binding (Var_name T3)))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name O))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '==')))))
                     (fa_args
                       ((Expr_name (Var_name O))
                         (Expr_literal (Lit_atom (Atom asc))))))))))
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name M)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_1))))
                         (fa_args
                           ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                             (Expr_list ()) (Expr_name (Var_name O))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H2))))))
                             (Expr_name (Var_name H2))
                             (Expr_name (Var_name T2))
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H3))))))
                             (Expr_name (Var_name H3))
                             (Expr_name (Var_name T3))))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                         (Expr_cons ((Expr_name (Var_name M)))
                           (Expr_name (Var_name Acc)))
                         (Expr_name (Var_name O)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_binding (Var_name T1))
                     (Pattern_cons ((Pattern_binding (Var_name H2)))
                       (Pattern_binding (Var_name T2)))
                     (Pattern_cons ((Pattern_binding (Var_name H3)))
                       (Pattern_binding (Var_name T3))))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name O))
                          (Expr_literal (Lit_atom (Atom desc))))))))))
              (c_rhs
                (Expr_let
                  ((lb_lhs (Pattern_binding (Var_name M)))
                    (lb_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_list ()) (Expr_name (Var_name O))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom element))))
                                  (fa_args
                                    ((Expr_name (Var_name I))
                                      (Expr_name (Var_name H2))))))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom element))))
                                  (fa_args
                                    ((Expr_name (Var_name I))
                                      (Expr_name (Var_name H3))))))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                      (fa_args
                        ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                          (Expr_cons ((Expr_name (Var_name M)))
                            (Expr_name (Var_name Acc)))
                          (Expr_name (Var_name O)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                     (Pattern_binding (Var_name T1)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom asc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rkeymerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_binding (Var_name T1))
                     (Pattern_cons ((Pattern_binding (Var_name H2)))
                       (Pattern_binding (Var_name T2))))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rkeymerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name L)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name Acc))
                        (Expr_list ()) (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "%% An extra argument, D, just to avoid some move instructions."))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom keymerge3_1)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name D))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name T2))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom keymerge3_2)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name T1))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name D))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2. Inlined."))
    (Function_decl
      ((fd_name (Atom keymerge3_12)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keymerge3_1))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name D)) (Expr_name (Var_name E2))
                       (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom keymerge3_12_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge3_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom keymerge3_12_3))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name T3))
                               (Expr_cons ((Expr_name (Var_name H3)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2. Inlined."))
    (Function_decl
      ((fd_name (Atom keymerge3_21)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom keymerge3_2))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name D)) (Expr_name (Var_name E3))
                       (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom keymerge3_21_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E2))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge3_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom keymerge3_21_3))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name T3))
                               (Expr_cons ((Expr_name (Var_name H3)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom keymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_1)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name T2))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rkeymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name D))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_2)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name E3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name D))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rkeymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name T1))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2. Inlined."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_12)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name D)) (Expr_name (Var_name E3))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_12_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E2))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name (Atom_name (Atom rkeymerge3_12_3))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name T3))
                              (Expr_cons ((Expr_name (Var_name H3)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2. Inlined."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_21)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name T3))
                       (Expr_cons ((Expr_name (Var_name H3)))
                         (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name D)) (Expr_name (Var_name E2))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rkeymerge3_21_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name (Atom_name (Atom rkeymerge3_21_3))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name T3))
                              (Expr_cons ((Expr_name (Var_name H3)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rkeymerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% keymerge/3"))
    (Module_comment
      (Comment "% Elements from the first list are prioritized."))
    (Function_decl
      ((fd_name (Atom keymerge2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keymerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name H1))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_list ())
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom keymerge2_2)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name HdM))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H1))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom keymerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons
                                ((Expr_name (Var_name H1))
                                  (Expr_name (Var_name HdM)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom keymerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name HdM)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name H1))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name HdM)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rkeymerge/3"))
    (Function_decl
      ((fd_name (Atom rkeymerge2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_list ())
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rkeymerge2_2)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name HdM))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H1))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name HdM)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rkeymerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons
                                 ((Expr_name (Var_name H1))
                                   (Expr_name (Var_name HdM)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I))
                 (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name HdM)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name HdM)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% ukeysort/2"))
    (Module_comment (Comment "% Ascending."))
    (Function_decl
      ((fd_name (Atom ukeysplit_1)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '==')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                              (Expr_name (Var_name EX))
                              (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name L)) (Expr_name (Var_name R))
                              (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EY))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name _EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name R)) (Expr_list ()))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_list ((Expr_name (Var_name Z))))
                               (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom ukeysplit_1_1)) (fd_arity 10)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name EX)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S)) (Pattern_binding (Var_name ES))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '==')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                              (Expr_name (Var_name EX))
                              (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name L)) (Expr_name (Var_name R))
                              (Expr_name (Var_name Rs))
                              (Expr_name (Var_name S))
                              (Expr_name (Var_name ES))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EY))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EX))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name X)))
                                 (Expr_name (Var_name R)))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name ES))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeysplit_1_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name X))
                               (Expr_name (Var_name EX))
                               (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name L))
                               (Expr_name (Var_name R))
                               (Expr_name (Var_name Rs))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name ES))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name S))
                               (Expr_name (Var_name ES))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_cons
                                 ((Expr_cons
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name X)))
                                    (Expr_name (Var_name R))))
                                 (Expr_name (Var_name Rs)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name _EX))
                 (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY)) (Pattern_list ())
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))
                 (Pattern_binding (Var_name _ES))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_list ((Expr_name (Var_name S))))
                            (Expr_cons
                              ((Expr_name (Var_name Y))
                                (Expr_name (Var_name X)))
                              (Expr_name (Var_name R))))
                          (Expr_name (Var_name Rs)))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom ukeysplit_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name Y))
                (Pattern_binding (Var_name EY))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_binding (Var_name EZ))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '==')))))
                            (fa_args
                              ((Expr_name (Var_name EY))
                                (Expr_name (Var_name EZ)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeysplit_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name EY))
                              (Expr_name (Var_name L))
                              (Expr_name (Var_name R))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '<')))))
                             (fa_args
                               ((Expr_name (Var_name EY))
                                 (Expr_name (Var_name EZ)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_1))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Y))
                               (Expr_name (Var_name EY))
                               (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L)) (Expr_list ())
                               (Expr_list
                                 ((Expr_apply
                                    ((fa_name
                                       (Expr_name
                                         (Qualified_name (n_mod (Atom lists))
                                           (n_name (Atom reverse)))))
                                      (fa_args
                                        ((Expr_name (Var_name R))
                                          (Expr_list ())))))))))))))
                   ((c_lhs ((Pattern_binding (Var_name EZ)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeysplit_2))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name Z))
                               (Expr_name (Var_name EZ))
                               (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name Y)))
                                 (Expr_name (Var_name R)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name Y))
                 (Pattern_binding (Var_name _EY)) (Pattern_list ())
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name Y))) (Expr_name (Var_name R)))))))
        (fd_spec ())))
    (Module_attribute
      ((atr_name (Atom dialyzer))
        (atr_value
          (Expr_tuple
            ((Expr_literal (Lit_atom (Atom no_improper_lists)))
              (Expr_apply
                ((fa_name
                   (Expr_name
                     (Qualified_name (n_mod (Atom erlang)) (n_name (Atom '/')))))
                  (fa_args
                    ((Expr_literal (Lit_atom (Atom ukeymergel)))
                      (Expr_literal (Lit_integer 3)))))))))))
    (Function_decl
      ((fd_name (Atom ukeymergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_cons ((Pattern_binding (Var_name H3)))
                      (Pattern_binding (Var_name T3))))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))))
             (c_guard ())
             (c_rhs
               (Expr_comment
                 (Comment
                   "% The fourth argument, [H2 | H3] (=HdM), may confuse type")
                 (Expr_comment
                   (Comment
                     "% checkers. Its purpose is to ensure that the tests H2 == HdM")
                   (Expr_comment
                     (Comment
                       "% and H3 == HdM in ukeymerge3_1 will always fail as long as M == [].")
                     (Expr_let
                       ((lb_lhs (Pattern_binding (Var_name M)))
                         (lb_rhs
                           (Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom ukeymerge3_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_name (Var_name Acc))
                                   (Expr_cons ((Expr_name (Var_name H2)))
                                     (Expr_name (Var_name H3)))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H3))))))
                                   (Expr_name (Var_name H3))
                                   (Expr_name (Var_name T3))))))))
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeymergel))))
                           (fa_args
                             ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                               (Expr_cons ((Expr_name (Var_name M)))
                                 (Expr_name (Var_name Acc)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_cons ((Pattern_binding (Var_name H1)))
                      (Pattern_binding (Var_name T1)))
                     (Pattern_binding (Var_name T2)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom ukeymerge2_2))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H1))))))
                                   (Expr_name (Var_name H1))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ())))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name L)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name Acc))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rukeymergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H3)))
                     (Pattern_binding (Var_name T3)))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))))
             (c_guard ())
             (c_rhs
               (Expr_let
                 ((lb_lhs (Pattern_binding (Var_name M)))
                   (lb_rhs
                     (Expr_apply
                       ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_1))))
                         (fa_args
                           ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                             (Expr_name (Var_name Acc)) (Expr_list ())
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H2))))))
                             (Expr_name (Var_name H2))
                             (Expr_name (Var_name T2)) (Expr_list ())
                             (Expr_apply
                               ((fa_name
                                  (Expr_name (Atom_name (Atom element))))
                                 (fa_args
                                   ((Expr_name (Var_name I))
                                     (Expr_name (Var_name H3))))))
                             (Expr_name (Var_name H3))
                             (Expr_name (Var_name T3))))))))
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                         (Expr_cons ((Expr_name (Var_name M)))
                           (Expr_name (Var_name Acc))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_cons
                   ((Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2)))
                     (Pattern_binding (Var_name T1)))
                   (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rukeymerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name I))
                                   (Expr_name (Var_name T1))
                                   (Expr_apply
                                     ((fa_name
                                        (Expr_name (Atom_name (Atom element))))
                                       (fa_args
                                         ((Expr_name (Var_name I))
                                           (Expr_name (Var_name H2))))))
                                   (Expr_name (Var_name T2)) (Expr_list ())
                                   (Expr_name (Var_name H2)))))))
                          (Expr_name (Var_name Acc)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I))
                 (Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name
                                  (Qualified_name (n_mod (Atom lists))
                                    (n_name (Atom reverse)))))
                               (fa_args
                                 ((Expr_name (Var_name L)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_list ())))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name Acc))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymergel))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name Acc))
                        (Expr_list ())))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "%% An extra argument, D, just to avoid some move instructions."))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_1)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name D)) (Pattern_binding (Var_name HdM))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name E3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name HdM))
                              (Expr_name (Var_name D))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E2))
                                 (Expr_name (Var_name HdM)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name HdM))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name HdM))
                               (Expr_name (Var_name T2))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name _H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name HdM))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name E2))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name _H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_2)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name HdM)) (Pattern_binding (Var_name D))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_12))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name HdM))
                              (Expr_name (Var_name T1))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeymerge3_21))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name HdM))
                               (Expr_name (Var_name D))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1)) (Pattern_list ())
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name _D)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1)) (Pattern_list ())
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name _D)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name _H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T3)) (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1)) (Pattern_list ())
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name _D)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2. Inlined."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_12)) (fd_arity 13)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name _HdM))
                (Pattern_binding (Var_name D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_1))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name D)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H1)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name _H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name _D))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name _D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_12_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_12_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeymerge3_12_3))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H3)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2. Inlined."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_21)) (fd_arity 13)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name _HdM))
                (Pattern_binding (Var_name D))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_2))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name E2))
                       (Expr_name (Var_name D))
                       (Expr_cons ((Expr_name (Var_name H2)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name _H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name _D))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E3)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name _E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name _D))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_21_3))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name T1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom ukeymerge3_21_3)) (fd_arity 9)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name T1)) (Pattern_binding (Var_name H1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E2))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge3_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom ukeymerge3_21_3))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H3)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name H1))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment
      (Comment
        "%% Two extra arguments, D1 and D2, just to avoid some move instructions."))
    (Module_comment (Comment "% Take L1 apart."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_1)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name D1)) (Pattern_binding (Var_name D2))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name E3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name (Atom_name (Atom rukeymerge3_12a))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))
                              (Expr_name (Var_name M))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_21a))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name D1))
                               (Expr_name (Var_name D2))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D1))
                 (Pattern_binding (Var_name _D2))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name T3))
                        (Expr_name (Var_name M)) (Expr_name (Var_name E3))
                        (Expr_name (Var_name H3)) (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_list ())
                 (Pattern_binding (Var_name _D1))
                 (Pattern_binding (Var_name _D2))
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2. Inlined."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_12a)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name M))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2. Inlined"))
    (Function_decl
      ((fd_name (Atom rukeymerge3_21a)) (fd_arity 13)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name _D1))
                (Pattern_binding (Var_name _D2))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2)) (Expr_name (Var_name M))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name D1))
                 (Pattern_binding (Var_name D2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name D1)) (Expr_name (Var_name D2))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Take L2 apart. E2M > E3. E2M > E2."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_2)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name H2M))
                (Pattern_binding (Var_name E2M)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name E3)) (Pattern_binding (Var_name H3))
                (Pattern_binding (Var_name T3))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_comment (Comment " E2M > E1.")
                        (Expr_apply
                          ((fa_name
                             (Expr_name (Atom_name (Atom rukeymerge3_12b))))
                            (fa_args
                              ((Expr_name (Var_name I))
                                (Expr_name (Var_name E1))
                                (Expr_name (Var_name H1))
                                (Expr_name (Var_name T1))
                                (Expr_name (Var_name E2))
                                (Expr_name (Var_name H2))
                                (Expr_name (Var_name T2))
                                (Expr_name (Var_name E3))
                                (Expr_name (Var_name H3))
                                (Expr_name (Var_name T3))
                                (Expr_name (Var_name M))
                                (Expr_name (Var_name H2M)))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E1))
                                 (Expr_name (Var_name E2M)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_comment (Comment " E2M > E1.")
                         (Expr_apply
                           ((fa_name
                              (Expr_name (Atom_name (Atom rukeymerge3_21b))))
                             (fa_args
                               ((Expr_name (Var_name I))
                                 (Expr_name (Var_name E1))
                                 (Expr_name (Var_name H1))
                                 (Expr_name (Var_name T1))
                                 (Expr_name (Var_name E2))
                                 (Expr_name (Var_name H2))
                                 (Expr_name (Var_name T2))
                                 (Expr_name (Var_name E3))
                                 (Expr_name (Var_name H3))
                                 (Expr_name (Var_name T3))
                                 (Expr_name (Var_name M))
                                 (Expr_name (Var_name H2M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name _H2M))
                 (Pattern_binding (Var_name E2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name _E2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '=<')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name T3))
                        (Expr_cons ((Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1)) (Pattern_list ())
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name _E2M))
                 (Pattern_binding (Var_name M)) (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name T3))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2. Inlined."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_12b)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name H2M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E2)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_12_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H2M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name T2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name E2))
                        (Expr_cons ((Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2. Inlined"))
    (Function_decl
      ((fd_name (Atom rukeymerge3_21b)) (fd_arity 12)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name E3))
                (Pattern_binding (Var_name H3)) (Pattern_binding (Var_name T3))
                (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name H2M))))
             (c_guard
               (((Expr_apply
                   ((fa_name
                      (Expr_name
                        (Qualified_name (n_mod (Atom erlang))
                          (n_name (Atom '=<')))))
                     (fa_args
                       ((Expr_name (Var_name E1)) (Expr_name (Var_name E3)))))))))
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_21_3))))
                   (fa_args
                     ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                       (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                       (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                       (Expr_name (Var_name T2))
                       (Expr_cons ((Expr_name (Var_name H2M)))
                         (Expr_name (Var_name M)))
                       (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                       (Expr_name (Var_name T3))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name E3))
                 (Pattern_binding (Var_name H3))
                 (Pattern_binding (Var_name T3)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge3_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name H1)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name T2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name E3)) (Expr_name (Var_name H3))
                        (Expr_name (Var_name T3))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 =< E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_12_3)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name E3M))
                (Pattern_binding (Var_name H3M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E2))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name (Atom_name (Atom rukeymerge3_12_3))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H3M)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E2))
                                 (Expr_name (Var_name E3M)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name M))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name E2))
                               (Expr_cons ((Expr_name (Var_name H3M)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3M))
                 (Pattern_binding (Var_name _H3M)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name E3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name T2))
                        (Expr_name (Var_name M)) (Expr_name (Var_name E2))
                        (Expr_name (Var_name H2)) (Expr_name (Var_name H1))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _E3M))
                 (Pattern_binding (Var_name H3M)) (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E1)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name H2))
                        (Expr_name (Var_name H1))))))))))
        (fd_spec ())))
    (Module_comment (Comment " E1 > E2, take L3 apart."))
    (Function_decl
      ((fd_name (Atom rukeymerge3_21_3)) (fd_arity 11)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                (Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name E3M))
                (Pattern_binding (Var_name H3M))
                (Pattern_cons ((Pattern_binding (Var_name H3)))
                  (Pattern_binding (Var_name T3)))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H3))))))
                 (((c_lhs ((Pattern_binding (Var_name E3))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E3)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name
                           (Expr_name (Atom_name (Atom rukeymerge3_21_3))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name E1))
                              (Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H3M)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name E3))
                              (Expr_name (Var_name H3))
                              (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E1))
                                 (Expr_name (Var_name E3M)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))
                   ((c_lhs ((Pattern_binding (Var_name E3)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge3_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name T2))
                               (Expr_cons
                                 ((Expr_name (Var_name H1))
                                   (Expr_name (Var_name H3M)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name E3))
                               (Expr_name (Var_name H3))
                               (Expr_name (Var_name T3))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E3M))
                 (Pattern_binding (Var_name _H3M)) (Pattern_list ())))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E3M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))
            ((c_lhs
               ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1))
                 (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _E3M))
                 (Pattern_binding (Var_name H3M)) (Pattern_list ())))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_1))))
                    (fa_args
                      ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                        (Expr_name (Var_name E2)) (Expr_name (Var_name T2))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H3M)))
                          (Expr_name (Var_name M)))
                        (Expr_name (Var_name H2))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% ukeymerge/3"))
    (Module_comment
      (Comment "% Elements from the first list are kept and prioritized."))
    (Function_decl
      ((fd_name (Atom ukeymerge2_1)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name E2))
                (Pattern_binding (Var_name HdM))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name H2))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name E1))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H2))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E2))
                                 (Expr_name (Var_name HdM)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T2))
                               (Expr_name (Var_name M))))))))
                   ((c_lhs ((Pattern_binding (Var_name E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_list ())
                 (Pattern_binding (Var_name E2))
                 (Pattern_binding (Var_name HdM))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _H2))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E2)) (Expr_name (Var_name HdM)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2)) (Expr_name (Var_name M))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_list ())
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name _HdM))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom ukeymerge2_2)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E1)) (Pattern_binding (Var_name H1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name E1))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H2))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ukeymerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E1))
                               (Expr_name (Var_name H1))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E1))
                 (Pattern_binding (Var_name H1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rukeymerge/3"))
    (Function_decl
      ((fd_name (Atom rukeymerge2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I))
                (Pattern_cons ((Pattern_binding (Var_name H1)))
                  (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name E2)) (Pattern_binding (Var_name T2))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name H2))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H1))))))
                 (((c_lhs ((Pattern_binding (Var_name E1))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E1))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_binding (Var_name _E1)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name H2))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_list ())
                 (Pattern_binding (Var_name _E2))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rukeymerge2_2)) (fd_arity 8)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name I)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name E1))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M)) (Pattern_binding (Var_name E2M))
                (Pattern_binding (Var_name H2M))
                (Pattern_binding (Var_name H1))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom element))))
                     (fa_args
                       ((Expr_name (Var_name I)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_binding (Var_name E2))))
                    (c_guard
                      (((Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom erlang))
                                 (n_name (Atom '=<')))))
                            (fa_args
                              ((Expr_name (Var_name E1))
                                (Expr_name (Var_name E2)))))))))
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rukeymerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name I)) (Expr_name (Var_name T1))
                              (Expr_name (Var_name E1))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2M)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name E2))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2))))
                     (c_guard
                       (((Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom erlang))
                                  (n_name (Atom '==')))))
                             (fa_args
                               ((Expr_name (Var_name E1))
                                 (Expr_name (Var_name E2M)))))))))
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name H2))))))))
                   ((c_lhs ((Pattern_binding (Var_name E2)))) (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name (Atom_name (Atom rukeymerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name I))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name E2))
                               (Expr_name (Var_name T2))
                               (Expr_cons
                                 ((Expr_name (Var_name H1))
                                   (Expr_name (Var_name H2M)))
                                 (Expr_name (Var_name M)))
                               (Expr_name (Var_name H2))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name E1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name E2M))
                 (Pattern_binding (Var_name _H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard
                (((Expr_apply
                    ((fa_name
                       (Expr_name
                         (Qualified_name (n_mod (Atom erlang))
                           (n_name (Atom '==')))))
                      (fa_args
                        ((Expr_name (Var_name E1)) (Expr_name (Var_name E2M)))))))))
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))
            ((c_lhs
               ((Pattern_binding (Var_name _I)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _E1)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name _E2M))
                 (Pattern_binding (Var_name H2M))
                 (Pattern_binding (Var_name H1))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons
                          ((Expr_name (Var_name H1))
                            (Expr_name (Var_name H2M)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% sort/2"))
    (Module_comment (Comment "% Ascending."))
    (Function_decl
      ((fd_name (Atom fsplit_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fsplit_1))))
                          (fa_args
                            ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom fsplit_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name Z))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard
                               (((Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom erlang))
                                          (n_name (Atom '==')))))
                                     (fa_args
                                       ((Expr_name (Var_name R))
                                         (Expr_list ()))))))))
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom fsplit_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_list ((Expr_name (Var_name Z))))
                                       (Expr_name (Var_name Rs))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom fsplit_1_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name R))
                                       (Expr_name (Var_name Rs))
                                       (Expr_name (Var_name Z))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fsplit_1_1)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fsplit_1_1))))
                          (fa_args
                            ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))
                              (Expr_name (Var_name S))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom fsplit_1_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name Z))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))
                                      (Expr_name (Var_name S))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_case
                                 (Expr_apply
                                   ((fa_name (Expr_name (Var_name Fun)))
                                     (fa_args
                                       ((Expr_name (Var_name S))
                                         (Expr_name (Var_name Z))))))
                                 (((c_lhs
                                     ((Pattern_match (Lit_atom (Atom true)))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom fsplit_1))))
                                          (fa_args
                                            ((Expr_name (Var_name Z))
                                              (Expr_name (Var_name S))
                                              (Expr_name (Var_name Fun))
                                              (Expr_name (Var_name L))
                                              (Expr_list ())
                                              (Expr_cons
                                                ((Expr_cons
                                                   ((Expr_name (Var_name Y))
                                                     (Expr_name (Var_name X)))
                                                   (Expr_name (Var_name R))))
                                                (Expr_name (Var_name Rs)))))))))
                                   ((c_lhs
                                      ((Pattern_match (Lit_atom (Atom false)))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom fsplit_1))))
                                           (fa_args
                                             ((Expr_name (Var_name S))
                                               (Expr_name (Var_name Z))
                                               (Expr_name (Var_name Fun))
                                               (Expr_name (Var_name L))
                                               (Expr_list ())
                                               (Expr_cons
                                                 ((Expr_cons
                                                    ((Expr_name (Var_name Y))
                                                      (Expr_name (Var_name X)))
                                                    (Expr_name (Var_name R))))
                                                 (Expr_name (Var_name Rs)))))))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom asc)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom fsplit_2)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fsplit_2))))
                          (fa_args
                            ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom fsplit_2))))
                                  (fa_args
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name Z))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                             (c_guard
                               (((Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom erlang))
                                          (n_name (Atom '==')))))
                                     (fa_args
                                       ((Expr_name (Var_name R))
                                         (Expr_list ()))))))))
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom fsplit_2))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_list ((Expr_name (Var_name Z))))
                                       (Expr_name (Var_name Rs))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom fsplit_2_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name R))
                                       (Expr_name (Var_name Rs))
                                       (Expr_name (Var_name Z))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fsplit_2_1)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fsplit_2_1))))
                          (fa_args
                            ((Expr_name (Var_name Z)) (Expr_name (Var_name Y))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name L))
                              (Expr_cons ((Expr_name (Var_name X)))
                                (Expr_name (Var_name R)))
                              (Expr_name (Var_name Rs))
                              (Expr_name (Var_name S))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom fsplit_2_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Y))
                                      (Expr_name (Var_name Z))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))
                                      (Expr_name (Var_name S))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_case
                                 (Expr_apply
                                   ((fa_name (Expr_name (Var_name Fun)))
                                     (fa_args
                                       ((Expr_name (Var_name S))
                                         (Expr_name (Var_name Z))))))
                                 (((c_lhs
                                     ((Pattern_match (Lit_atom (Atom false)))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom fsplit_2))))
                                          (fa_args
                                            ((Expr_name (Var_name Z))
                                              (Expr_name (Var_name S))
                                              (Expr_name (Var_name Fun))
                                              (Expr_name (Var_name L))
                                              (Expr_list ())
                                              (Expr_cons
                                                ((Expr_cons
                                                   ((Expr_name (Var_name Y))
                                                     (Expr_name (Var_name X)))
                                                   (Expr_name (Var_name R))))
                                                (Expr_name (Var_name Rs)))))))))
                                   ((c_lhs
                                      ((Pattern_match (Lit_atom (Atom true)))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom fsplit_2))))
                                           (fa_args
                                             ((Expr_name (Var_name S))
                                               (Expr_name (Var_name Z))
                                               (Expr_name (Var_name Fun))
                                               (Expr_name (Var_name L))
                                               (Expr_list ())
                                               (Expr_cons
                                                 ((Expr_cons
                                                    ((Expr_name (Var_name Y))
                                                      (Expr_name (Var_name X)))
                                                    (Expr_name (Var_name R))))
                                                 (Expr_name (Var_name Rs)))))))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom desc)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fmergel)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_binding (Var_name T1))
                   (Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2))))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name Fun))
                (Pattern_match (Lit_atom (Atom asc)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name (Expr_name (Atom_name (Atom fmerge2_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name Fun))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                       (Expr_name (Var_name Fun))
                       (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_cons ((Pattern_binding (Var_name H2)))
                     (Pattern_binding (Var_name T2)))
                    (Pattern_binding (Var_name T1)))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom fmerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name Fun))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ()) (Pattern_binding (Var_name _Fun))
                 (Pattern_binding (Var_name _O))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name Fun)) (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rfmergel)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_cons ((Pattern_binding (Var_name H2)))
                    (Pattern_binding (Var_name T2)))
                   (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name Fun))
                (Pattern_match (Lit_atom (Atom asc)))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name (Atom_name (Atom rfmerge2_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name Fun))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                       (Expr_name (Var_name Fun))
                       (Expr_literal (Lit_atom (Atom asc)))))))))
            ((c_lhs
               ((Pattern_cons
                  ((Pattern_binding (Var_name T1))
                    (Pattern_cons ((Pattern_binding (Var_name H2)))
                      (Pattern_binding (Var_name T2))))
                  (Pattern_binding (Var_name L)))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_match (Lit_atom (Atom desc)))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rfmergel))))
                    (fa_args
                      ((Expr_name (Var_name L))
                        (Expr_cons
                          ((Expr_apply
                             ((fa_name
                                (Expr_name (Atom_name (Atom rfmerge2_1))))
                               (fa_args
                                 ((Expr_name (Var_name T1))
                                   (Expr_name (Var_name H2))
                                   (Expr_name (Var_name Fun))
                                   (Expr_name (Var_name T2)) (Expr_list ()))))))
                          (Expr_name (Var_name Acc)))
                        (Expr_name (Var_name Fun))
                        (Expr_literal (Lit_atom (Atom desc)))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name Fun))
                        (Expr_name (Var_name O))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name O))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom fmergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name Fun)) (Expr_name (Var_name O))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% merge/3 "))
    (Module_comment
      (Comment "% Elements from the first list are prioritized."))
    (Function_decl
      ((fd_name (Atom fmerge2_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom fmerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name _Fun))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom fmerge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom fmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom fmerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rmerge/3"))
    (Function_decl
      ((fd_name (Atom rfmerge2_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rfmerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rfmerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name _Fun))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rfmerge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rfmerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2)))
                                (Expr_name (Var_name M)))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rfmerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% usort/2"))
    (Module_comment (Comment "% Ascending. X < Y"))
    (Function_decl
      ((fd_name (Atom ufsplit_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_case
                        (Expr_apply
                          ((fa_name (Expr_name (Var_name Fun)))
                            (fa_args
                              ((Expr_name (Var_name Z))
                                (Expr_name (Var_name Y))))))
                        (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                           (c_guard ())
                           (c_rhs
                             (Expr_comment (Comment " Z equal to Y")
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufsplit_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name R))
                                       (Expr_name (Var_name Rs)))))))))
                          ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom ufsplit_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Z))
                                      (Expr_name (Var_name Y))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))))))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_case
                                (Expr_apply
                                  ((fa_name (Expr_name (Var_name Fun)))
                                    (fa_args
                                      ((Expr_name (Var_name Z))
                                        (Expr_name (Var_name X))))))
                                (((c_lhs
                                    ((Pattern_match (Lit_atom (Atom true)))))
                                   (c_guard ())
                                   (c_rhs
                                     (Expr_comment (Comment " Z equal to X")
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom ufsplit_1))))
                                           (fa_args
                                             ((Expr_name (Var_name Y))
                                               (Expr_name (Var_name X))
                                               (Expr_name (Var_name Fun))
                                               (Expr_name (Var_name L))
                                               (Expr_name (Var_name R))
                                               (Expr_name (Var_name Rs)))))))))
                                  ((c_lhs
                                     ((Pattern_match (Lit_atom (Atom false)))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom ufsplit_1))))
                                          (fa_args
                                            ((Expr_name (Var_name Y))
                                              (Expr_name (Var_name Z))
                                              (Expr_name (Var_name Fun))
                                              (Expr_name (Var_name L))
                                              (Expr_cons
                                                ((Expr_name (Var_name X)))
                                                (Expr_name (Var_name R)))
                                              (Expr_name (Var_name Rs))))))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard
                               (((Expr_apply
                                   ((fa_name
                                      (Expr_name
                                        (Qualified_name (n_mod (Atom erlang))
                                          (n_name (Atom '==')))))
                                     (fa_args
                                       ((Expr_name (Var_name R))
                                         (Expr_list ()))))))))
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufsplit_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_list ((Expr_name (Var_name Z))))
                                       (Expr_name (Var_name Rs))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufsplit_1_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name R))
                                       (Expr_name (Var_name Rs))
                                       (Expr_name (Var_name Z))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R))
                 (Pattern_binding (Var_name Rs))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rufmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_cons
                            ((Expr_name (Var_name Y)) (Expr_name (Var_name X)))
                            (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% X < Y"))
    (Function_decl
      ((fd_name (Atom ufsplit_1_1)) (fd_arity 7)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                (Pattern_binding (Var_name S))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_case
                        (Expr_apply
                          ((fa_name (Expr_name (Var_name Fun)))
                            (fa_args
                              ((Expr_name (Var_name Z))
                                (Expr_name (Var_name Y))))))
                        (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                           (c_guard ())
                           (c_rhs
                             (Expr_comment (Comment " Z equal to Y")
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufsplit_1_1))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name X))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name R))
                                       (Expr_name (Var_name Rs))
                                       (Expr_name (Var_name S)))))))))
                          ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom ufsplit_1_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Z))
                                      (Expr_name (Var_name Y))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L))
                                      (Expr_cons ((Expr_name (Var_name X)))
                                        (Expr_name (Var_name R)))
                                      (Expr_name (Var_name Rs))
                                      (Expr_name (Var_name S))))))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name X))
                                 (Expr_name (Var_name Z))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_case
                                (Expr_apply
                                  ((fa_name (Expr_name (Var_name Fun)))
                                    (fa_args
                                      ((Expr_name (Var_name Z))
                                        (Expr_name (Var_name X))))))
                                (((c_lhs
                                    ((Pattern_match (Lit_atom (Atom true)))))
                                   (c_guard ())
                                   (c_rhs
                                     (Expr_comment (Comment " Z equal to X")
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom ufsplit_1_1))))
                                           (fa_args
                                             ((Expr_name (Var_name Y))
                                               (Expr_name (Var_name X))
                                               (Expr_name (Var_name Fun))
                                               (Expr_name (Var_name L))
                                               (Expr_name (Var_name R))
                                               (Expr_name (Var_name Rs))
                                               (Expr_name (Var_name S)))))))))
                                  ((c_lhs
                                     ((Pattern_match (Lit_atom (Atom false)))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_apply
                                        ((fa_name
                                           (Expr_name
                                             (Atom_name (Atom ufsplit_1_1))))
                                          (fa_args
                                            ((Expr_name (Var_name Y))
                                              (Expr_name (Var_name Z))
                                              (Expr_name (Var_name Fun))
                                              (Expr_name (Var_name L))
                                              (Expr_cons
                                                ((Expr_name (Var_name X)))
                                                (Expr_name (Var_name R)))
                                              (Expr_name (Var_name Rs))
                                              (Expr_name (Var_name S))))))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_case
                                 (Expr_apply
                                   ((fa_name (Expr_name (Var_name Fun)))
                                     (fa_args
                                       ((Expr_name (Var_name S))
                                         (Expr_name (Var_name Z))))))
                                 (((c_lhs
                                     ((Pattern_match (Lit_atom (Atom true)))))
                                    (c_guard ())
                                    (c_rhs
                                      (Expr_case
                                        (Expr_apply
                                          ((fa_name (Expr_name (Var_name Fun)))
                                            (fa_args
                                              ((Expr_name (Var_name Z))
                                                (Expr_name (Var_name S))))))
                                        (((c_lhs
                                            ((Pattern_match
                                               (Lit_atom (Atom true)))))
                                           (c_guard ())
                                           (c_rhs
                                             (Expr_comment
                                               (Comment " Z equal to S")
                                               (Expr_apply
                                                 ((fa_name
                                                    (Expr_name
                                                      (Atom_name
                                                        (Atom ufsplit_1_1))))
                                                   (fa_args
                                                     ((Expr_name (Var_name Y))
                                                       (Expr_name (Var_name X))
                                                       (Expr_name
                                                         (Var_name Fun))
                                                       (Expr_name (Var_name L))
                                                       (Expr_name (Var_name R))
                                                       (Expr_name
                                                         (Var_name Rs))
                                                       (Expr_name (Var_name S)))))))))
                                          ((c_lhs
                                             ((Pattern_match
                                                (Lit_atom (Atom false)))))
                                            (c_guard ())
                                            (c_rhs
                                              (Expr_apply
                                                ((fa_name
                                                   (Expr_name
                                                     (Atom_name
                                                       (Atom ufsplit_1))))
                                                  (fa_args
                                                    ((Expr_name (Var_name Z))
                                                      (Expr_name (Var_name S))
                                                      (Expr_name
                                                        (Var_name Fun))
                                                      (Expr_name (Var_name L))
                                                      (Expr_list ())
                                                      (Expr_cons
                                                        ((Expr_cons
                                                           ((Expr_name
                                                              (Var_name Y))
                                                             (Expr_name
                                                               (Var_name X)))
                                                           (Expr_name
                                                             (Var_name R))))
                                                        (Expr_name
                                                          (Var_name Rs)))))))))))))
                                   ((c_lhs
                                      ((Pattern_match (Lit_atom (Atom false)))))
                                     (c_guard ())
                                     (c_rhs
                                       (Expr_apply
                                         ((fa_name
                                            (Expr_name
                                              (Atom_name (Atom ufsplit_1))))
                                           (fa_args
                                             ((Expr_name (Var_name S))
                                               (Expr_name (Var_name Z))
                                               (Expr_name (Var_name Fun))
                                               (Expr_name (Var_name L))
                                               (Expr_list ())
                                               (Expr_cons
                                                 ((Expr_cons
                                                    ((Expr_name (Var_name Y))
                                                      (Expr_name (Var_name X)))
                                                    (Expr_name (Var_name R))))
                                                 (Expr_name (Var_name Rs)))))))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_binding (Var_name X))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name R)) (Pattern_binding (Var_name Rs))
                 (Pattern_binding (Var_name S))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rufmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_list ((Expr_name (Var_name S))))
                           (Expr_cons
                             ((Expr_name (Var_name Y))
                               (Expr_name (Var_name X)))
                             (Expr_name (Var_name R))))
                         (Expr_name (Var_name Rs)))
                        (Expr_list ()) (Expr_name (Var_name Fun))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% Descending."))
    (Function_decl
      ((fd_name (Atom ufsplit_2)) (fd_arity 4)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Y))
                (Pattern_cons ((Pattern_binding (Var_name Z)))
                  (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name R))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name Y)) (Expr_name (Var_name Z))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_case
                        (Expr_apply
                          ((fa_name (Expr_name (Var_name Fun)))
                            (fa_args
                              ((Expr_name (Var_name Z))
                                (Expr_name (Var_name Y))))))
                        (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                           (c_guard ())
                           (c_rhs
                             (Expr_comment (Comment " Z equal to Y")
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufsplit_2))))
                                   (fa_args
                                     ((Expr_name (Var_name Y))
                                       (Expr_name (Var_name L))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name R)))))))))
                          ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_apply
                                ((fa_name
                                   (Expr_name (Atom_name (Atom ufsplit_1))))
                                  (fa_args
                                    ((Expr_name (Var_name Z))
                                      (Expr_name (Var_name Y))
                                      (Expr_name (Var_name Fun))
                                      (Expr_name (Var_name L)) (Expr_list ())
                                      (Expr_list
                                        ((Expr_apply
                                           ((fa_name
                                              (Expr_name
                                                (Qualified_name
                                                  (n_mod (Atom lists))
                                                  (n_name (Atom reverse)))))
                                             (fa_args
                                               ((Expr_name (Var_name R))
                                                 (Expr_list ())))))))))))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ufsplit_2))))
                           (fa_args
                             ((Expr_name (Var_name Z)) (Expr_name (Var_name L))
                               (Expr_name (Var_name Fun))
                               (Expr_cons ((Expr_name (Var_name Y)))
                                 (Expr_name (Var_name R)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name Y)) (Pattern_list ())
                 (Pattern_binding (Var_name _Fun))
                 (Pattern_binding (Var_name R))))
              (c_guard ())
              (c_rhs
                (Expr_cons ((Expr_name (Var_name Y))) (Expr_name (Var_name R)))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom ufmergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_cons ((Pattern_binding (Var_name H1)))
                    (Pattern_binding (Var_name T1)))
                   (Pattern_binding (Var_name T2)))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name Fun))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom ufmergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name (Atom_name (Atom ufmerge2_2))))
                              (fa_args
                                ((Expr_name (Var_name H1))
                                  (Expr_name (Var_name T1))
                                  (Expr_name (Var_name Fun))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                       (Expr_name (Var_name Fun))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_list ()) (Pattern_binding (Var_name _Fun))))
              (c_guard ()) (c_rhs (Expr_name (Var_name L))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rufmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name Fun))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom rufmergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name Fun))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom rufmergel)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_cons
                 ((Pattern_cons ((Pattern_binding (Var_name H2)))
                    (Pattern_binding (Var_name T2)))
                   (Pattern_binding (Var_name T1)))
                 (Pattern_binding (Var_name L)))
                (Pattern_binding (Var_name Acc))
                (Pattern_binding (Var_name Fun))))
             (c_guard ())
             (c_rhs
               (Expr_apply
                 ((fa_name (Expr_name (Atom_name (Atom rufmergel))))
                   (fa_args
                     ((Expr_name (Var_name L))
                       (Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name (Atom_name (Atom rufmerge2_1))))
                              (fa_args
                                ((Expr_name (Var_name T1))
                                  (Expr_name (Var_name H2))
                                  (Expr_name (Var_name Fun))
                                  (Expr_name (Var_name T2)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                       (Expr_name (Var_name Fun))))))))
            ((c_lhs
               ((Pattern_list ((Pattern_binding (Var_name L))))
                 (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ufmergel))))
                    (fa_args
                      ((Expr_cons
                         ((Expr_apply
                            ((fa_name
                               (Expr_name
                                 (Qualified_name (n_mod (Atom lists))
                                   (n_name (Atom reverse)))))
                              (fa_args
                                ((Expr_name (Var_name L)) (Expr_list ()))))))
                         (Expr_name (Var_name Acc)))
                        (Expr_list ()) (Expr_name (Var_name Fun))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name Acc))
                 (Pattern_binding (Var_name Fun))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name (Expr_name (Atom_name (Atom ufmergel))))
                    (fa_args
                      ((Expr_name (Var_name Acc)) (Expr_list ())
                        (Expr_name (Var_name Fun))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% umerge/3"))
    (Module_comment
      (Comment "% Elements from the first list are kept and prioritized."))
    (Module_comment (Comment "% HdM before H2."))
    (Function_decl
      ((fd_name (Atom ufmerge2_1)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name HdM))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ufmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name H2))
                                 (Expr_name (Var_name HdM))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_comment (Comment " HdM equal to H2")
                                (Expr_apply
                                  ((fa_name
                                     (Expr_name (Atom_name (Atom ufmerge2_2))))
                                    (fa_args
                                      ((Expr_name (Var_name H1))
                                        (Expr_name (Var_name T1))
                                        (Expr_name (Var_name Fun))
                                        (Expr_name (Var_name T2))
                                        (Expr_name (Var_name M)))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom ufmerge2_2))))
                                   (fa_args
                                     ((Expr_name (Var_name H1))
                                       (Expr_name (Var_name T1))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name T2))
                                       (Expr_cons ((Expr_name (Var_name H2)))
                                         (Expr_name (Var_name M)))))))))))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name Fun))
                 (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name HdM))))
              (c_guard ())
              (c_rhs
                (Expr_case
                  (Expr_apply
                    ((fa_name (Expr_name (Var_name Fun)))
                      (fa_args
                        ((Expr_name (Var_name H2)) (Expr_name (Var_name HdM))))))
                  (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_comment (Comment " HdM equal to H2")
                         (Expr_apply
                           ((fa_name
                              (Expr_name
                                (Qualified_name (n_mod (Atom lists))
                                  (n_name (Atom reverse)))))
                             (fa_args
                               ((Expr_name (Var_name T2))
                                 (Expr_name (Var_name M)))))))))
                    ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                      (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom lists))
                                 (n_name (Atom reverse)))))
                            (fa_args
                              ((Expr_name (Var_name T2))
                                (Expr_cons ((Expr_name (Var_name H2)))
                                  (Expr_name (Var_name M)))))))))))))))
        (fd_spec ())))
    (Function_decl
      ((fd_name (Atom ufmerge2_2)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom ufmerge2_1))))
                          (fa_args
                            ((Expr_name (Var_name T1))
                              (Expr_name (Var_name H2))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H1)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H1))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom ufmerge2_2))))
                           (fa_args
                             ((Expr_name (Var_name H1))
                               (Expr_name (Var_name T1))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H2)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name _Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T1))
                        (Expr_cons ((Expr_name (Var_name H1)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% rumerge/3"))
    (Function_decl
      ((fd_name (Atom rufmerge2_1)) (fd_arity 5)
        (fd_cases
          (((c_lhs
              ((Pattern_cons ((Pattern_binding (Var_name H1)))
                 (Pattern_binding (Var_name T1)))
                (Pattern_binding (Var_name H2))
                (Pattern_binding (Var_name Fun))
                (Pattern_binding (Var_name T2)) (Pattern_binding (Var_name M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rufmerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_name (Var_name M))
                              (Expr_name (Var_name H2))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name (Expr_name (Atom_name (Atom rufmerge2_1))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_name (Var_name H2))
                               (Expr_name (Var_name Fun))
                               (Expr_name (Var_name T2))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))))))))))))
            ((c_lhs
               ((Pattern_list ()) (Pattern_binding (Var_name H2))
                 (Pattern_binding (Var_name _Fun))
                 (Pattern_binding (Var_name T2))
                 (Pattern_binding (Var_name M))))
              (c_guard ())
              (c_rhs
                (Expr_apply
                  ((fa_name
                     (Expr_name
                       (Qualified_name (n_mod (Atom lists))
                         (n_name (Atom reverse)))))
                    (fa_args
                      ((Expr_name (Var_name T2))
                        (Expr_cons ((Expr_name (Var_name H2)))
                          (Expr_name (Var_name M)))))))))))
        (fd_spec ())))
    (Module_comment (Comment "% H1 before H2M"))
    (Function_decl
      ((fd_name (Atom rufmerge2_2)) (fd_arity 6)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                (Pattern_binding (Var_name Fun))
                (Pattern_cons ((Pattern_binding (Var_name H2)))
                  (Pattern_binding (Var_name T2)))
                (Pattern_binding (Var_name M))
                (Pattern_binding (Var_name H2M))))
             (c_guard ())
             (c_rhs
               (Expr_case
                 (Expr_apply
                   ((fa_name (Expr_name (Var_name Fun)))
                     (fa_args
                       ((Expr_name (Var_name H1)) (Expr_name (Var_name H2))))))
                 (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                    (c_guard ())
                    (c_rhs
                      (Expr_apply
                        ((fa_name (Expr_name (Atom_name (Atom rufmerge2_2))))
                          (fa_args
                            ((Expr_name (Var_name H1))
                              (Expr_name (Var_name T1))
                              (Expr_name (Var_name Fun))
                              (Expr_name (Var_name T2))
                              (Expr_cons ((Expr_name (Var_name H2M)))
                                (Expr_name (Var_name M)))
                              (Expr_name (Var_name H2))))))))
                   ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_case
                         (Expr_apply
                           ((fa_name (Expr_name (Var_name Fun)))
                             (fa_args
                               ((Expr_name (Var_name H2M))
                                 (Expr_name (Var_name H1))))))
                         (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                            (c_guard ())
                            (c_rhs
                              (Expr_comment (Comment " H2M equal to H1")
                                (Expr_apply
                                  ((fa_name
                                     (Expr_name (Atom_name (Atom rufmerge2_1))))
                                    (fa_args
                                      ((Expr_name (Var_name T1))
                                        (Expr_name (Var_name H2))
                                        (Expr_name (Var_name Fun))
                                        (Expr_name (Var_name T2))
                                        (Expr_cons ((Expr_name (Var_name H1)))
                                          (Expr_name (Var_name M))))))))))
                           ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                             (c_guard ())
                             (c_rhs
                               (Expr_apply
                                 ((fa_name
                                    (Expr_name (Atom_name (Atom rufmerge2_1))))
                                   (fa_args
                                     ((Expr_name (Var_name T1))
                                       (Expr_name (Var_name H2))
                                       (Expr_name (Var_name Fun))
                                       (Expr_name (Var_name T2))
                                       (Expr_cons
                                         ((Expr_name (Var_name H1))
                                           (Expr_name (Var_name H2M)))
                                         (Expr_name (Var_name M)))))))))))))))))
            ((c_lhs
               ((Pattern_binding (Var_name H1)) (Pattern_binding (Var_name T1))
                 (Pattern_binding (Var_name Fun)) (Pattern_list ())
                 (Pattern_binding (Var_name M))
                 (Pattern_binding (Var_name H2M))))
              (c_guard ())
              (c_rhs
                (Expr_case
                  (Expr_apply
                    ((fa_name (Expr_name (Var_name Fun)))
                      (fa_args
                        ((Expr_name (Var_name H2M)) (Expr_name (Var_name H1))))))
                  (((c_lhs ((Pattern_match (Lit_atom (Atom true)))))
                     (c_guard ())
                     (c_rhs
                       (Expr_apply
                         ((fa_name
                            (Expr_name
                              (Qualified_name (n_mod (Atom lists))
                                (n_name (Atom reverse)))))
                           (fa_args
                             ((Expr_name (Var_name T1))
                               (Expr_cons ((Expr_name (Var_name H1)))
                                 (Expr_name (Var_name M)))))))))
                    ((c_lhs ((Pattern_match (Lit_atom (Atom false)))))
                      (c_guard ())
                      (c_rhs
                        (Expr_apply
                          ((fa_name
                             (Expr_name
                               (Qualified_name (n_mod (Atom lists))
                                 (n_name (Atom reverse)))))
                            (fa_args
                              ((Expr_name (Var_name T1))
                                (Expr_cons
                                  ((Expr_name (Var_name H1))
                                    (Expr_name (Var_name H2M)))
                                  (Expr_name (Var_name M)))))))))))))))
        (fd_spec ()))))
