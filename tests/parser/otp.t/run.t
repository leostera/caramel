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
        (typ_params ((Var_name Subject) (Var_name Pos)))))
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
        (typ_params ((Var_name Subject)))))
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
        (typ_params ((Var_name Subject) (Var_name PosLen)))))
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
        (typ_params ((Var_name Subject) (Var_name Pos) (Var_name Len)))))
    (Function_decl
      ((fd_name (Atom bin_to_list)) (fd_arity 3)
        (fd_cases
          (((c_lhs
              ((Pattern_binding (Var_name Subject))
                (Pattern_binding (Var_name Pos))
                (Pattern_binding (Var_name Len))))
             (c_guard
               (((Expr_apply
                   ((fa_name (Expr_name (Atom_name (Atom is_binary))))
                     (fa_args ((Expr_name (Var_name Subject))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Pos))))))
                  (Expr_apply
                    ((fa_name (Expr_name (Atom_name (Atom is_integer))))
                      (fa_args ((Expr_name (Var_name Len)))))))))
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
        (typ_params ((Var_name Pattern)))))
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
        (typ_params ((Var_name Subject)))))
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
        (typ_params ((Var_name Subject) (Var_name N)))))
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
        (typ_name (Atom decode_unsigned)) (typ_params ((Var_name Subject)))))
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
        (typ_params ((Var_name Subject) (Var_name Endianness)))))
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
        (typ_params ((Var_name Unsigned)))))
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
        (typ_params ((Var_name Unsigned) (Var_name Endianness)))))
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
        (typ_params ((Var_name Subject)))))
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
        (typ_params ((Var_name Subject)))))
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
        (typ_params ((Var_name ByteList)))))
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
        (typ_params ((Var_name Binaries)))))
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
        (typ_params ((Var_name Binaries)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern) (Var_name Options)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern) (Var_name Options)))))
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
        (typ_params ((Var_name Subject) (Var_name PosLen)))))
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
        (typ_params ((Var_name Subject) (Var_name Pos) (Var_name Len)))))
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
        (typ_params ((Var_name Binary)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern)))))
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
        (typ_params ((Var_name Subject) (Var_name Pattern) (Var_name Options)))))
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
          ((Var_name Subject) (Var_name Pattern) (Var_name Replacement)))))
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
          ((Var_name Subject) (Var_name Pattern) (Var_name Replacement)
            (Var_name Options)))))
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
                                    (((Expr_name (Var_name Global))
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
                                      ((Expr_literal (Lit_atom (Atom true)))
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
                      (((Expr_apply
                          ((fa_name (Expr_name (Atom_name (Atom is_list))))
                            (fa_args ((Expr_name (Var_name Replacement))))))
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
                        ((Expr_literal (Lit_atom (Atom true)))
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
  $ echo 0
  0
