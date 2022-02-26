  $ cat main.caramel
  pub macro list_of_lists(args) {
    quote {
      [ [1, 2], unquote(args), [2, 3] ]
    }
  }
  
  pub macro flat_list(args) {
    quote {
      [ 1, unquote_splicing(args), 2 ]
    }
  }
  
  pub fn main(args) {
    let a = list_of_lists([1, 2]);
    flat_list([args, args])
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id list_of_lists) Parens_left (Id args)
                     Parens_right Brace_left Quote Brace_left Bracket_left
                     Bracket_left (Integer 1) Comma (Integer 2) Bracket_right
                     Comma Unquote Parens_left (Id args) Parens_right Comma
                     Bracket_left (Integer 2) Comma (Integer 3) Bracket_right
                     Bracket_right Brace_right Brace_right Pub Macro
                     (Id flat_list) Parens_left (Id args) Parens_right
                     Brace_left Quote Brace_left Bracket_left (Integer 1) Comma
                     Unquote_splicing Parens_left (Id args) Parens_right Comma
                     (Integer 2) Bracket_right Brace_right Brace_right Pub Fn
                     (Id main) Parens_left (Id args) Parens_right Brace_left
                     Let (Id a) Equal (Id list_of_lists) Parens_left
                     Bracket_left (Integer 1) Comma (Integer 2) Bracket_right
                     Parens_right Semicolon (Id flat_list) Parens_left
                     Bracket_left (Id args) Comma (Id args) Bracket_right
                     Parens_right Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (list_of_lists)))
                        (fn_args ((No_label (Pat_bind (Id (args))))))
                        (fn_arity 1)
                        (fn_body
                          (Expr_quote
                            ((Quasiquote
                               (Bracket_left Bracket_left (Integer 1) Comma
                                 (Integer 2) Bracket_right Comma))
                              (Unquote (Expr_var (Id (args))))
                              (Quasiquote
                                (Comma Bracket_left (Integer 2) Comma
                                  (Integer 3) Bracket_right Bracket_right)))))
                        (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (flat_list)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_quote
                             ((Quasiquote (Bracket_left (Integer 1) Comma))
                               (Unquote_splicing (Expr_var (Id (args))))
                               (Quasiquote (Comma (Integer 2) Bracket_right)))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_let (Pat_bind (Id (a)))
                             (Expr_call (Expr_var (Id (list_of_lists)))
                               ((Expr_cons (Expr_literal (Lit_integer 1))
                                  (Expr_cons (Expr_literal (Lit_integer 2))
                                    Expr_nil))))
                             (Expr_call (Expr_var (Id (flat_list)))
                               ((Expr_cons (Expr_var (Id (args)))
                                  (Expr_cons (Expr_var (Id (args))) Expr_nil))))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (list_of_lists)))
                                          (fn_args
                                            ((No_label (Pat_bind (Id (args))))))
                                          (fn_arity 1)
                                          (fn_body
                                            (Expr_quote
                                              ((Quasiquote
                                                 (Bracket_left Bracket_left
                                                   (Integer 1) Comma
                                                   (Integer 2) Bracket_right
                                                   Comma))
                                                (Unquote
                                                  (Expr_var (Id (args))))
                                                (Quasiquote
                                                  (Comma Bracket_left
                                                    (Integer 2) Comma
                                                    (Integer 3) Bracket_right
                                                    Bracket_right)))))
                                          (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (flat_list)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_quote
                                               ((Quasiquote
                                                  (Bracket_left (Integer 1)
                                                    Comma))
                                                 (Unquote_splicing
                                                   (Expr_var (Id (args))))
                                                 (Quasiquote
                                                   (Comma (Integer 2)
                                                     Bracket_right)))))
                                           (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (main)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_let (Pat_bind (Id (a)))
                                               (Expr_cons
                                                 (Expr_cons
                                                   (Expr_literal
                                                     (Lit_integer 1))
                                                   (Expr_cons
                                                     (Expr_literal
                                                       (Lit_integer 2))
                                                     Expr_nil))
                                                 (Expr_cons
                                                   (Expr_cons
                                                     (Expr_literal
                                                       (Lit_integer 1))
                                                     (Expr_cons
                                                       (Expr_literal
                                                         (Lit_integer 2))
                                                       Expr_nil))
                                                   (Expr_cons
                                                     (Expr_cons
                                                       (Expr_literal
                                                         (Lit_integer 2))
                                                       (Expr_cons
                                                         (Expr_literal
                                                           (Lit_integer 3))
                                                         Expr_nil))
                                                     Expr_nil)))
                                               (Expr_cons
                                                 (Expr_literal (Lit_integer 1))
                                                 (Expr_cons
                                                   (Expr_var (Id (args)))
                                                   (Expr_cons
                                                     (Expr_var (Id (args)))
                                                     (Expr_cons
                                                       (Expr_literal
                                                         (Lit_integer 2))
                                                       Expr_nil))))))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] let rec main args =
                     let a = [[1; 2]; [1; 2]; [2; 3]] in [1; args; args; 2]

  $ caramel compile main.caramel --new-syntax --debug
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main.caramel.parsetree
  caramel: [DEBUG] OK
  File "_none_", line 1:
  Warning 26 [unused-var]: unused variable a.
  caramel: [DEBUG] Writing main.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing main.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ cat main.caramel.lambda
  (letrec
    (main/3
       (function args/4[int]
         (let
           (a/5 =
              [0: [0: 1 [0: 2 0]] [0: [0: 1 [0: 2 0]] [0: [0: 2 [0: 3 0]] 0]]])
           (makeblock 0 (int,*) 1
             (makeblock 0 (int,*) args/4 (makeblock 0 (int,*) args/4 [0: 2 0]))))))
    (makeblock 0 main/3))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_3) (source_name main))
            (Ir_fun (((path ()) (unique_name args_4) (source_name args)))
              (Ir_let Private ((path ()) (unique_name a_5) (source_name a))
                (Ir_cons
                  (Ir_cons (Ir_lit (Lit_int 1))
                    (Ir_cons (Ir_lit (Lit_int 2)) Ir_nil))
                  (Ir_cons
                    (Ir_cons (Ir_lit (Lit_int 1))
                      (Ir_cons (Ir_lit (Lit_int 2)) Ir_nil))
                    (Ir_cons
                      (Ir_cons (Ir_lit (Lit_int 2))
                        (Ir_cons (Ir_lit (Lit_int 3)) Ir_nil))
                      Ir_nil)))
                (Ir_cons (Ir_lit (Lit_int 1))
                  (Ir_cons
                    (Ir_var
                      ((path ()) (unique_name args_4) (source_name args)))
                    (Ir_cons
                      (Ir_var
                        ((path ()) (unique_name args_4) (source_name args)))
                      (Ir_cons (Ir_lit (Lit_int 2)) Ir_nil))))))))
         (Ir_tuple
           ((Ir_var ((path ()) (unique_name main_3) (source_name main)))))))))

  $ cat main.caramel.b_0
  (Module (name Caramel.Main)
    (defs
      (((df_name (module_info 0))
         (df_body
           (Fun
             ((args ())
               (body
                 (Call (mod_ erlang) (fun_ get_module_info)
                   (args ((Literal (Lit_atom Caramel.Main))))))))))
        ((df_name (module_info 1))
          (df_body
            (Fun
              ((args (Opts))
                (body
                  (Call (mod_ erlang) (fun_ get_module_info)
                    (args ((Literal (Lit_atom Caramel.Main)) (Var Opts)))))))))
        ((df_name (main 1))
          (df_body
            (Fun
              ((args (args))
                (body
                  (Let (value_list (a))
                    (expr
                      (List
                        (List (Literal (Lit_int 1))
                          (List (Literal (Lit_int 2)) (Literal Lit_nil)))
                        (List
                          (List (Literal (Lit_int 1))
                            (List (Literal (Lit_int 2)) (Literal Lit_nil)))
                          (List
                            (List (Literal (Lit_int 2))
                              (List (Literal (Lit_int 3)) (Literal Lit_nil)))
                            (Literal Lit_nil)))))
                    (body
                      (List (Literal (Lit_int 1))
                        (List (Var args)
                          (List (Var args)
                            (List (Literal (Lit_int 2)) (Literal Lit_nil))))))))))))))
    (exports ((main 1) (module_info 0) (module_info 1))))

  $ cat Caramel.Main.core
  % Source code generated with Caramel.
  module 'Caramel.Main'
  [
   'main'/1,
   'module_info'/0,
   'module_info'/1
  ]
  attributes []
  
  'module_info'/0 =
   (fun () -> call 'erlang':'get_module_info'('Caramel.Main') -| [])
  
  'module_info'/1 =
   (fun (Opts) -> call 'erlang':'get_module_info'('Caramel.Main', Opts) -| [])
  
  'main'/1 =
   (fun (Args) ->
   let <A> = [ [ 1|[ 2|[] ] ]|[ [ 1|[ 2|[] ] ]|[ [ 2|[ 3|[] ] ]|[] ] ] ] in
     [ 1|[ Args|[ Args|[ 2|[] ] ] ] ] -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam
