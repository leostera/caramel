  $ cat main.caramel
  pub macro go(direction, steps) {
    match steps {
    | 0 -> quote { [:done] }
    | _ ->
      quote {
        [ unquote(direction), ...unquote(go(direction, steps - 1)) ]
      }
    }
  }
  
  pub macro comptime_if(cond, on_true, on_false) {
    match cond {
    | :true -> quote { unquote(on_true) }
    | :false -> quote { unquote(on_false) }
    }
  }
  
  pub fn main(args) {
    let direction = comptime_if( :true, :up, :down );
    go(direction, 3)
  }

  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (Pub Macro (Id go) Parens_left (Id direction) Comma
                     (Id steps) Parens_right Brace_left Match (Id steps)
                     Brace_left Pipe (Integer 0) Arrow Quote Brace_left
                     Bracket_left (Atom done) Bracket_right Brace_right Pipe
                     Any Arrow Quote Brace_left Bracket_left Unquote
                     Parens_left (Id direction) Parens_right Comma Dot_dot_dot
                     Unquote Parens_left (Id go) Parens_left (Id direction)
                     Comma (Id steps) Dash (Integer 1) Parens_right
                     Parens_right Bracket_right Brace_right Brace_right
                     Brace_right Pub Macro (Id comptime_if) Parens_left
                     (Id cond) Comma (Id on_true) Comma (Id on_false)
                     Parens_right Brace_left Match (Id cond) Brace_left Pipe
                     (Atom true) Arrow Quote Brace_left Unquote Parens_left
                     (Id on_true) Parens_right Brace_right Pipe (Atom false)
                     Arrow Quote Brace_left Unquote Parens_left (Id on_false)
                     Parens_right Brace_right Brace_right Brace_right Pub Fn
                     (Id main) Parens_left (Id args) Parens_right Brace_left
                     Let (Id direction) Equal (Id comptime_if) Parens_left
                     (Atom true) Comma (Atom up) Comma (Atom down) Parens_right
                     Semicolon (Id go) Parens_left (Id direction) Comma
                     (Integer 3) Parens_right Brace_right)

  $ caramel parse --file main.caramel --dump-parsetree --debug
  caramel: [DEBUG] ((Str_macro
                      ((fn_visibility Public) (fn_name (Id (go)))
                        (fn_args
                          ((No_label (Pat_bind (Id (direction))))
                            (No_label (Pat_bind (Id (steps))))))
                        (fn_arity 2)
                        (fn_body
                          (Expr_match (Expr_var (Id (steps)))
                            (((cs_lhs (Pat_literal (Lit_integer 0)))
                               (cs_rhs
                                 (Expr_quote
                                   ((Quasiquote
                                      (Bracket_left (Atom done) Bracket_right))))))
                              ((cs_lhs Pat_any)
                                (cs_rhs
                                  (Expr_quote
                                    ((Quasiquote (Bracket_left))
                                      (Unquote (Expr_var (Id (direction))))
                                      (Quasiquote (Comma Dot_dot_dot))
                                      (Unquote
                                        (Expr_call (Expr_var (Id (go)))
                                          ((Expr_var (Id (direction)))
                                            (Expr_call (Expr_var (Id (-)))
                                              ((Expr_var (Id (steps)))
                                                (Expr_literal (Lit_integer 1)))))))
                                      (Quasiquote (Bracket_right)))))))))
                        (fn_annot ())))
                     (Str_macro
                       ((fn_visibility Public) (fn_name (Id (comptime_if)))
                         (fn_args
                           ((No_label (Pat_bind (Id (cond))))
                             (No_label (Pat_bind (Id (on_true))))
                             (No_label (Pat_bind (Id (on_false))))))
                         (fn_arity 3)
                         (fn_body
                           (Expr_match (Expr_var (Id (cond)))
                             (((cs_lhs (Pat_literal (Lit_atom true)))
                                (cs_rhs
                                  (Expr_quote
                                    ((Quasiquote ())
                                      (Unquote (Expr_var (Id (on_true))))
                                      (Quasiquote ())))))
                               ((cs_lhs (Pat_literal (Lit_atom false)))
                                 (cs_rhs
                                   (Expr_quote
                                     ((Quasiquote ())
                                       (Unquote (Expr_var (Id (on_false))))
                                       (Quasiquote ()))))))))
                         (fn_annot ())))
                     (Str_fun
                       ((fn_visibility Public) (fn_name (Id (main)))
                         (fn_args ((No_label (Pat_bind (Id (args))))))
                         (fn_arity 1)
                         (fn_body
                           (Expr_let (Pat_bind (Id (direction)))
                             (Expr_call (Expr_var (Id (comptime_if)))
                               ((Expr_literal (Lit_atom true))
                                 (Expr_literal (Lit_atom up))
                                 (Expr_literal (Lit_atom down))))
                             (Expr_call (Expr_var (Id (go)))
                               ((Expr_var (Id (direction)))
                                 (Expr_literal (Lit_integer 3))))))
                         (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-expanded --dump-macro-env --debug
  caramel: [DEBUG] Expanded program: ((Str_macro
                                        ((fn_visibility Public)
                                          (fn_name (Id (go)))
                                          (fn_args
                                            ((No_label
                                               (Pat_bind (Id (direction))))
                                              (No_label
                                                (Pat_bind (Id (steps))))))
                                          (fn_arity 2)
                                          (fn_body
                                            (Expr_match (Expr_var (Id (steps)))
                                              (((cs_lhs
                                                  (Pat_literal (Lit_integer 0)))
                                                 (cs_rhs
                                                   (Expr_quote
                                                     ((Quasiquote
                                                        (Bracket_left
                                                          (Atom done)
                                                          Bracket_right))))))
                                                ((cs_lhs Pat_any)
                                                  (cs_rhs
                                                    (Expr_quote
                                                      ((Quasiquote
                                                         (Bracket_left))
                                                        (Unquote
                                                          (Expr_var
                                                            (Id (direction))))
                                                        (Quasiquote
                                                          (Comma Dot_dot_dot))
                                                        (Unquote
                                                          (Expr_call
                                                            (Expr_var
                                                              (Id (go)))
                                                            ((Expr_var
                                                               (Id (direction)))
                                                              (Expr_call
                                                                (Expr_var
                                                                  (Id (-)))
                                                                ((Expr_var
                                                                   (Id (steps)))
                                                                  (Expr_literal
                                                                    (Lit_integer
                                                                      1)))))))
                                                        (Quasiquote
                                                          (Bracket_right)))))))))
                                          (fn_annot ())))
                                       (Str_macro
                                         ((fn_visibility Public)
                                           (fn_name (Id (comptime_if)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (cond))))
                                               (No_label
                                                 (Pat_bind (Id (on_true))))
                                               (No_label
                                                 (Pat_bind (Id (on_false))))))
                                           (fn_arity 3)
                                           (fn_body
                                             (Expr_match (Expr_var (Id (cond)))
                                               (((cs_lhs
                                                   (Pat_literal
                                                     (Lit_atom true)))
                                                  (cs_rhs
                                                    (Expr_quote
                                                      ((Quasiquote ())
                                                        (Unquote
                                                          (Expr_var
                                                            (Id (on_true))))
                                                        (Quasiquote ())))))
                                                 ((cs_lhs
                                                    (Pat_literal
                                                      (Lit_atom false)))
                                                   (cs_rhs
                                                     (Expr_quote
                                                       ((Quasiquote ())
                                                         (Unquote
                                                           (Expr_var
                                                             (Id (on_false))))
                                                         (Quasiquote ()))))))))
                                           (fn_annot ())))
                                       (Str_fun
                                         ((fn_visibility Public)
                                           (fn_name (Id (main)))
                                           (fn_args
                                             ((No_label (Pat_bind (Id (args))))))
                                           (fn_arity 1)
                                           (fn_body
                                             (Expr_let
                                               (Pat_bind (Id (direction)))
                                               (Expr_literal (Lit_atom up))
                                               (Expr_cons
                                                 (Expr_var (Id (direction)))
                                                 (Expr_cons
                                                   (Expr_var (Id (direction)))
                                                   (Expr_cons
                                                     (Expr_var
                                                       (Id (direction)))
                                                     (Expr_cons
                                                       (Expr_literal
                                                         (Lit_atom done))
                                                       Expr_nil))))))
                                           (fn_annot ()))))

  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] let rec main args =
                     let direction = `up in
                     [direction; direction; direction; `done]

  $ caramel compile main.caramel --new-syntax --debug
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main.caramel.parsetree
  caramel: [DEBUG] OK
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
       (function args/4
         (let (direction/5 = 26203)
           (makeblock 0 direction/5
             (makeblock 0 direction/5
               (makeblock 0 direction/5 [0: -1032982398 0]))))))
    (makeblock 0 main/3))

  $ cat main.caramel.ir
  (Ir_program
    ((Ir_module
       ((path ()) (unique_name Caramel.Main) (source_name Caramel.Main))
       (Ir_letrec
         ((Exported ((path ()) (unique_name main_3) (source_name main))
            (Ir_fun (((path ()) (unique_name args_4) (source_name args)))
              (Ir_let Private
                ((path ()) (unique_name direction_5) (source_name direction))
                (Ir_lit (Lit_atom up))
                (Ir_cons
                  (Ir_var
                    ((path ()) (unique_name direction_5)
                      (source_name direction)))
                  (Ir_cons
                    (Ir_var
                      ((path ()) (unique_name direction_5)
                        (source_name direction)))
                    (Ir_cons
                      (Ir_var
                        ((path ()) (unique_name direction_5)
                          (source_name direction)))
                      (Ir_cons (Ir_lit (Lit_atom done)) Ir_nil))))))))
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
                  (Let (value_list (direction)) (expr (Literal (Lit_atom up)))
                    (body
                      (List (Var direction)
                        (List (Var direction)
                          (List (Var direction)
                            (List (Literal (Lit_atom done)) (Literal Lit_nil))))))))))))))
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
   let <Direction> = 'up' in
     [ Direction|[ Direction|[ Direction|[ 'done'|[] ] ] ] ] -| [])
  end
  

  $ erlc *.core

  $ escript Caramel.Main.beam
