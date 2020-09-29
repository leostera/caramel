  $ cat >small.ml <<EOF
  > let add x y = x + y
  > EOF
  $ caramelc compile --dump-ast --target=core small.ml
  ((m_filename small.core) (m_name small)
    (m_fnames (((fn_name add) (fn_arity 2)))) (m_attributes ())
    (m_defs
      (((fd_name ((fn_name add) (fn_arity 2)))
         (fd_body
           ((fe_vars (X Y)) (fe_arity 2)
             (fe_body
               (Expr_qualified_call (qc_mod (Expr_literal (Lit_atom erlang)))
                 (qc_fun (Expr_literal (Lit_atom +)))
                 (qc_args ((Expr_var X) (Expr_var Y)))))))))))
  
  Compiling small.core	OK
  $ cat small.core
  % Source code generated with Caramel.
  module 'small' [
    'add'/2,
    'module_info'/0,
    'module_info'/1]
  attributes []
  
  'add'/2 = fun (_X,_Y) -> call 'erlang':'+'(_X,_Y)'module_info'/0 =
    ( fun () ->
    call 'erlang':'get_module_info'
        ('expected')
      -| [{'function',{'module_info',0; }; }] )
  
  'module_info'/1 =
    ( fun (_0) ->
    call 'erlang':'get_module_info'
        ('expected', ( _0
           -| [{'function',{'module_info',1; }; }] ))
      -| [{'function',{'module_info',1; }; }] )
  
  end 
