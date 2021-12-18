  $ caramel compile --dump-ast empty.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (empty.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file empty.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file empty.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating empty.ml (module Empty)
  caramel: [DEBUG] Creating module: Caramel.Empty
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Found 0 inner modules
  caramel: [DEBUG] Writing Caramel.Empty.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Empty.core	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Empty.core Caramel.Empty.core.ast
  % Source code generated with Caramel.
  -module('Caramel.Empty').
  
  
  
  
  
  % Source code generated with Caramel.
  ((attributes ()) (behaviours ()) (functions ())
    (module_name (Atom () 'Caramel.Empty')) (file_name Caramel.Empty.core)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --dump-ast include.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (include.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file include.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          include.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating include.ml (module Include)
  caramel: [DEBUG] Creating module: Caramel.Include
  caramel: [DEBUG] Found 2 inlined functions from included modules
  caramel: [DEBUG] Found 0 inner modules
  caramel: [DEBUG] Writing Caramel.Include.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Include.core	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Include.core Caramel.Include.core.ast
  % Source code generated with Caramel.
  -module('Caramel.Include').
  
  -export([f/1]).
  -export([g/1]).
  -export([run/1]).
  
  f({}) -> 0.
  
  g({}) -> 1.
  
  run({}) -> 1.
  
  
  % Source code generated with Caramel.
  ((attributes
     ((Export_fun (attr_fun_name (Atom () f)) (attr_fun_arity 1))
       (Export_fun (attr_fun_name (Atom () g)) (attr_fun_arity 1))
       (Export_fun (attr_fun_name (Atom () run)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () f)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 0)))))))
        ((fd_name (Atom () g)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
               (c_rhs (Expr_term (Term_integer () 1)))))))
        ((fd_name (Atom () run)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
               (c_rhs (Expr_term (Term_integer () 1)))))))))
    (module_name (Atom () 'Caramel.Include')) (file_name Caramel.Include.core)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --dump-ast let_open.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (let_open.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file let_open.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          let_open.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating let_open.ml (module Let_open)
  caramel: [DEBUG] Creating module: Caramel.Let_open
  caramel: [DEBUG] Found 0 inlined functions from included modules
  Oops! This function has not been implemented yet: from_ocaml_open
  [1]
  $ cat Caramel.Let_open.core Caramel.Let_open.core.ast
  cat: Caramel.Let_open.core: No such file or directory
  cat: Caramel.Let_open.core.ast: No such file or directory
  [1]

  $ caramel compile --dump-ast nested.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (nested.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file nested.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file nested.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating nested.ml (module Nested)
  caramel: [DEBUG] Creating module: Caramel.Nested
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Creating module: Caramel.Nested.B
  caramel: [DEBUG] Found 0 inlined functions from included modules
  Oops! This function has not been implemented yet: from_ocaml_function
  [1]
  $ cat Caramel.Nested.core Caramel.Nested.core.ast
  cat: Caramel.Nested.core: No such file or directory
  cat: Caramel.Nested.core.ast: No such file or directory
  [1]

  $ caramel compile --dump-ast no_exports.mli no_exports.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (no_exports.mli no_exports.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file no_exports.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file no_exports.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          no_exports.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating no_exports.ml (module No_exports)
  caramel: [DEBUG] Creating module: Caramel.No_exports
  caramel: [DEBUG] Found 0 inlined functions from included modules
  Oops! This function has not been implemented yet: from_ocaml_function
  [1]
  $ cat Caramel.No_exports.core Caramel.No_exports.core.ast
  cat: Caramel.No_exports.core: No such file or directory
  cat: Caramel.No_exports.core.ast: No such file or directory
  [1]

  $ caramel compile --dump-ast sig.mli sig.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (sig.mli sig.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file sig.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file sig.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file sig.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating sig.ml (module Sig)
  caramel: [DEBUG] Creating module: Caramel.Sig
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Found 0 inner modules
  caramel: [DEBUG] Writing Caramel.Sig.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Sig.core	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Sig.core Caramel.Sig.core.ast
  % Source code generated with Caramel.
  -module('Caramel.Sig').
  
  -export([inc/1]).
  
  inc(Caramel@x) -> {x,1}.
  
  hidden({}) -> {}.
  
  secret({}) -> {}.
  
  
  % Source code generated with Caramel.
  ((attributes ((Export_fun (attr_fun_name (Atom () inc)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () inc)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_var (Name_var () Caramel@x))))
              (c_rhs
                (Expr_term
                  (Term_tuple
                    ((tup_size 2)
                      (tup_elements
                        ((Expr_symbol (Sym_local (Name_atom (Atom () x))))
                          (Expr_term (Term_integer () 1))))))))))))
        ((fd_name (Atom () hidden)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
               (c_rhs
                 (Expr_term (Term_tuple ((tup_size 0) (tup_elements ())))))))))
        ((fd_name (Atom () secret)) (fd_arity 1)
          (fd_cases
            (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
               (c_rhs
                 (Expr_term (Term_tuple ((tup_size 0) (tup_elements ())))))))))))
    (module_name (Atom () 'Caramel.Sig')) (file_name Caramel.Sig.core) (ctx ())
    (mod_ctx ()))
  

  $ caramel compile --dump-ast sig_dep.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (sig_dep.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file sig_dep.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          sig_dep.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating sig_dep.ml (module Sig_dep)
  caramel: [DEBUG] Creating module: Caramel.Sig_dep
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Found 0 inner modules
  caramel: [DEBUG] Writing Caramel.Sig_dep.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Sig_dep.core	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Sig_dep.core Caramel.Sig_dep.core.ast
  % Source code generated with Caramel.
  -module('Caramel.Sig_dep').
  
  
  
  
  
  % Source code generated with Caramel.
  ((attributes ()) (behaviours ()) (functions ())
    (module_name (Atom () 'Caramel.Sig_dep')) (file_name Caramel.Sig_dep.core)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --dump-ast simple_nested.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (simple_nested.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file
                                                     simple_nested.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          simple_nested.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating simple_nested.ml (module Simple_nested)
  caramel: [DEBUG] Creating module: Caramel.Simple_nested
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Creating module: Caramel.Simple_nested.B
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Creating module: Caramel.Simple_nested.A
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Found 2 inner modules
  caramel: [DEBUG] Writing Caramel.Simple_nested.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Simple_nested.B.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Simple_nested.A.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Simple_nested.core	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Simple_nested.B.core	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Simple_nested.A.core	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Simple_nested.core Caramel.Simple_nested.core.ast
  % Source code generated with Caramel.
  -module('Caramel.Simple_nested').
  
  -export([run/1]).
  
  run({}) -> {'Caramel.A':f({}),'Caramel.B':f({})}.
  
  
  % Source code generated with Caramel.
  ((attributes ((Export_fun (attr_fun_name (Atom () run)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () run)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs
                (Expr_term
                  (Term_tuple
                    ((tup_size 2)
                      (tup_elements
                        ((Expr_fun_call
                           ((fncall_name
                              (Expr_symbol
                                (Sym_qualified
                                  (sym_mod (Name_atom (Atom () 'Caramel.A')))
                                  (sym_name (Name_atom (Atom () f))))))
                             (fncall_args
                               ((Expr_term
                                  (Term_tuple ((tup_size 0) (tup_elements ()))))))))
                          (Expr_fun_call
                            ((fncall_name
                               (Expr_symbol
                                 (Sym_qualified
                                   (sym_mod (Name_atom (Atom () 'Caramel.B')))
                                   (sym_name (Name_atom (Atom () f))))))
                              (fncall_args
                                ((Expr_term
                                   (Term_tuple
                                     ((tup_size 0) (tup_elements ()))))))))))))))))))))
    (module_name (Atom () 'Caramel.Simple_nested'))
    (file_name Caramel.Simple_nested.core) (ctx ()) (mod_ctx ()))
  
