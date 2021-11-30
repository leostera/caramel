  $ caramel compile --sugarcane --dump-ast empty.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (empty.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file empty.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file empty.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: empty.ml (module Empty)
  caramel: [DEBUG] Creating module: Caramel.Empty
  caramel: [DEBUG] Writing Caramel.Empty.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Empty.erl	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Empty.erl Caramel.Empty.erl.ast
  % Source code generated with Caramel.
  -module('Caramel.Empty').
  % Module attributes:
  
  
  % Function declarations:
  
  
  ((attributes ()) (behaviours ()) (functions ())
    (module_name (Atom () 'Caramel.Empty')) (file_name Caramel.Empty.erl)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --sugarcane --dump-ast include.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (include.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file include.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          include.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: include.ml (module Include)
  caramel: [DEBUG] Creating module: Caramel.Include
  caramel: [DEBUG] Writing Caramel.Include.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Include.erl	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Include.erl Caramel.Include.erl.ast
  % Source code generated with Caramel.
  -module('Caramel.Include').
  % Module attributes:
  -export([f/1]).
  -export([g/1]).
  -export([run/1]).
  
  % Function declarations:
  f({}) -> 0.
  
  g({}) -> 1.
  
  run({}) -> 1.
  
  
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
    (module_name (Atom () 'Caramel.Include')) (file_name Caramel.Include.erl)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --sugarcane --dump-ast let_open.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (let_open.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file let_open.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          let_open.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: let_open.ml (module Let_open)
  caramel: [DEBUG] Creating module: Caramel.Let_open
  [1]
  $ cat Caramel.Let_open.erl Caramel.Let_open.erl.ast
  cat: Caramel.Let_open.erl: No such file or directory
  cat: Caramel.Let_open.erl.ast: No such file or directory
  [1]

  $ caramel compile --sugarcane --dump-ast nested.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (nested.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file nested.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file nested.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: nested.ml (module Nested)
  caramel: [DEBUG] Creating module: Caramel.Nested
  caramel: [DEBUG] Creating module: Caramel.Nested.A
  Oops! This function has not been implemented yet.
  [1]
  $ cat Caramel.Nested.erl Caramel.Nested.erl.ast
  cat: Caramel.Nested.erl: No such file or directory
  cat: Caramel.Nested.erl.ast: No such file or directory
  [1]

  $ caramel compile --sugarcane --dump-ast no_exports.mli no_exports.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (no_exports.mli no_exports.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file no_exports.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file no_exports.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          no_exports.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: no_exports.ml (module No_exports)
  caramel: [DEBUG] Creating module: Caramel.No_exports
  Oops! This function has not been implemented yet.
  [1]
  $ cat Caramel.No_exports.erl Caramel.No_exports.erl.ast
  cat: Caramel.No_exports.erl: No such file or directory
  cat: Caramel.No_exports.erl.ast: No such file or directory
  [1]

  $ caramel compile --sugarcane --dump-ast sig.mli sig.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (sig.mli sig.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file sig.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file sig.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file sig.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: sig.ml (module Sig)
  caramel: [DEBUG] Creating module: Caramel.Sig
  Oops! This function has not been implemented yet.
  [1]
  $ cat Caramel.Sig.erl Caramel.Sig.erl.ast
  cat: Caramel.Sig.erl: No such file or directory
  cat: Caramel.Sig.erl.ast: No such file or directory
  [1]

  $ caramel compile --sugarcane --dump-ast sig_dep.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (sig_dep.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file sig_dep.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          sig_dep.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: sig_dep.ml (module Sig_dep)
  caramel: [DEBUG] Creating module: Caramel.Sig_dep
  caramel: [DEBUG] Writing Caramel.Sig_dep.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.Sig_dep.erl	
  caramel: [DEBUG] OK
  
  $ cat Caramel.Sig_dep.erl Caramel.Sig_dep.erl.ast
  % Source code generated with Caramel.
  -module('Caramel.Sig_dep').
  % Module attributes:
  
  
  % Function declarations:
  
  
  ((attributes ()) (behaviours ()) (functions ())
    (module_name (Atom () 'Caramel.Sig_dep')) (file_name Caramel.Sig_dep.erl)
    (ctx ()) (mod_ctx ()))
  

  $ caramel compile --sugarcane --dump-ast simple_nested.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (simple_nested.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling implementation unit: ((source_file
                                                     simple_nested.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file
                                                          simple_nested.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: simple_nested.ml (module Simple_nested)
  caramel: [DEBUG] Creating module: Caramel.Simple_nested
  Oops! This function has not been implemented yet.
  [1]
  $ cat Caramel.Simple_nested.erl Caramel.Simple_nested.erl.ast
  cat: Caramel.Simple_nested.erl: No such file or directory
  cat: Caramel.Simple_nested.erl.ast: No such file or directory
  [1]
