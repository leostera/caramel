  $ caramel compile --dump-ast a.mli a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.mli a.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/sugarcane/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file a.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file a.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file a.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating a.ml (module A)
  caramel: [DEBUG] Creating module: Caramel.A
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Creating module: Caramel.A.B
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Creating module: Caramel.A.B.C
  caramel: [DEBUG] Found 0 inlined functions from included modules
  caramel: [DEBUG] Found 2 inner modules
  caramel: [DEBUG] Writing Caramel.A.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.C.core.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.core	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.core	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.C.core	
  caramel: [DEBUG] OK
  
  $ cat a.mli a.ml
  val x : unit -> int
  
  module B : sig
  
    val y : unit -> int
  
    module C : sig
  
      val z : unit -> int
  
    end
  
  end
  let x () = 2
  
  module B = struct
    let y () = 199
  
    module C = struct
      let z () = 900
    end
  end
  $ cat Caramel.A.core.ast Caramel.A.core
  % Source code generated with Caramel.
  ((attributes ((Export_fun (attr_fun_name (Atom () x)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () x)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 2)))))))))
    (module_name (Atom () 'Caramel.A')) (file_name Caramel.A.core) (ctx ())
    (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A').
  
  -export([x/1]).
  
  x({}) -> 2.
  
  
  $ cat Caramel.A.B.core.ast Caramel.A.B.core
  % Source code generated with Caramel.
  ((attributes ((Export_fun (attr_fun_name (Atom () y)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () y)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 199)))))))))
    (module_name (Atom () 'Caramel.A.B')) (file_name Caramel.A.B.core) (ctx ())
    (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A.B').
  
  -export([y/1]).
  
  y({}) -> 199.
  
  
  $ cat Caramel.A.B.C.core.ast Caramel.A.B.C.core
  % Source code generated with Caramel.
  ((attributes ((Export_fun (attr_fun_name (Atom () z)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () z)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 900)))))))))
    (module_name (Atom () 'Caramel.A.B.C')) (file_name Caramel.A.B.C.core)
    (ctx ()) (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A.B.C').
  
  -export([z/1]).
  
  z({}) -> 900.
  
  
