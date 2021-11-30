  $ caramel compile --sugarcane --dump-ast a.mli a.ml
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (a.mli a.ml))
    (stdlib
      (/Users/ostera/repos/github.com/AbstractMachinesLab/caramel/_build/install/default/bin/../lib/caramel/stdlib))
    (dump_ast true))
  
  caramel: [DEBUG] Compiling interface unit: ((source_file a.mli)
                                               (source_kind intf))
  caramel: [DEBUG] Compiling implementation unit: ((source_file a.ml)
                                                    (source_kind impl))
  caramel: [DEBUG] Calling typedtree handler for unit: ((source_file a.ml)
                                                         (source_kind impl))
  caramel: [DEBUG] Translating file: a.ml (module A)
  caramel: [DEBUG] Creating module: Caramel.A
  caramel: [DEBUG] Creating module: Caramel.A.B
  caramel: [DEBUG] Creating module: Caramel.A.B.C
  caramel: [DEBUG] Writing Caramel.A.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.C.erl.ast	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.erl	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.erl	
  caramel: [DEBUG] OK
  
  caramel: [DEBUG] Writing Caramel.A.B.C.erl	
  caramel: [DEBUG] OK
  
  $ cat a.mli a.ml
  val x : unit -> int
  
  module B : sig
  
    val y : unit -> int
  
    module C : sig
  
      val omg : unit -> int
  
    end
  
  end
  let x () = 1
  
  module B = struct
    let y () = 2
  
    module C = struct
      let omg () = 3
    end
  end
  $ cat Caramel.A.erl.ast Caramel.A.erl
  ((attributes ((Export_fun (attr_fun_name (Atom () x)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () x)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 1)))))))))
    (module_name (Atom () 'Caramel.A')) (file_name Caramel.A.erl) (ctx ())
    (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A').
  % Module attributes:
  -export([x/1]).
  
  % Function declarations:
  x({}) -> 1.
  
  
  $ cat Caramel.A.B.erl.ast Caramel.A.B.erl
  ((attributes ((Export_fun (attr_fun_name (Atom () y)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () y)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 2)))))))))
    (module_name (Atom () 'Caramel.A.B')) (file_name Caramel.A.B.erl) (ctx ())
    (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A.B').
  % Module attributes:
  -export([y/1]).
  
  % Function declarations:
  y({}) -> 2.
  
  
  $ cat Caramel.A.B.C.erl.ast Caramel.A.B.C.erl
  ((attributes ((Export_fun (attr_fun_name (Atom () omg)) (attr_fun_arity 1))))
    (behaviours ())
    (functions
      (((fd_name (Atom () omg)) (fd_arity 1)
         (fd_cases
           (((c_lhs ((Pat_tuple ((ptup_size 0) (ptup_elements ())))))
              (c_rhs (Expr_term (Term_integer () 3)))))))))
    (module_name (Atom () 'Caramel.A.B.C')) (file_name Caramel.A.B.C.erl)
    (ctx ()) (mod_ctx ()))
  
  % Source code generated with Caramel.
  -module('Caramel.A.B.C').
  % Module attributes:
  -export([omg/1]).
  
  % Function declarations:
  omg({}) -> 3.
  
  
