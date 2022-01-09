  $ cd src;

  $ caramel parse --dump-caml --file erlang.caramel
  caramel: [DEBUG] type pid
                   type term
                   type atom
                   external atom : string -> atom = "erlang:binary_to_atom"
                   external as_term : 'a -> term = "%identity"
                   type ('ok, 'err) result =
                     | Ok of 'ok 
                     | Error of 'err 
                   external (&&) : bool -> bool -> bool = "erlang:and"
                   external ( * ) : int -> int -> int = "erlang:*"
                   external (+) : int -> int -> int = "erlang:+"
                   external (-) : int -> int -> int = "erlang:-"
                   external (/) : int -> int -> int = "erlang:div"
                   external (<) : int -> int -> bool = "erlang:<"
                   external (<=) : int -> int -> bool = "erlang:=<"
                   external (>=) : int -> int -> bool = "erlang:>="
                   external (!=) : int -> int -> bool = "erlang:=/="
                   external (=) : 'a -> 'a -> bool = "erlang:=="
                   external (>) : int -> int -> bool = "erlang:>"
                   external (lsl) : int -> int -> int = "erlang:bsl"
                   external (mod) : int -> int -> int = "erlang:rem"
                   external (||) : bool -> bool -> bool = "erlang:or"
                   external abs : int -> int = "erlang:abs"
                   external length : 'a list -> int = "erlang:length"
                   external list_to_binary :
                     'a list -> string = "erlang:list_to_binary"
                   external max : int -> int -> int = "erlang:max"
                   external min : int -> int -> int = "erlang:min"
                   external neg : int -> int = "erlang:-"
                   external not : bool -> bool = "erlang:not"
                   external binary_to_integer :
                     string -> int = "erlang:binary_to_integer"
                   external integer_to_binary :
                     int -> string = "erlang:integer_to_binary"
                   external throw : 'a -> 'b = "erlang:throw"
                   external append : 'a list list -> 'a list = "lists:append"
                   module Io =
                     struct
                       external format :
                         string -> 'a list -> unit = "io:format"
                     end
                   module Io_lib =
                     struct
                       external format :
                         string -> 'a list -> string = "io_lib:format"
                     end
                   let rec (^) a b = list_to_binary [a; b]
                   let rec (++) a b = append [a; b]
                   let rec (|>) x f = f x
                   let rec fst (x, _) = x
                   let rec snd (_, y) = y
                   module Supervisor =
                     struct
                       type sup_strategy =
                         | One_for_all 
                       type sup_flags =
                         {
                         strategy: sup_strategy ;
                         intensity: int ;
                         period: int }
                       type child_spec =
                         {
                         id: atom ;
                         start: (atom * atom * term) }
                       type register =
                         | Local of atom 
                       external start_link :
                         register -> 'b -> 'c list -> (pid, 'err) result =
                           "supervisor:start_link"
                     end
                   let rec compare a b =
                     match a <= b with
                     | true -> 1
                     | _ -> (match a = b with | true -> 0 | _ -> neg 1)

  $ caramel compile --new-syntax --debug erlang.caramel
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (erlang.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file erlang.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing erlang.caramel.parsetree
  caramel: [DEBUG] OK
  File "_none_", line 1:
  Warning 61 [unboxable-type-in-prim-decl]: This primitive declaration uses type register, whose representation
  may be either boxed or unboxed. Without an annotation to indicate
  which representation is intended, the boxed representation has been
  selected by default. This default choice may change in future
  versions of the compiler, breaking the primitive implementation.
  You should explicitly annotate the declaration of register
  with [@@boxed] or [@@unboxed], so that its external interface
  remains stable in the future.
  caramel: [DEBUG] Writing erlang.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing erlang.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing erlang.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.caramel.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.caramel.b_2
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing erlang.caramel.b_3
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Supervisor.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Io.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Erlang.Io_lib.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel parse --dump-caml --file http_method.caramel
  caramel: [DEBUG] type t =
                     | GET 
                     | HEAD 
                     | POST 
                     | PUT 
                     | DELETE 
                     | CONNECT 
                     | OPTIONS 
                     | TRACE 

  $ caramel compile --new-syntax --debug http_method.caramel
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (http_method.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file http_method.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing http_method.caramel.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing http_method.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing http_method.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing http_method.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Http_method.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done


  $ caramel parse --dump-caml --file elli.caramel
  caramel: [DEBUG] open Erlang
                   type meth =
                     | GET 
                     | HEAD 
                     | POST 
                     | PUT 
                     | DELETE 
                     | CONNECT 
                     | OPTIONS 
                     | TRACE 
                   type opts =
                     | Callback of atom 
                     | Port of int 
                   let rec child_spec name opts =
                     let open Supervisor in
                       {
                         id = (atom name);
                         start =
                           ((atom "elli"), (atom "start_link"),
                             (as_term [opts]))
                       }
                   module Request =
                     struct
                       type t
                       external meth : t -> meth = "elli_request:method"
                       external path : t -> string list = "elli_request:path"
                     end
                   let rec reply status opts body = (status, opts, body)

  $ caramel compile --new-syntax --debug elli.caramel
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (elli.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file elli.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing elli.caramel.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing elli.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing elli.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing elli.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing elli.caramel.b_1
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Elli.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Elli.Request.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel parse --dump-caml --file main_sup.caramel
  caramel: [DEBUG] constructor: expression (_none_[0,0+-1]..[0,0+-1]) ghost
    Pexp_construct "Supervisor.One_for_all" (_none_[0,0+-1]..[0,0+-1]) ghost
    None
  
  caramel: [DEBUG] open Erlang
                   let rec start_link () =
                     Supervisor.start_link (Local (atom "Caramel.Main_sup"))
                       (atom "Caramel.Main_sup") []
                   let rec init _args =
                     let open Supervisor in
                       let sup_flags =
                         {
                           strategy = Supervisor.One_for_all;
                           intensity = 0;
                           period = 1
                         } in
                       let child_specs =
                         [Elli.child_spec "http_server"
                            [Elli.Callback (Erlang.atom "Caramel.Main");
                            Elli.Port 2112]] in
                       Ok (sup_flags, child_specs)

  $ caramel compile --new-syntax --debug main_sup.caramel 
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main_sup.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main_sup.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] constructor: expression (_none_[0,0+-1]..[0,0+-1]) ghost
    Pexp_construct "Supervisor.One_for_all" (_none_[0,0+-1]..[0,0+-1]) ghost
    None
  
  caramel: [DEBUG] Writing main_sup.caramel.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main_sup.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing main_sup.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main_sup.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_sup.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel parse --dump-caml --file main_app.caramel
  caramel: [DEBUG] let rec start _args _opts = Main_sup.start_link ()
                   let rec stop _state = `ok

  $ caramel compile --new-syntax --debug main_app.caramel
  caramel: [DEBUG] Running Sugarcane compiler on sources: 
  ((sources (main_app.caramel)) (stdlib (./)) (dump_parsetree true)
    (dump_typedtree true) (dump_ir true) (dump_pass -1) (dump_erl_ast true)
    (print_time false) (new_syntax true) (to_beam false))
  
  caramel: [DEBUG] Compiling unit: ((source_file main_app.caramel)
                                     (source_kind impl))
  
  caramel: [DEBUG] Writing main_app.caramel.parsetree
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing main_app.caramel.lambda
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to IR...
  caramel: [DEBUG] Writing main_app.caramel.ir
  caramel: [DEBUG] OK
  caramel: [DEBUG] Translating to B...
  caramel: [DEBUG] Writing main_app.caramel.b_0
  caramel: [DEBUG] OK
  caramel: [DEBUG] Writing Caramel.Main_app.core
  caramel: [DEBUG] OK
  caramel: [DEBUG] Done

  $ caramel parse --dump-caml --file main.caramel
  caramel: [DEBUG] open Erlang
                   open Elli
                   let rec not_found () =
                     let html =
                       "<!doctype html>\n<html>\n  <head>\n    <meta charset=\\\"utf-8\\\">\n    <title>Not Found</title>\n  </head>\n  <body>\n    <h1>Not Found</h1>\n  </body>\n</html>" in
                     reply 404 [] html
                   let rec handle req args =
                     let meth = Request.meth req in
                     let path = Request.path req in
                     Io.format "Handling ~p\\n" [(meth, path)];
                     (match (meth, path) with
                      | (GET, "hello"::you::[]) ->
                          reply 200 [] ("Hello, " ^ (you ^ "!"))
                      | _ -> not_found ())
                   let rec handle_event _event _data _args = `ok

  $ caramel compile --new-syntax --debug main.caramel
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

  $ erlc *.core
