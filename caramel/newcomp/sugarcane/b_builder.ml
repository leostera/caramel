open Ir

let rec of_ir_module mod_ =
  (* Small utility to traverse an IR and collect all the top-level lets *)
  let rec collect_lets acc ir =
    match ir with
    | Ir_let (_, _, _, next) as let' -> collect_lets (let' :: acc) next
    | Ir_letrec (_, next) as let' -> collect_lets (let' :: acc) next
    | _ -> acc
  in

  match mod_ with
  | Ir_module (name, next) ->
      let lets = collect_lets [] next in
      let exports, module_info_defs = build_exports ~name lets in
      let defs = module_info_defs @ List.concat_map def_of_ir_let lets in
      B.module_ ~name:(Identifier.to_string name) ~defs ~exports
  | _ -> Error.panic "we expected a module here!"

(* NOTE: all B-lang modules need to have 2 `module_info` public functions that
   are expected of every module on the BEAM. These are normally used to access
   metadata on modules, and can be very helpful for metaprograming, including
   runtime validation of module behavior compliance (eg. does module X export
   functions f/1 and g/2?).

   We probably want to do this in another way in the long run, but since this is
   where we first build up the B-lang representation, it seemed like a good spot
   for now.
*)
and build_exports ~name lets =
  let exports : B.fn_name list =
    List.concat_map export_of_ir_let lets
    @ [
        B.fun_name ~name:"module_info" ~arity:0;
        B.fun_name ~name:"module_info" ~arity:1;
      ]
  in
  let defs =
    [
      B.def ~name:"module_info" ~arity:0
        ~body:
          (B.fun_ ~args:[]
             ~body:
               (B.call ~mod_:"erlang" ~fun_:"get_module_info"
                  ~args:[ B.atom (Identifier.to_string name) ]));
      B.def ~name:"module_info" ~arity:1
        ~body:
          (B.fun_ ~args:[ "Opts" ]
             ~body:
               (B.call ~mod_:"erlang" ~fun_:"get_module_info"
                  ~args:[ B.atom (Identifier.to_string name); B.var "Opts" ]));
    ]
  in
  (exports, defs)

(**

   Given a top-level let expression, we turn it into a list of exported
   function names with arities.

   Private bindings are not exported.

*)
and export_of_ir_let let_ =
  match let_ with
  | Ir_letrec (bindings, _) ->
      List.filter_map
        (fun (v, name, body) ->
          match (v, body) with
          | Exported, Ir_fun (args, _) ->
              let arity = List.length args in
              Some (B.fun_name ~name:(Identifier.to_string name) ~arity)
          | _ -> None)
        bindings
  | Ir_let (Exported, name, Ir_fun (args, _), _next) ->
      let arity = List.length args in
      [ B.fun_name ~name:(Identifier.to_string name) ~arity ]
  | Ir_let (Private, _name, _body, _next) -> []
  | _ -> Error.panic "we expected a let binding here!"

(**

  Given a top-level let expression, we turn it into a function definition.

*)
and def_of_ir_let let_ =
  match let_ with
  | Ir_letrec (bindings, _next) ->
      List.map
        (function
          | _vis, name, (Ir_fun (args, _) as body) ->
              let name = Identifier.to_string name in
              let arity = List.length args in
              B.def ~name ~arity ~body:(of_ir_expr body)
          | _ -> Error.panic "we expected a let binding for a function here!")
        bindings
  | Ir_let (_vis, name, (Ir_fun (args, _) as body), _next) ->
      let name = Identifier.to_string name in
      let arity = List.length args in
      [ B.def ~name ~arity ~body:(of_ir_expr body) ]
  | _ -> Error.panic "we expected a let binding for a function here!"

(**

  This function translates from the IR to the B language.

  The B language maps almost directly with what is expressible in the BEAM's
  Core Erlang language. Because of that, most of what happens here is fitting
  the IR into the AST defined by the B module.

  One example of this is field access (in IR: `Ir_field (idx, expr)`), which
  does not have an equivalent on B and instead needs to translate to a form of
  let and pattern matching, to extract the relevant field.

*)
and of_ir_expr expr =
  match expr with
  | Ir_fn_name (name, arity) ->
      B.fun_ref (B.fun_name ~name:(Identifier.to_string name) ~arity)
  | Ir_apply (e, args) ->
      B.apply ~fn:(of_ir_expr e) ~args:(List.map of_ir_expr args)
  | Ir_let (_vis, id, e1, e2) ->
      B.let_
        ~value_list:[ Identifier.to_string id ]
        ~expr:(of_ir_expr e1) ~body:(of_ir_expr e2)
  | Ir_case (e, pats) ->
      let cond = of_ir_expr e in
      let cases =
        List.map (fun (pat, e2) -> (of_ir_pat pat, of_ir_expr e2)) pats
      in
      B.case ~cond ~cases
  | Ir_catch (e1, e2) -> B.catch (of_ir_expr e1) (of_ir_expr e2)
  | Ir_ext_call ((mod_, fun_), args) ->
      let args = List.map of_ir_expr args in
      B.call ~mod_ ~fun_ ~args
  (* NOTE: here we turn field access into a map access *)
  | Ir_field (_idx, Some field, e) ->
      B.call ~mod_:"maps" ~fun_:"get" ~args:[ of_ir_expr field; of_ir_expr e ]
  (* NOTE: here we turn index field access into a list access. We shift the index
     by one since `lists:nth/2` is 1-indexed. God knows why.
  *)
  | Ir_field (0, None, e) ->
      B.call ~mod_:"erlang" ~fun_:"hd" ~args:[ of_ir_expr e ]
  | Ir_field (_, None, e) ->
      B.call ~mod_:"erlang" ~fun_:"tl" ~args:[ of_ir_expr e ]
  | Ir_fun (args, body) ->
      B.fun_ ~args:(List.map Identifier.to_string args) ~body:(of_ir_expr body)
  | Ir_letrec (_bindings, _e) -> Error.todo "Ir_letrec"
  | Ir_module (_id, _e) ->
      Error.panic "there should be no modules at this level!"
  | Ir_record { fields; _ } ->
      let fields =
        List.map (fun (k, v) -> (of_ir_expr k, of_ir_expr v)) fields
      in
      B.map ~fields
  | Ir_throw (_idx, exprs) ->
      B.call ~mod_:"erlang" ~fun_:"throw" ~args:(List.map of_ir_expr exprs)
  | Ir_seq (e1, e2) -> B.seq (of_ir_expr e1) (of_ir_expr e2)
  | Ir_tuple parts -> B.tuple ~parts:(List.map of_ir_expr parts)
  | Ir_program _ -> Error.panic "there should be no programs at this level!"
  | Ir_lit lit -> of_ir_lit lit
  | Ir_var name -> B.var (Identifier.to_string name)
  | Ir_nil -> B.nil
  | Ir_cons (head, tail) ->
      B.cons ~head:(of_ir_expr head) ~tail:(of_ir_expr tail)

and of_ir_pat pat =
  match pat with
  | P_ignore -> B.pat_ignore
  | P_tuple parts -> B.pat_tuple (List.map of_ir_pat parts)
  | P_nil -> B.pat_nil
  | P_bind var -> B.pat_var var
  | P_lit (Lit_atom atom) -> B.pat_atom atom
  | P_lit (Lit_int int) -> B.pat_int int
  | P_lit (Lit_char char) -> B.pat_char char
  | P_lit (Lit_string string) -> B.pat_binary string
  | P_lit (Lit_float float) -> B.pat_float float

and of_ir_lit lit =
  match lit with
  | Lit_atom atom -> B.atom atom
  | Lit_int int -> B.int int
  | Lit_char char -> B.char char
  | Lit_string string -> B.binary string
  | Lit_float float -> B.float float

let of_ir Translation_unit.{ ir; _ } =
  match ir with
  | Ir_program modules -> List.map of_ir_module modules
  | _ -> Error.panic "we expected a program here!"
