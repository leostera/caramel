open Ir

module Mod_table = struct
  let empty : (string, B.t) Hashtbl.t = Hashtbl.create 256

  let add t name expr = Hashtbl.add t name expr

  let values t = Hashtbl.to_seq_values t |> List.of_seq
end

let collect_lets ir =
  let rec aux acc ir =
    match ir with
    | Ir_let (_, _, next) as let' -> aux (let' :: acc) next
    | _ -> acc
  in
  aux [] ir

let rec of_ir_module tbl mod_ =
  match mod_ with
  | Ir_module (name, next) ->
      let lets = collect_lets next in
      let defs = List.map (def_of_ir_let tbl) lets in
      B.module_ ~name:(Identifier.to_string name) ~defs
  | _ -> Error.panic "we expected a module here!"

and def_of_ir_let _tbl let_ =
  match let_ with
  | Ir_let (name, body, _next) ->
      B.def ~name:(Identifier.to_string name) ~arity:1 ~body:(of_ir_expr body)
  | _ -> Error.panic "we expected a let binding here!"

and of_ir_expr expr =
  match expr with
  | Ir_apply (e, args) ->
      B.apply ~fn:(of_ir_expr e) ~args:(List.map of_ir_expr args)
  | Ir_let (id, e1, e2) ->
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
  | Ir_field (idx, e) ->
      B.call ~mod_:"maps" ~fun_:"get"
        ~args:[ B.int (Int.to_string idx); of_ir_expr e ]
  | Ir_fun (args, body) ->
      B.fun_ ~args:(List.map Identifier.to_string args) ~body:(of_ir_expr body)
  | Ir_letrec (_bindings, _e) -> Error.todo "Ir_letrec"
  | Ir_module (_id, _e) ->
      Error.panic "there should be no modules at this level!"
  | Ir_record fields ->
      let fields =
        List.map (fun (k, v) -> (B.int (Int.to_string k), of_ir_expr v)) fields
      in
      B.map ~fields
  | Ir_throw (_idx, _exprs) -> Error.todo "Ir_throw"
  | Ir_seq (e1, e2) -> B.seq (of_ir_expr e1) (of_ir_expr e2)
  | Ir_program _ -> Error.panic "there should be no programs at tthis level!"
  | Ir_lit lit -> of_ir_lit lit
  | Ir_var name -> B.var (Identifier.to_string name)

and of_ir_pat pat =
  match pat with
  | P_ignore -> B.pat_ignore
  | P_lit (Lit_atom atom) -> B.pat_atom atom
  | P_lit (Lit_int int) -> B.pat_int int
  | P_lit (Lit_char char) -> B.pat_char char
  | P_lit (Lit_string _string) -> Error.todo "binary pattern matching missing"
  | P_lit (Lit_float float) -> B.pat_float float

and of_ir_lit lit =
  match lit with
  | Lit_atom atom -> B.atom atom
  | Lit_int int -> B.int int
  | Lit_char char -> B.char char
  | Lit_string string -> B.binary string
  | Lit_float float -> B.float float

let of_ir Translation_unit.{ ir; _ } =
  let mod_table = Mod_table.empty in
  match ir with
  | Ir_program modules -> List.map (of_ir_module mod_table) modules
  | _ -> Error.panic "we expected a program here!"
