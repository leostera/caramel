open Erlang.Ast_helper

let translation_table : (Erlang.Ast.name, Erlang.Ast.name) Hashtbl.t =
  let h = Hashtbl.create 1024 in
  [ (("List", "length"), ("erlang", "length")) ]
  |> List.iter (fun ((m1, n1), (m2, n2)) ->
         let k = Name.qualified ~module_name:(Atom.mk m1) (Atom.mk n1) in
         let v = Name.qualified ~module_name:(Atom.mk m2) (Atom.mk n2) in
         Hashtbl.add h k v);
  h

let translate n =
  match Hashtbl.find_opt translation_table n with Some m -> m | None -> n

exception Unsupported_empty_identifier

let varname_of_ident i = i |> Ident.name |> Name.var |> translate

let varname_of_longident i = i |> Longident.last |> Name.var |> translate

let atom_of_ident i = i |> Ident.name |> Atom.mk

let atom_of_longident x = x |> Longident.last |> Atom.mk

let name_of_ident i = i |> Ident.name |> Name.atom |> translate

let name_of_path p = p |> Path.name |> Name.atom |> translate

let name_of_longident x =
  match x |> Longident.flatten |> List.rev with
  | [] -> raise Unsupported_empty_identifier
  | [ x ] -> Name.var x
  | n_name :: mods ->
      let module_name = mods |> List.rev |> String.concat "__" |> Atom.mk in
      let n_name = Atom.mk n_name in
      Name.qualified ~module_name n_name |> translate

let ocaml_to_erlang_type t =
  match t with
  | "string" -> Name.atom "binary"
  | "int" -> Name.atom "integer"
  | "bool" -> Name.atom "boolean"
  | "option" -> Name.qualified ~module_name:(Atom.mk "option") (Atom.mk "t")
  | "result" -> Name.qualified ~module_name:(Atom.mk "result") (Atom.mk "t")
  | u -> Name.atom u

let type_name_of_parts parts =
  match List.rev parts with
  | [] -> raise Unsupported_empty_identifier
  | [ x ] -> ocaml_to_erlang_type x
  | n_name :: mods -> (
      let n_mod =
        mods |> List.rev |> String.concat "__" |> String.lowercase_ascii
      in
      let module_name = Atom.mk n_mod in
      match (n_mod, n_name) with
      | _, x when x = "option" || x = "result" ->
          Name.qualified ~module_name:(Atom.mk x) (Atom.mk "t")
      | "erlang", "process" -> Name.qualified ~module_name (Atom.mk "pid")
      | _, _ -> Name.qualified ~module_name (Atom.mk n_name) )

let type_name_of_path p =
  match Path.flatten p with
  | `Contains_apply ->
      Path.print Format.std_formatter p;
      raise Unsupported_empty_identifier
  | `Ok (id, parts) ->
      let name = id |> Ident.name |> ocaml_to_erlang_type |> Name.to_string in
      type_name_of_parts (name :: parts)

let longident_to_type_name x = x |> Longident.flatten |> type_name_of_parts

let to_erl_op t = Name.qualified ~module_name:(Atom.mk "erlang") (Atom.mk t)

let ocaml_to_erlang_primitive_op t =
  match t with
  | "!" | "++" | "-" | "--" | "/" | "<" | ">" | "*" | "+" -> to_erl_op t
  | "^" ->
      Name.qualified ~module_name:(Atom.mk "caramel") (Atom.mk "binary_concat")
  | "<>" -> to_erl_op "=/="
  | "=" -> to_erl_op "=:="
  | "==" -> to_erl_op "=="
  | "@" -> to_erl_op "++"
  | u -> Name.atom u |> translate
