open Erlang.Ast_helper

exception Unsupported_empty_identifier

let varname_of_ident i = i |> Ident.name |> Name.var

let varname_of_longident i = i |> Longident.last |> Name.var

let atom_of_ident i = i |> Ident.name |> Atom.mk

let atom_of_longident x = x |> Longident.last |> Atom.mk

let name_of_ident i = i |> Ident.name |> Name.atom

let name_of_path p = p |> Path.name |> Name.atom

let name_of_longident x =
  match x |> Longident.flatten |> List.rev with
  | [] -> raise Unsupported_empty_identifier
  | [ x ] -> Name.var x
  | n_name :: mods ->
      let module_name = mods |> List.rev |> String.concat "__" |> Atom.mk in
      let n_name = Atom.mk n_name in
      Name.qualified ~module_name n_name

let ocaml_to_erlang_type t =
  match t with
  | "int" -> Name.atom "integer"
  | "bool" -> Name.atom "boolean"
  | "option" -> Name.qualified ~module_name:(Atom.mk "option") (Atom.mk "t")
  | "result" -> Name.qualified ~module_name:(Atom.mk "result") (Atom.mk "t")
  | u -> Name.atom u

let longident_to_type_name x =
  match x |> Longident.flatten |> List.rev with
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
  | u -> Name.atom u
