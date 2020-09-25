exception Unsupported_empty_identifier

let safe_atom u = match u.[0] with 'a' .. 'z' -> u | _ -> "'" ^ u ^ "'"

let rec varname_of_string s =
  let name = s |> String.capitalize_ascii in
  match (name.[0], name) with
  | '_', name when name <> "_" ->
      let name =
        name |> String.to_seq |> List.of_seq |> List.tl |> List.to_seq
        |> String.of_seq
      in
      "_" ^ varname_of_string name
  | _, _ -> name

let varname_of_ident i = i |> Ident.name |> varname_of_string

let atom_of_string = String.lowercase_ascii

let atom_of_ident i = i |> Ident.name |> atom_of_string

let longident_to_atom x = x |> Longident.last |> atom_of_string

let longident_to_name x =
  match x |> Longident.flatten |> List.rev with
  | [] -> raise Unsupported_empty_identifier
  | [ x ] -> Erlang.Ast.(Var_name x)
  | n_name :: mods ->
      let n_mod = mods |> List.rev |> String.concat "__" in
      let n_name = safe_atom n_name in
      Erlang.Ast.(Qualified_name { n_mod; n_name })

let ocaml_to_erlang_type t =
  let open Erlang.Ast in
  match t with
  | "int" -> Atom_name "integer"
  | "bool" -> Atom_name "boolean"
  | "option" -> Qualified_name { n_mod = "option"; n_name = "t" }
  | "result" -> Qualified_name { n_mod = "result"; n_name = "t" }
  | u -> Atom_name (safe_atom u)

let path_to_type_name p = Erlang.Ast.Atom_name (Path.name p)

let longident_to_type_name x =
  match x |> Longident.flatten |> List.rev with
  | [] -> raise Unsupported_empty_identifier
  | [ x ] -> ocaml_to_erlang_type x
  | n_name :: mods -> (
      let n_mod =
        mods |> List.rev |> String.concat "__" |> String.lowercase_ascii
      in
      let n_name = safe_atom n_name in
      match (n_mod, n_name) with
      | _, x when x = "option" || x = "result" ->
          Erlang.Ast.(Qualified_name { n_mod = x; n_name = "t" })
      | "erlang", "process" ->
          Erlang.Ast.(Qualified_name { n_mod; n_name = "pid" })
      | _, _ -> Erlang.Ast.(Qualified_name { n_mod; n_name }) )

let to_erl_op t =
  Erlang.Ast.(Qualified_name { n_mod = "erlang"; n_name = "'" ^ t ^ "'" })

let ocaml_to_erlang_primitive_op t =
  match t with
  | "!" | "++" | "-" | "--" | "/" | "<" | ">" | "*" | "+" -> to_erl_op t
  | "^" ->
      Erlang.Ast.(
        Qualified_name { n_mod = "caramel"; n_name = "binary_concat" })
  | "<>" -> to_erl_op "=/="
  | "=" -> to_erl_op "=:="
  | "==" -> to_erl_op "=="
  | "@" -> to_erl_op "++"
  | u -> Erlang.Ast.Atom_name (safe_atom u)
