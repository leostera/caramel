module Erl = Erlang.Parsetree_helper

(* Module Name conversions between OCaml and Erlang *)
module Module_name = struct
  type t = string list

  let separator = "."

  let root_module = "Caramel"

  let make ~prefix ~ident = ident :: prefix

  let from_ocaml : prefix:t -> ident:Ident.t option -> t =
   fun ~prefix ~ident ->
    let ident = match ident with Some x -> Ident.name x | None -> "" in
    make ~prefix ~ident

  let from_parts parts = parts

  let root ident = make ~prefix:[ root_module ] ~ident

  let to_string t = String.concat separator (List.rev t)

  let to_file_name t = to_string t ^ ".erl"

  let to_atom t = Erl.Atom.mk ~ctx:Erl.Loc.empty (to_string t)
end

module Well_known = struct
  let unit = Erl.Atom.mk ~ctx:Erl.Loc.empty "()"

  let is_well_known id = if id = unit then `unit else `not_well_known
end

let constructor ident =
  let name = Longident.last ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name

let binding ident =
  let name = Ident.name ident in
  Erl.Name.var ~ctx:Erl.Loc.empty ~name

let from_ocaml ~ident =
  match Longident.flatten ident with
  | [ name ] -> Erl.Name.var ~ctx:Erl.Loc.empty ~name
  | f :: mod_path ->
      let m =
        List.rev mod_path |> Module_name.from_parts |> Module_name.to_string
      in
      Erl.Name.mf ~ctx:Erl.Loc.empty ~m ~f
  | _ -> Error.todo ()

let function_name ident =
  let name = Ident.name ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name

let type_name ident =
  let name = Ident.name ident in
  Erl.Atom.mk ~ctx:Erl.Loc.empty name
