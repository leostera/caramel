module Erl = Erlang.Parsetree_helper

type t = string list
(** An intermediate representation of identifiers as a list of strings.

    This is used to make sure we save all possible information from OCaml
    while translating identifiers, and it will be translated into the
    right Erlang term (variable, atom, function name, etc) once we
    know how to use it.
  *)

let root : t = [ "Caramel" ]

let from_ident : ident:Ident.t -> t = fun ~ident -> root @ [ Ident.name ident ]

let from_longident : ident:Longident.t -> t =
 fun ~ident -> root @ Longident.flatten ident

(* Module Name conversions between OCaml and Erlang *)
module Module_name : sig
  type t

  val make : prefix:t -> ident:string -> t

  val from_caml_module_path : string list -> t

  val from_parts : string list -> t

  val to_string : t -> string

  val to_file_name : t -> string

  val to_atom : t -> Erlang.Parsetree.location Erlang.Parsetree.atom
end = struct
  type t = string list

  let separator = "."

  let root_module = "Caramel"

  let make ~prefix ~ident = ident :: prefix

  let from_caml_module_path path = path @ [ root_module ]

  let from_parts parts = parts

  let to_string t = String.concat separator (List.rev t)

  let to_file_name t = to_string t ^ ".erl"

  let to_atom t = Erl.Atom.mk ~ctx:Erl.Loc.empty (to_string t)
end

module Well_known = struct
  let unit : t = root @ [ "()" ]

  let list : t = root @ [ "[]" ]

  let is_well_known id =
    if id = unit then `unit else if id = list then `list else `not_well_known
end

let export t =
  match List.rev t with
  | name :: _ -> Erl.Atom.mk ~ctx:Erl.Loc.empty name
  | _ -> Error.todo "Identifier.export was entirely empty?"

(** Builds a datatype constructor name from an `Identifier.t`.

    These usually look like `'Caramel.App.User.Admin'`, where `Admin`
    is the Constructor Name and `Caramel.App.User` is the Module Path.
  *)
let constructor t =
  let atom = String.concat "." t in
  Erl.Atom.mk ~ctx:Erl.Loc.empty atom

(** Builds the name of a bound variable from an `Identifier.t`.

    These usually look like `Caramel@x` or `Caramel@user_name`.
  *)
let binding t =
  let name = String.concat "@" t in
  Erl.Name.var ~ctx:Erl.Loc.empty ~name

(** Builds a local symbol. Useful for building names for types and
    function declarations.
  *)
let local_symbol t =
  let open Erl in
  match t with
  | [ _caramel_suffix; name ] -> Atom.mk ~ctx:Loc.empty name
  | _ -> Error.todo "Identifier.local_symbol was entirely empty?"

(** Builds an Erlang symbol reference from an `Identifier.t`.

    If the identifier includes a Module Path, then the symbol
    will be qualified.

    If the identifier does not include a Module Path, we assume it is
    local.

    These typically look like:

      * Qualified - `'Caramel@App.User':'login'`
      * Local - `'login'`
  *)
let symbol t =
  let open Erl in
  match List.rev t with
  | [ _name; _caramel_suffix ] ->
      let n = Name.atom (local_symbol t) in
      Symbol.local ~n
  | name :: mod_path ->
      let m = Name.atom Module_name.(from_parts mod_path |> to_atom) in
      let n = Name.atom (Atom.mk ~ctx:Loc.empty name) in
      Symbol.qualified ~m ~n
  | _ -> Error.todo "Identifier.symbol was entirely empty?"
