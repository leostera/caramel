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

module Module_name : sig
  type t

  val make : prefix:t -> ident:string -> t

  val from_caml_module_path : string list -> t

  val from_parts : string list -> t

  val to_string : t -> string

  val to_file_name : t -> string
end = struct
  type t = string list

  let separator = "."

  let root_module = "Caramel"

  let make ~prefix ~ident = ident :: prefix

  let from_caml_module_path path = path @ [ root_module ]

  let from_parts parts = parts

  let to_string t = String.concat separator (List.rev t)

  let to_file_name t = to_string t ^ ".erl"
end

module Well_known = struct
  let unit : t = root @ [ "()" ]

  let list : t = root @ [ "[]" ]

  let is_well_known id =
    if id = unit then `unit else if id = list then `list else `not_well_known
end
