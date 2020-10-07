(**
 *  Helpers to create Erlang ASTs
 *)

open Erl_ast

(* Helpers to work with Atoms *)
module Atom : sig
  val mk : string -> atom

  val to_string : atom -> string

  val equal : atom -> atom -> bool

  val concat : atom -> atom -> string -> atom
end

(* Helpers to work with Names *)
module Name : sig
  val var : string -> name

  val atom : string -> name

  val qualified : module_name:atom -> atom -> name

  val to_string : name -> string

  val ignore : name
end

(* Helpers to work with Literals *)
module Const : sig
  val integer : string -> literal

  val char : string -> literal

  val binary : string -> literal

  val float : string -> literal

  val atom : atom -> literal
end

(* Helpers to work with Expressions *)
module Expr : sig
  val apply : expr -> expr list -> expr

  val case : expr -> case list -> expr

  val cons : expr list -> expr -> expr

  val const : literal -> expr

  val field : atom -> expr -> map_field

  val fun_ : cases:case list -> expr

  val fun_ref : atom -> arity:int -> expr

  val ident : name -> expr

  val let_ : let_binding -> expr -> expr

  val let_bind : pattern -> expr -> let_binding

  val list : expr list -> expr

  val map : map_field list -> expr

  val nil : expr

  val recv : cases:case list -> after:case option -> expr

  val tuple : expr list -> expr
end

(* Helpers to work with Patterns *)
module Pat : sig
  val any : pattern

  val bind : name -> pattern

  val tuple : pattern list -> pattern

  val list : pattern list -> pattern

  val cons : pattern list -> pattern -> pattern

  val map : (atom * pattern) list -> pattern

  val const : literal -> pattern
end

(* Helpers to work with Functions *)
module FunDecl : sig
  val mk : name:atom -> ?spec:type_kind option -> cases:case list -> fun_decl

  val case : lhs:pattern list -> ?guard:guard option -> rhs:expr -> case
end

(* Helpers to work with Types *)
module Type : sig
  val mk :
    ?visibility:type_visibility ->
    ?params:name list ->
    name:atom ->
    kind:type_kind ->
    type_decl

  val apply : ?args:type_kind list -> name:name -> type_kind

  val constr : ?args:type_kind list -> name:name -> variant_constructor

  val extension : type_kind -> variant_constructor

  val field : atom -> type_kind -> record_field

  val fun_ : ?args:type_kind list -> return:type_kind -> type_kind

  val record : record_field list -> type_kind

  val var : name -> type_kind

  val const : literal -> type_kind

  val variant : variant_constructor list -> type_kind

  val tuple : type_kind list -> type_kind

  val opaque : type_visibility

  val visible : type_visibility

  val any : type_kind
end

module Export : sig
  val mk : kind:export_type -> name:atom -> arity:int -> export

  val fun_ : atom -> arity:int -> export

  val type_ : atom -> arity:int -> export
end

(* Helpers to work with Modules *)
module Mod : sig
  val empty : t

  val mk :
    ?attributes:attribute list ->
    ?behaviours:atom list ->
    ?exports:export list ->
    ?functions:fun_decl list ->
    ?types:type_decl list ->
    atom ->
    t

  val of_structure : structure -> t
end

(* Find a function by its name within a module.
 *)
val find_fun_by_name : Erl_ast.t -> Erl_ast.atom -> Erl_ast.fun_decl option
