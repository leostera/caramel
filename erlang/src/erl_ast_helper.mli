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

  val lowercase : atom -> atom
end

(* Helpers to work with Names *)
module Name : sig
  val var : string -> name

  val atom : atom -> name

  val qualified : m:name -> f:name -> name

  val to_string : name -> string
end

(* Helpers to work with Literals *)
module Const : sig
  val integer : string -> literal

  val char : string -> literal

  val binary : string -> literal

  val string : string -> literal

  val float : string -> literal

  val atom : atom -> literal
end

(* Helpers to work with Expressions *)
module Expr : sig
  val apply : expr -> expr list -> expr

  val case : expr -> case list -> expr

  val cons : expr list -> expr -> expr

  val const : literal -> expr

  val fun_ : cases:case list -> expr

  val fun_ref : name -> arity:int -> expr

  val ident : name -> expr

  val let_ : let_binding -> expr -> expr

  val let_bind : pattern -> expr -> let_binding

  val list : expr list -> expr

  val map_field : expr -> expr -> map_field

  val map : map_field list -> expr

  val map_update : expr -> map_field list -> expr

  val nil : expr

  val recv : cases:case list -> after:case option -> expr

  val tuple : expr list -> expr

  val comment : comment -> expr -> expr

  val try_ : expr -> catch:case list option -> after:expr option -> expr

  val catch : expr -> expr

  val if_ : clauses:(expr list list * expr) list -> expr

  val macro : string -> expr
end

(* Helpers to work with Patterns *)
module Pat : sig
  val any : pattern

  val bind : name -> pattern

  val tuple : pattern list -> pattern

  val list : pattern list -> pattern

  val cons : pattern list -> pattern -> pattern

  val map : (pattern * pattern) list -> pattern

  val with_name : pattern -> pattern -> pattern

  val const : literal -> pattern

  val catch :
    ?class_:name option -> ?stacktrace:name option -> pattern -> pattern
end

(* Helpers to work with Functions *)
module FunDecl : sig
  val mk : name:atom -> ?spec:type_expr option -> cases:case list -> fun_decl

  val case : lhs:pattern list -> ?guard:guard option -> rhs:expr -> case
end

(* Helpers to work with Types *)
module Type : sig
  val mk :
    ?kind:type_kind ->
    ?params:type_expr list ->
    name:atom ->
    expr:type_expr ->
    type_decl

  val apply : ?args:type_expr list -> name:name -> type_expr

  val field : atom -> type_expr -> record_field

  val fun_ : ?args:type_expr list -> return:type_expr -> type_expr

  val all_named_vars : type_expr -> string list

  val clean_unbound_named_vars : type_expr -> type_expr

  val record : name -> record_field list -> type_expr

  val map_field :
    ?presence:field_presence -> type_expr -> type_expr -> type_map_field

  val map : type_map_field list -> type_expr

  val var : name -> type_expr

  val const : literal -> type_expr

  val variant : type_expr list -> type_expr

  val tuple : type_expr list -> type_expr

  val list : type_expr -> type_expr

  val opaque : type_kind

  val type_ : type_kind

  val spec : type_kind

  val callback : type_kind

  val any : type_expr
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
