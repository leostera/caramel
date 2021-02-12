open Import

module Kind : sig
  type t =
    | Intf
    | Impl

  type ('intf, 'impl) pair =
    { intf : 'intf
    ; impl : 'impl
    }

  module Map : sig
    type 'a t = ('a, 'a) pair

    type kind

    val get : 'a t -> kind -> 'a

    val iter : 'a t -> f:('a -> unit) -> unit

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val both : 'a t -> 'b t -> ('a * 'b) t

    val make_both : 'a -> 'a t
  end
  with type kind := t
end

module Arg : sig
  type 'e t =
    | Unnamed of 'e
    | Labeled of string * 'e
    | Optional of string * 'e
end

module Type : sig
  [@@@warning "-30"]

  type attr

  type prim =
    | Unit
    | String
    | Int
    | Bool

  type t =
    | Named of string
    | Var of string
    | Prim of prim
    | Tuple of t list
    | Optional of t
    | List of t
    | Poly_variant of constr list
    | Assoc of t * t
    | App of t * t list
    | Fun of t Arg.t * t

  and field =
    { name : string
    ; typ : t
    ; attrs : attr list
    }

  and constr =
    { name : string
    ; args : t list
    }

  type decl =
    | Alias of t
    | Record of field list
    | Variant of constr list

  val fun_ : t Arg.t list -> t -> t

  (* This is for lists where the keys are equal to strings *)
  val assoc_list : key:t -> data:t -> t

  val pp_decl : name:string -> kind:Kind.t -> decl -> unit Pp.t

  val pp : t -> kind:Kind.t -> unit Pp.t

  val field : t -> name:string -> field

  val kind_field : literal:string -> field

  val get_kind : field -> string option

  val constr : t list -> name:string -> constr

  (** Simplified sum types*)
  val enum : string list -> decl

  (** Polymorphic variant form *)
  val poly_enum : string list -> t

  val list : t -> t

  val module_t : string -> t

  val t : t

  val string : t

  val name : string -> t

  val int : t

  val bool : t

  val alpha : t

  val json : t

  val unit : t

  val void : t

  class virtual ['env, 'm] mapreduce :
    object ('self)
      method virtual empty : 'm

      method virtual plus : 'm -> 'm -> 'm

      method private fold_left_map :
        'a. f:('a -> 'a * 'm) -> 'a list -> 'a list * 'm

      method alias : 'env -> t -> decl * 'm

      method app : 'env -> t -> t list -> t * 'm

      method assoc : 'env -> t -> t -> t * 'm

      method constr : 'env -> constr -> constr * 'm

      method field : 'env -> field -> field * 'm

      method list : 'env -> t -> t * 'm

      method named : 'env -> string -> t * 'm

      method optional : 'env -> t -> t * 'm

      method poly_variant : 'env -> constr list -> t * 'm

      method prim : 'env -> prim -> t * 'm

      method record : 'env -> field list -> decl * 'm

      method t : 'env -> t -> t * 'm

      method decl : 'env -> decl -> decl * 'm

      method tuple : 'env -> t list -> t * 'm

      method var : 'env -> string -> t * 'm

      method variant : 'env -> constr list -> decl * 'm
    end
end

module Expr : sig
  [@@@warning "-30"]

  type expr =
    | Let of pat * expr * expr
    | Match of expr * (pat * expr) list
    | Fun of pat Arg.t list * expr
    | App of expr * expr Arg.t list
    | Create of expr prim
    | Assert_false

  and 'e prim =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Ident of string
    | Cons of 'e * 'e prim
    | List of 'e list
    | Tuple of 'e list
    | Record of 'e record_
    | Constr of 'e constr

  and pat =
    | Wildcard
    | Pat of pat prim

  and 'e record_ = (string * 'e) list

  and 'e constr =
    { tag : string
    ; poly : bool
    ; args : 'e list
    }

  type t = expr

  val assert_false_clause : pat * expr

  type toplevel =
    { pat : (string Arg.t * Type.t) list
    ; type_ : Type.t
    ; body : t
    }
end

module Module : sig
  type 'a t =
    { name : string
    ; bindings : 'a Named.t list
    }

  val empty : string -> 'a t

  type sig_ =
    | Value of Type.t
    | Type_decl of Type.decl
    | Json_conv_sig

  type impl =
    | Type_decl of Type.decl
    | Value of Expr.toplevel

  val pp_sig : sig_ t -> unit Pp.t

  val pp_impl : impl t -> unit Pp.t
end
