(**************************************************************************)
(*                                                                        *)
(*                              OCamlFormat                               *)
(*                                                                        *)
(*            Copyright (c) Facebook, Inc. and its affiliates.            *)
(*                                                                        *)
(*      This source code is licensed under the MIT license found in       *)
(*      the LICENSE file in the root directory of this source tree.       *)
(*                                                                        *)
(**************************************************************************)

(** Abstract syntax tree terms *)

open Migrate_ast
open Parsetree

val init : Conf.t -> unit
(** Initialize internal state *)

module Attr : sig
  val is_doc : attribute -> bool
  (** Holds for docstrings, that are attributes of the form [(** ... *)]. *)
end

module String_id : sig
  val is_prefix : string -> bool
  (** Holds for prefix symbols. *)

  val is_infix : string -> bool
  (** Holds for infix symbols. *)

  val is_symbol : string -> bool
  (** Holds for prefix or infix symbols. *)

  val is_hash_getter : string -> bool
  (** [is_hash_getter id] returns whether [id] is considered a hash-getter
      operator, of the form [#**#] or [#**.] where [**] can be 0 or more
      operator chars. *)

  val is_monadic_binding : string -> bool
  (** [is_monadic_binding id] returns whether [id] is a monadic binding
      operator of the form [let**] or [and**] where [**] can be 1 or more
      operator chars. *)
end

module Longident : sig
  include module type of Longident

  val is_infix : t -> bool
  (** Holds for infix identifiers. *)

  val is_hash_getter : t -> bool
  (** [is_hash_getter id] returns whether [id] is considered a hash-getter
      operator, of the form [#**#] or [#**.] where [**] can be 0 or more
      operator chars. *)

  val is_monadic_binding : t -> bool
  (** [is_monadic_binding id] returns whether [id] is a monadic binding
      operator of the form [let**] or [and**] where [**] can be 1 or more
      operator chars. *)
end

module Exp : sig
  val is_prefix : expression -> bool
  (** Holds for prefix symbol expressions. *)

  val is_symbol : expression -> bool
  (** Holds for prefix or infix expressions. *)

  val is_monadic_binding : expression -> bool
  (** [is_monadic_binding id] returns whether [id] is a monadic binding
      operator of the form [let**] or [and**] where [**] can be 1 or more
      operator chars. *)

  val is_sugared_list : expression -> bool
  (** Holds for expressions that can be sugared into [\[e1; ...; eN\]] form. *)

  val exposed_left : expression -> bool
  (** [exposed_left exp] holds if the left-most subexpression of [exp] is a
      prefix operators. *)
end

module Indexing_op : sig
  type brackets = Round | Square | Curly

  type custom_operator =
    { path: string list  (** eg. [a.X.Y.*{b}] *)
    ; opchars: string
    ; brackets: brackets }

  type indexing_op =
    | Defined of expression * custom_operator
        (** [.*( a )]: take a single argument *)
    | Extended of expression list * custom_operator
        (** [.*( a; b; c )]: take several arguments, separated by [;] *)
    | Special of expression list * brackets
        (** [.()], [.\[\]] and bigarray operators: take several arguments,
            separated by [,] *)

  type t =
    { lhs: expression
    ; op: indexing_op
    ; rhs: expression option  (** eg. [a.*{b} <- exp] *)
    ; loc: Location.t }

  val get_sugar :
    expression -> (Asttypes.arg_label * expression) list -> t option
  (** [get_sugar e args] is [Some all] if [e] is an identifier that is an
      indexing operator and if the sugar syntax is already used in the
      source, [None] otherwise. [args] should be the arguments of the
      corresponding [Pexp_apply]. *)
end

val doc_atrs :
     ?acc:(string Location.loc * bool) list
  -> attributes
  -> (string Location.loc * bool) list option * attributes

module Pat : sig
  val is_simple : pattern -> bool
end

module Mod : sig
  val is_simple : module_expr -> bool
end

module Mty : sig
  val is_simple : module_type -> bool
end

module Cl : sig
  val is_simple : class_expr -> bool
end

module Cty : sig
  val is_simple : class_type -> bool
end

module Tyd : sig
  val is_simple : type_declaration -> bool
end

type toplevel_item =
  [`Item of structure_item | `Directive of toplevel_directive]

(** Ast terms of various forms. *)
type t =
  | Pld of payload
  | Typ of core_type
  | Cty of class_type
  | Pat of pattern
  | Exp of expression
  | Vb of value_binding
  | Cl of class_expr
  | Mty of module_type
  | Mod of module_expr
  | Sig of signature_item
  | Str of structure_item
  | Tli of toplevel_item
  | Top

val is_top : t -> bool

val break_between :
     Source.t
  -> cmts:'a
  -> has_cmts_before:('a -> Location.t -> bool)
  -> has_cmts_after:('a -> Location.t -> bool)
  -> t * Conf.t
  -> t * Conf.t
  -> bool

val attributes : t -> attributes

val location : t -> Location.t

val dump : Format.formatter -> t -> unit
(** Debug: Dump the representation of an Ast term. *)

(** Term-in-context [{ctx; ast}] records that [ast] is (considered to be) an
    immediate sub-term of [ctx]. *)
type 'a xt = private {ctx: t; ast: 'a}

val sub_typ : ctx:t -> core_type -> core_type xt
(** Construct a core_type-in-context. *)

val sub_cty : ctx:t -> class_type -> class_type xt
(** Construct a class_type-in-context. *)

val sub_pat : ctx:t -> pattern -> pattern xt
(** Construct a pattern-in-context. *)

val sub_exp : ctx:t -> expression -> expression xt
(** Construct a expression-in-context. *)

val sub_cl : ctx:t -> class_expr -> class_expr xt
(** Construct a class_expr-in-context. *)

val sub_mty : ctx:t -> module_type -> module_type xt
(** Construct a module_type-in-context. *)

val sub_mod : ctx:t -> module_expr -> module_expr xt
(** Construct a module_expr-in-context. *)

val sub_sig : ctx:t -> signature_item -> signature_item xt
(** Construct a signature_item-in-context. *)

val sub_str : ctx:t -> structure_item -> structure_item xt
(** Construct a structure_item-in-context. *)

val is_simple : Conf.t -> (expression xt -> int) -> expression xt -> bool
(** Holds of "simple" expressions: constants and constructor and function
    applications of other simple expressions. *)

(** 'Classes' of expressions which are parenthesized differently. *)
type cls = Let_match | Match | Non_apply | Sequence | Then | ThenElse

val exposed_right_exp : cls -> expression -> bool
(** [exposed_right_exp cls exp] holds if there is a right-most subexpression
    of [exp] which is of class [cls] and is not parenthesized. *)

val prec_ast : t -> Prec.t option
(** [prec_ast ast] is the precedence of [ast]. Meaningful for binary
    operators, otherwise returns [None]. *)

val parenze_typ : core_type xt -> bool
(** [parenze_typ xtyp] holds when core_type-in-context [xtyp] should be
    parenthesized. *)

val parenze_cty : class_type xt -> bool
(** [parenze_cty xcty] holds when class_type-in-context [xcty] should be
    parenthesized. *)

val parenze_cl : class_expr xt -> bool
(** [parenze_cl xcl] holds when class-in-context [xcl] should be
    parenthesized. *)

val parenze_pat : pattern xt -> bool
(** [parenze_pat xpat] holds when pattern-in-context [xpat] should be
    parenthesized. *)

val parenze_exp : expression xt -> bool
(** [parenze_exp xexp] holds when expression-in-context [xexp] should be
    parenthesized. *)

val parenze_nested_exp : expression xt -> bool
(** [parenze_nested_exp xexp] holds when nested expression-in-context [xexp]
    should be parenthesized. *)

val parenze_mty : module_type xt -> bool
(** [parenze_mty xmty] holds when module_type-in-context [xmty] should be
    parenthesized. *)

val parenze_mod : module_expr xt -> bool
(** [parenze_mod xmod] holds when module_expr-in-context [xmod] should be
    parenthesized. *)

val is_displaced_infix_op : expression xt -> bool
(** [is_displaced_infix_op xexp] holds if an expression-in-context [xexp] is
    an infix op that is not fully applied. *)
