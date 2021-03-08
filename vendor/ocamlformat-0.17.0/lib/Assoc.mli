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

(** Associativities of Ast terms *)
type t = Left | Non | Right

val to_string : t -> string

val equal : t -> t -> bool

val of_prec : Prec.t -> t
(** [of_prec prec] is the associativity of Ast terms with precedence [prec].
    (Associativity is uniform across precedence levels.) *)
