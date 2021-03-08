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

(** Associativities of Ast terms. *)
type t = Left | Non | Right

let to_string = function Left -> "Left" | Non -> "Non" | Right -> "Right"

let equal : t -> t -> bool = Poly.( = )

(** Compute associativity from precedence, since associativity is uniform
    across precedence levels. *)
let of_prec (x : Prec.t) =
  match x with
  | Low | Semi | LessMinus -> Non
  | ColonEqual -> Right
  | As -> Non
  | Comma -> Non
  | MinusGreater | BarBar | AmperAmper -> Right
  | InfixOp0 -> Left
  | InfixOp1 -> Right
  | ColonColon -> Right
  | InfixOp2 | InfixOp3 -> Left
  | InfixOp4 -> Right
  | UMinus | Apply -> Non
  | HashOp -> Left
  | Dot -> Left
  | High -> Non
  | Atomic -> Non
