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

(** Precedence levels of Ast terms. *)
type t =
  | Low
  | Semi
  | LessMinus
  | ColonEqual
  | As
  | Comma
  | MinusGreater
  | BarBar
  | AmperAmper
  | InfixOp0
  | InfixOp1
  | ColonColon
  | InfixOp2
  | InfixOp3
  | InfixOp4
  | UMinus
  | Apply
  | HashOp
  | Dot
  | High
  | Atomic

let compare : t -> t -> int = Poly.compare

let equal a b = compare a b = 0

let to_string = function
  | Low -> "Low"
  | Semi -> "Semi"
  | LessMinus -> "LessMinus"
  | ColonEqual -> "ColonEqual"
  | As -> "As"
  | Comma -> "Comma"
  | MinusGreater -> "MinusGreater"
  | BarBar -> "BarBar"
  | AmperAmper -> "AmperAmper"
  | InfixOp0 -> "InfixOp0"
  | InfixOp1 -> "InfixOp1"
  | ColonColon -> "ColonColon"
  | InfixOp2 -> "InfixOp2"
  | InfixOp3 -> "InfixOp3"
  | InfixOp4 -> "InfixOp4"
  | UMinus -> "UMinus"
  | Apply -> "Apply"
  | Dot -> "Dot"
  | HashOp -> "HashOp"
  | High -> "High"
  | Atomic -> "Atomic"
