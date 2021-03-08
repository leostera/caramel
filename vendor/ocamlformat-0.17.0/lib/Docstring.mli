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

val parse :
     loc:Warnings.loc
  -> string
  -> (Odoc_parser.Ast.docs, Odoc_model.Error.t list) Result.t

val warn : Format.formatter -> Odoc_model.Error.t -> unit
