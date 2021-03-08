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

open Migrate_ast

type t = private {txt: string; loc: Location.t}

val create : string -> Location.t -> t

val loc : t -> Location.t

val txt : t -> string

include Comparator.S with type t := t

type pos = Before | Within | After

val fmt :
     t
  -> Source.t
  -> wrap:bool
  -> ocp_indent_compat:bool
  -> fmt_code:(string -> (Fmt.t, unit) Result.t)
  -> pos
  -> Fmt.t
