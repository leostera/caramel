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

let parse ~loc text =
  let location = loc.Location.loc_start in
  let location =
    { location with
      pos_cnum= location.pos_cnum + 3 (* Length of comment opening *) }
  in
  match Odoc_parser.parse_comment_raw ~location ~text with
  | exception _ ->
      let span = Migrate_ast.Location.to_span loc in
      Error [Odoc_model.Error.make "comment could not be parsed" span]
  | {value; warnings= []} -> Ok value
  | {warnings; _} -> Error warnings

let warn fmt warning =
  Format.fprintf fmt "Warning: Invalid documentation comment:@,%s\n%!"
    (Odoc_model.Error.to_string warning)
