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

(** Opened in each source module to establish global namespace *)

open Base
module Format = Caml.Format
module Filename = Caml.Filename
include Option.Monad_infix
include Stdio

let ( >> ) f g x = g (f x)

let impossible msg = failwith msg

let check f x =
  assert (
    ignore (f x) ;
    true ) ;
  x

module Fpath = struct
  include Fpath

  let cwd () = Unix.getcwd () |> v

  let exists p = to_string p |> Caml.Sys.file_exists

  let to_absolute file = if is_rel file then append (cwd ()) file else file

  let to_string ?(relativize = false) p =
    if relativize then
      Option.value_map
        (Fpath.relativize ~root:(cwd ()) p)
        ~default:(to_string p) ~f:to_string
    else to_string p

  let pp fmt p = Format.fprintf fmt "%s" (to_string ~relativize:true p)
end

(** Extension of Cmdliner supporting lighter-weight option definition *)
module Cmdliner = struct
  include Cmdliner

  (** existential package of a Term and a setter for a ref to receive the
      parsed value *)
  type arg = Arg : 'a Term.t * ('a -> unit) -> arg

  (** convert a list of arg packages to a term for the tuple of all the arg
      terms, and apply it to a function that sets all the receiver refs *)
  let tuple args =
    let pair (Arg (trm_x, set_x)) (Arg (trm_y, set_y)) =
      let trm_xy = Term.(const (fun a b -> (a, b)) $ trm_x $ trm_y) in
      let set_xy (a, b) = set_x a ; set_y b in
      Arg (trm_xy, set_xy)
    in
    let init = Arg (Term.const (), fun () -> ()) in
    let (Arg (trm, set)) = List.fold_right ~f:pair args ~init in
    Term.app (Term.const set) trm

  let args : arg list ref = ref []

  let mk ~default arg =
    let var = ref default in
    let set x = var := x in
    args := Arg (arg, set) :: !args ;
    var

  let parse info validate =
    Term.eval (Term.(ret (const validate $ tuple !args)), info)
end

module String = struct
  include Base.String

  let starts_with_whitespace s =
    (not (is_empty s)) && Char.is_whitespace s.[0]

  let ends_with_whitespace s =
    (not (is_empty s)) && Char.is_whitespace s.[length s - 1]
end
