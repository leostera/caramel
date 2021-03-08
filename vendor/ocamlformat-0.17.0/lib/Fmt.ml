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

(** Formatting combinators *)

module Format = Format_

(** Define the core type and minimal combinators.

    Other higher level functions like [fmt_if] or [list_pn] are implemented
    on top of this. The goal is to be able to modify the underlying
    abstraction without modifying too many places in the [Fmt] module. *)
module T : sig
  type t

  val ( $ ) : t -> t -> t
  (** Sequence *)

  val with_pp : (Format.formatter -> unit) -> t
  (** Use an arbitrary pretty-printing function *)

  val protect : t -> on_error:(exn -> unit) -> t
  (** Exception handler *)

  val lazy_ : (unit -> t) -> t
  (** Defer the evaluation of some side effects until formatting happens.

      This can matter if for example a list of [t] is built, and then only
      some of them end up being displayed. Using [lazy_] ensures that only
      side effects for the displayed elements have run.

      See [tests_lazy] in [Test_fmt]. *)

  val eval : Format.formatter -> t -> unit
  (** Main function to evaluate a term using an actual formatter. *)
end = struct
  type t = (Format.formatter -> unit) Staged.t

  let ( $ ) f g =
    let f = Staged.unstage f in
    let g = Staged.unstage g in
    Staged.stage (fun x -> f x ; g x)

  let with_pp f = Staged.stage f

  let eval fs f =
    let f = Staged.unstage f in
    f fs

  let protect t ~on_error =
    let t = Staged.unstage t in
    Staged.stage (fun fs ->
        try t fs
        with exn ->
          Format.pp_print_flush fs () ;
          on_error exn )

  let lazy_ f =
    Staged.stage (fun fs ->
        let k = Staged.unstage (f ()) in
        k fs )
end

include T

type s = (unit, Format.formatter, unit) format

type sp = Blank | Cut | Space | Break of int * int

let ( >$ ) f g x = f $ g x

let set_margin n =
  with_pp (fun fs -> Format.pp_set_geometry fs ~max_indent:n ~margin:(n + 1))

let max_indent = ref None

let set_max_indent n = with_pp (fun _ -> max_indent := Some n)

(** Debug of formatting -------------------------------------------------*)

let pp_color_k color_code k fs =
  let c = Format.sprintf "\x1B[%dm" in
  Format.fprintf fs "@<0>%s%t@<0>%s" (c color_code) k (c 0)

(** Break hints and format strings --------------------------------------*)

let break n o = with_pp (fun fs -> Format.pp_print_break fs n o)

let cbreak ~fits ~breaks =
  with_pp (fun fs -> Format.pp_print_custom_break fs ~fits ~breaks)

let noop = with_pp (fun _ -> ())

let sequence l =
  let rec go l len =
    match l with
    | [] -> noop
    | [x] -> x
    | l ->
        let a_len = len / 2 in
        let b_len = len - a_len in
        let a, b = List.split_n l a_len in
        go a a_len $ go b b_len
  in
  go l (List.length l)

let fmt f = with_pp (fun fs -> Format.fprintf fs f)

(** Primitive types -----------------------------------------------------*)

let char c = with_pp (fun fs -> Format.pp_print_char fs c)

let str s =
  with_pp (fun fs ->
      if not (String.is_empty s) then Format.pp_print_string fs s )

let sp = function
  | Blank -> char ' '
  | Cut -> fmt "@,"
  | Space -> fmt "@ "
  | Break (x, y) -> break x y

(** Primitive containers ------------------------------------------------*)

let opt o f = Option.value_map ~default:noop ~f o

let list_pn x1N pp =
  match x1N with
  | [] -> noop
  | [x1] -> lazy_ (fun () -> pp ~prev:None x1 ~next:None)
  | x1 :: (x2 :: _ as x2N) ->
      let l =
        let rec aux (prev, acc) = function
          | [] -> acc
          | [xI] -> aux (xI, (Some prev, xI, None) :: acc) []
          | xI :: (xJ :: _ as xJN) ->
              aux (xI, (Some prev, xI, Some xJ) :: acc) xJN
        in
        aux (x1, [(None, x1, Some x2)]) x2N
      in
      List.rev_map l ~f:(fun (prev, x, next) ->
          lazy_ (fun () -> pp ~prev x ~next) )
      |> sequence

let list_fl xs pp =
  list_pn xs (fun ~prev x ~next ->
      pp ~first:(Option.is_none prev) ~last:(Option.is_none next) x )

let list_k l sep f = List.map l ~f |> List.intersperse ~sep |> sequence

let list xs sep pp = list_k xs (fmt sep) pp

(** Conditional formatting ----------------------------------------------*)

let fmt_if_k cnd x = if cnd then x else noop

let fmt_if cnd f = fmt_if_k cnd (fmt f)

let fmt_or_k cnd t f = if cnd then t else f

let fmt_or cnd t f = fmt_or_k cnd (fmt t) (fmt f)

let fmt_opt o = Option.value o ~default:noop

(** Conditional on immediately following a line break -------------------*)

let if_newline s = with_pp (fun fs -> Format.pp_print_string_if_newline fs s)

let break_unless_newline n o =
  with_pp (fun fs -> Format.pp_print_or_newline fs n o "" "")

(** Conditional on breaking of enclosing box ----------------------------*)

type behavior = Fit | Break

let fits_or_breaks ~level fits nspaces offset breaks =
  with_pp (fun fs ->
      Format.pp_print_fits_or_breaks fs ~level fits nspaces offset breaks )

let fits_breaks ?force ?(hint = (0, Int.min_value)) ?(level = 0) fits breaks
    =
  let nspaces, offset = hint in
  match force with
  | Some Fit -> str fits
  | Some Break -> fmt_if_k (offset >= 0) (break nspaces offset) $ str breaks
  | None -> fits_or_breaks ~level fits nspaces offset breaks

let fits_breaks_if ?force ?hint ?level cnd fits breaks =
  fmt_if_k cnd (fits_breaks ?force ?hint ?level fits breaks)

(** Wrapping ------------------------------------------------------------*)

let wrap_if_k cnd pre suf k = fmt_if_k cnd pre $ k $ fmt_if_k cnd suf

let wrap_k x = wrap_if_k true x

let wrap_if cnd pre suf = wrap_if_k cnd (fmt pre) (fmt suf)

and wrap pre suf = wrap_k (fmt pre) (fmt suf)

let wrap_if_fits_and cnd pre suf k =
  fits_breaks_if cnd pre "" $ k $ fits_breaks_if cnd suf ""

let wrap_if_fits_or cnd pre suf k =
  if cnd then wrap_k (str pre) (str suf) k
  else fits_breaks pre "" $ k $ fits_breaks suf ""

let wrap_fits_breaks_if ?(space = true) c cnd pre suf k =
  match (c.Conf.indicate_multiline_delimiters, space) with
  | `No, false -> wrap_if_k cnd (str pre) (str suf) k
  | `Space, _ | `No, true ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1, 0) suf
  | `Closing_on_separate_line, _ ->
      fits_breaks_if cnd pre (pre ^ " ")
      $ k
      $ fits_breaks_if cnd suf ~hint:(1000, 0) suf

let wrap_fits_breaks ?(space = true) conf x =
  wrap_fits_breaks_if ~space conf true x

(** Boxes ---------------------------------------------------------------*)

let box_debug_enabled = ref false

let with_box_debug k =
  let g = !box_debug_enabled in
  with_pp (fun _ -> box_debug_enabled := true)
  $ k
  $ with_pp (fun _ -> box_debug_enabled := g)

let box_depth = ref 0

(* Numeric part of the ANSI escape sequence for colors *)
let box_depth_colors = [|32; 33; 94; 31; 35; 36|]

let box_depth_color () =
  box_depth_colors.(!box_depth % Array.length box_depth_colors)

let debug_box_open ?name box_kind n fs =
  if !box_debug_enabled then (
    let name =
      match name with
      | Some s -> Format.sprintf "%s:%s" box_kind s
      | None -> box_kind
    in
    let openning = if n = 0 then name else Format.sprintf "%s<%d" name n in
    pp_color_k (box_depth_color ())
      (fun fs -> Format.fprintf fs "@<0>[@<0>%s@<0>>" openning)
      fs ;
    Int.incr box_depth )

let debug_box_close fs =
  if !box_debug_enabled then
    if !box_depth = 0 then
      (* mismatched close, red background *)
      pp_color_k 41 (fun fs -> Format.fprintf fs "@<0>]") fs
    else (
      Int.decr box_depth ;
      pp_color_k (box_depth_color ())
        (fun fs -> Format.fprintf fs "@<0>]")
        fs )

let apply_max_indent n = Option.value_map !max_indent ~f:(min n) ~default:n

let open_box ?name n =
  with_pp (fun fs ->
      let n = apply_max_indent n in
      debug_box_open ?name "b" n fs ;
      Format.pp_open_box fs n )

and open_vbox ?name n =
  with_pp (fun fs ->
      let n = apply_max_indent n in
      debug_box_open ?name "v" n fs ;
      Format.pp_open_vbox fs n )

and open_hvbox ?name n =
  with_pp (fun fs ->
      let n = apply_max_indent n in
      debug_box_open ?name "hv" n fs ;
      Format.pp_open_hvbox fs n )

and open_hovbox ?name n =
  with_pp (fun fs ->
      let n = apply_max_indent n in
      debug_box_open ?name "hov" n fs ;
      Format.pp_open_hovbox fs n )

and close_box =
  with_pp (fun fs -> debug_box_close fs ; Format.pp_close_box fs ())

(** Wrapping boxes ------------------------------------------------------*)

let cbox ?name n = wrap_k (open_box ?name n) close_box

and vbox ?name n = wrap_k (open_vbox ?name n) close_box

and hvbox ?name n = wrap_k (open_hvbox ?name n) close_box

and hovbox ?name n = wrap_k (open_hovbox ?name n) close_box

and cbox_if ?name cnd n = wrap_if_k cnd (open_box ?name n) close_box

and vbox_if ?name cnd n = wrap_if_k cnd (open_vbox ?name n) close_box

and hvbox_if ?name cnd n = wrap_if_k cnd (open_hvbox ?name n) close_box

and hovbox_if ?name cnd n = wrap_if_k cnd (open_hovbox ?name n) close_box

(** Text filling --------------------------------------------------------*)

let utf8_length s =
  Uuseg_string.fold_utf_8 `Grapheme_cluster (fun n _ -> n + 1) 0 s

let print_as size s = with_pp (fun fs -> Format.pp_print_as fs size s)

let fill_text ?epi text =
  assert (not (String.is_empty text)) ;
  let fmt_line line =
    let words =
      List.filter ~f:(Fn.non String.is_empty)
        (String.split_on_chars line
           ~on:['\t'; '\n'; '\011'; '\012'; '\r'; ' '] )
    in
    list words "@ " (fun s -> print_as (utf8_length s) s)
  in
  let lines =
    List.remove_consecutive_duplicates
      ~equal:(fun x y -> String.is_empty x && String.is_empty y)
      (String.split (String.rstrip text) ~on:'\n')
  in
  fmt_if (String.starts_with_whitespace text) " "
  $ hovbox 0
      ( hvbox 0
          (hovbox 0
             (list_pn lines (fun ~prev:_ curr ~next ->
                  fmt_line curr
                  $
                  match next with
                  | Some str when String.for_all str ~f:Char.is_whitespace ->
                      close_box $ fmt "\n@," $ open_hovbox 0
                  | Some _ when not (String.is_empty curr) -> fmt "@ "
                  | _ -> noop ) ) )
      $ fmt_if
          (String.length text > 1 && String.ends_with_whitespace text)
          " "
      $ opt epi Fn.id )
