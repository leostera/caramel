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

(** Translation units *)

open Migrate_ast
open Parse_with_comments

exception
  Internal_error of
    [ `Cannot_parse of exn
    | `Ast_changed
    | `Doc_comment of Normalize.docstring_error list
    | `Comment
    | `Comment_dropped of Cmt.t list
    | `Warning50 of (Location.t * Warnings.t) list ]
    * (string * Sexp.t) list

let internal_error msg kvs = raise (Internal_error (msg, kvs))

type error =
  | Invalid_source of {exn: exn}
  | Unstable of {iteration: int; prev: string; next: string}
  | Ocamlformat_bug of {exn: exn}
  | User_error of string

let ellipsis n msg =
  let msg = String.strip msg in
  if n > 0 && String.length msg > (n * 2) + 10 then
    Format.sprintf "%s ... %s" (String.prefix msg n) (String.suffix msg n)
  else msg

let ellipsis_cmt = ellipsis 50

let with_file input_name output_file suf ext f =
  let dir =
    match output_file with
    | Some filename -> Filename.dirname filename
    | None -> Filename.get_temp_dir_name ()
  in
  let base = Filename.remove_extension (Filename.basename input_name) in
  let tmp = Filename.concat dir (base ^ suf ^ ext) in
  Out_channel.with_file tmp ~f ;
  tmp

let dump_ast ~input_name ?output_file ~suffix fmt =
  let ext = ".ast" in
  with_file input_name output_file suffix ext (fun oc ->
      fmt (Format.formatter_of_out_channel oc) )

let dump_formatted ~input_name ?output_file ~suffix fmted =
  let ext = Filename.extension input_name in
  with_file input_name output_file suffix ext (fun oc ->
      Out_channel.output_string oc fmted )

let print_error ~fmt ~exe ~debug ~quiet ~input_name error =
  match error with
  | Invalid_source _ when quiet -> ()
  | Invalid_source {exn} -> (
      let reason =
        match exn with
        | Syntaxerr.Error _ | Lexer.Error _ -> " (syntax error)"
        | Warning50 _ -> " (misplaced documentation comments - warning 50)"
        | _ -> ""
      in
      Format.fprintf fmt "%s: ignoring %S%s\n%!" exe input_name reason ;
      match exn with
      | Syntaxerr.Error _ | Lexer.Error _ ->
          Location.report_exception fmt exn
      | Warning50 l ->
          List.iter l ~f:(fun (l, w) -> print_warning l w) ;
          Format.fprintf fmt
            "@{<warning>Hint@}: (Warning 50) This file contains a \
             documentation comment (** ... *) that the OCaml compiler does \
             not know how to attach to the AST. OCamlformat does not \
             support these cases. You can find more information at: \
             https://github.com/ocaml-ppx/ocamlformat#overview. If you'd \
             like to disable this check and let ocamlformat make a choice \
             (though it might not be consistent with the ocaml compilers \
             and odoc), you can set the --no-comment-check option.\n\
             %!"
      | exn -> Format.fprintf fmt "%s\n%!" (Exn.to_string exn) )
  | Unstable {iteration; prev; next} ->
      if debug then (
        let ext = Filename.extension input_name in
        let input_name =
          Filename.chop_extension (Filename.basename input_name)
        in
        let p =
          Filename.temp_file input_name (Printf.sprintf ".prev%s" ext)
        in
        Out_channel.write_all p ~data:prev ;
        let n =
          Filename.temp_file input_name (Printf.sprintf ".next%s" ext)
        in
        Out_channel.write_all n ~data:next ;
        ignore (Unix.system (Printf.sprintf "diff %S %S 1>&2" p n)) ;
        Unix.unlink p ;
        Unix.unlink n ) ;
      if iteration <= 1 then
        Format.fprintf fmt
          "%s: %S was not already formatted. ([max-iters = 1])\n%!" exe
          input_name
      else (
        Format.fprintf fmt
          "%s: Cannot process %S.\n\
          \  Please report this bug at \
           https://github.com/ocaml-ppx/ocamlformat/issues.\n\
           %!"
          exe input_name ;
        Format.fprintf fmt
          "  BUG: formatting did not stabilize after %i iterations.\n%!"
          iteration )
  | User_error msg -> Format.fprintf fmt "%s: %s.\n%!" exe msg
  | Ocamlformat_bug {exn} -> (
      Format.fprintf fmt
        "%s: Cannot process %S.\n\
        \  Please report this bug at \
         https://github.com/ocaml-ppx/ocamlformat/issues.\n\
         %!"
        exe input_name ;
      match exn with
      | Internal_error (m, l) ->
          let s =
            match m with
            | `Cannot_parse _ -> "generating invalid ocaml syntax"
            | `Ast_changed -> "ast changed"
            | `Doc_comment _ -> "doc comments changed"
            | `Comment -> "comments changed"
            | `Comment_dropped _ -> "comments dropped"
            | `Warning50 _ -> "misplaced documentation comments"
          in
          Format.fprintf fmt "  BUG: %s.\n%!" s ;
          ( match m with
          | `Doc_comment l when not quiet ->
              List.iter l ~f:(function
                | Normalize.Moved (loc_before, loc_after, msg) ->
                    if Location.compare loc_before Location.none = 0 then
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Docstring (** %s *) added.\n\
                         %!"
                        Location.print loc_after (ellipsis_cmt msg)
                    else if Location.compare loc_after Location.none = 0 then
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Docstring (** %s *) dropped.\n\
                         %!"
                        Location.print loc_before (ellipsis_cmt msg)
                    else
                      Format.fprintf fmt
                        "%!@{<loc>%a@}:@,\
                         @{<error>Error@}: Docstring (** %s *) moved to \
                         @{<loc>%a@}.\n\
                         %!"
                        Location.print loc_before (ellipsis_cmt msg)
                        Location.print loc_after
                | Normalize.Unstable (loc, s) ->
                    Format.fprintf fmt
                      "%!@{<loc>%a@}:@,\
                       @{<error>Error@}: Formatting of (** %s *) is \
                       unstable (e.g. parses as a list or not depending on \
                       the margin), please tighten up this comment in the \
                       source or disable the formatting using the option \
                       --no-parse-docstrings.\n\
                       %!"
                      Location.print loc (ellipsis_cmt s) )
          | `Comment_dropped l when not quiet ->
              List.iter l ~f:(fun Cmt.{txt= msg; loc} ->
                  Format.fprintf fmt
                    "%!@{<loc>%a@}:@,\
                     @{<error>Error@}: Comment (* %s *) dropped.\n\
                     %!"
                    Location.print loc (ellipsis_cmt msg) )
          | `Cannot_parse ((Syntaxerr.Error _ | Lexer.Error _) as exn) ->
              if debug then Location.report_exception fmt exn
          | `Warning50 l ->
              if debug then List.iter l ~f:(fun (l, w) -> print_warning l w)
          | _ -> () ) ;
          if debug then
            List.iter l ~f:(fun (msg, sexp) ->
                Format.fprintf fmt "  %s: %s\n%!" msg (Sexp.to_string sexp) )
      | exn ->
          Format.fprintf fmt
            "  BUG: unhandled exception. Use [--debug] for details.\n%!" ;
          if debug then Format.fprintf fmt "%s\n%!" (Exn.to_string exn) )

let check_all_locations fmt cmts_t =
  match Cmts.remaining_locs cmts_t with
  | [] -> ()
  | l ->
      let print l = Format.fprintf fmt "%a\n%!" Location.print l in
      Format.fprintf fmt
        "Warning: Some locations have not been considered\n%!" ;
      List.iter ~f:print (List.sort l ~compare:Location.compare)

let check_margin conf ~filename ~fmted =
  List.iteri (String.split_lines fmted) ~f:(fun i line ->
      if String.length line > conf.Conf.margin then
        Format.fprintf Format.err_formatter
          "Warning: %s:%i exceeds the margin\n%!" filename i )

let with_optional_box_debug ~box_debug k =
  if box_debug then Fmt.with_box_debug k else k

let with_buffer_formatter ~buffer_size k =
  let buffer = Buffer.create buffer_size in
  let fs = Format_.formatter_of_buffer buffer in
  Fmt.eval fs k ;
  Format_.pp_print_flush fs () ;
  if Buffer.length buffer > 0 then Format_.pp_print_newline fs () ;
  Buffer.contents buffer

let equal fragment ~ignore_doc_comments c a b =
  Normalize.equal fragment ~ignore_doc_comments c a.Parse_with_comments.ast
    b.Parse_with_comments.ast

let normalize fragment c {Parse_with_comments.ast; _} =
  Normalize.normalize fragment c ast

let recover (type a) : a Traverse.fragment -> _ = function
  | Traverse.Structure -> Parse_wyc.Make_parsable.structure
  | Traverse.Signature -> Parse_wyc.Make_parsable.signature
  | Traverse.Use_file -> Parse_wyc.Make_parsable.use_file

let format fragment ?output_file ~input_name ~prev_source ~parsed conf opts =
  let open Result.Monad_infix in
  let dump_ast ~suffix ast =
    if opts.Conf.debug then
      Some
        (dump_ast ~input_name ?output_file ~suffix (fun fmt ->
             Migrate_ast.Printast.fragment fragment fmt ast ) )
    else None
  in
  let dump_formatted ~suffix fmted =
    if opts.debug then
      Some (dump_formatted ~input_name ?output_file ~suffix fmted)
    else None
  in
  Ocaml_common.Location.input_name := input_name ;
  (* iterate until formatting stabilizes *)
  let rec print_check ~i ~(conf : Conf.t) ~prev_source t =
    let format ~box_debug =
      let open Fmt in
      let cmts_t =
        Cmts.init fragment ~debug:opts.debug t.source t.ast t.comments
      in
      let contents =
        with_buffer_formatter
          ~buffer_size:(String.length prev_source)
          ( set_margin conf.margin
          $ opt conf.max_indent set_max_indent
          $ fmt_if_k
              (not (String.is_empty t.prefix))
              (str t.prefix $ fmt "@.")
          $ with_optional_box_debug ~box_debug
              (Fmt_ast.fmt_fragment fragment ~debug:opts.debug t.source
                 cmts_t conf t.ast ) )
      in
      (contents, cmts_t)
    in
    if opts.debug then
      format ~box_debug:true |> fst
      |> dump_formatted ~suffix:".boxes"
      |> (ignore : string option -> unit) ;
    let fmted, cmts_t = format ~box_debug:false in
    let conf = if opts.debug then conf else {conf with Conf.quiet= true} in
    if String.equal prev_source fmted then (
      if opts.debug then check_all_locations Format.err_formatter cmts_t ;
      if opts.Conf.margin_check then
        check_margin conf ~fmted
          ~filename:(Option.value output_file ~default:input_name) ;
      Ok fmted )
    else
      let exn_args () =
        [("output file", dump_formatted ~suffix:".invalid-ast" fmted)]
        |> List.filter_map ~f:(fun (s, f_opt) ->
               Option.map f_opt ~f:(fun f -> (s, String.sexp_of_t f)) )
      in
      ( match parse fragment conf ~source:fmted with
      | exception Sys_error msg -> Error (User_error msg)
      | exception Warning50 l -> internal_error (`Warning50 l) (exn_args ())
      | exception exn ->
          if opts.Conf.format_invalid_files then (
            match parse fragment conf ~source:(recover fragment fmted) with
            | exception exn ->
                internal_error (`Cannot_parse exn) (exn_args ())
            | t_new ->
                Format.fprintf Format.err_formatter
                  "Warning: %s is invalid, recovering.\n%!" input_name ;
                Ok t_new )
          else internal_error (`Cannot_parse exn) (exn_args ())
      | t_new -> Ok t_new )
      >>= fun t_new ->
      (* Ast not preserved ? *)
      ( if
        not
          (equal fragment ~ignore_doc_comments:(not conf.comment_check) conf
             t t_new )
      then
        let old_ast = dump_ast ~suffix:".old" (normalize fragment conf t) in
        let new_ast =
          dump_ast ~suffix:".new" (normalize fragment conf t_new)
        in
        let args ~suffix =
          [ ("output file", dump_formatted ~suffix fmted)
          ; ("old ast", old_ast)
          ; ("new ast", new_ast) ]
          |> List.filter_map ~f:(fun (s, f_opt) ->
                 Option.map f_opt ~f:(fun f -> (s, String.sexp_of_t f)) )
        in
        if equal fragment ~ignore_doc_comments:true conf t t_new then
          let docstrings =
            Normalize.moved_docstrings fragment conf
              t.Parse_with_comments.ast t_new.Parse_with_comments.ast
          in
          let args = args ~suffix:".unequal-docs" in
          internal_error (`Doc_comment docstrings) args
        else
          let args = args ~suffix:".unequal-ast" in
          internal_error `Ast_changed args ) ;
      (* Comments not preserved ? *)
      if conf.comment_check then (
        ( match Cmts.remaining_comments cmts_t with
        | [] -> ()
        | l -> internal_error (`Comment_dropped l) [] ) ;
        let is_docstring Cmt.{txt; _} =
          conf.parse_docstrings && Char.equal txt.[0] '*'
        in
        let old_docstrings, old_comments =
          List.partition_tf t.comments ~f:is_docstring
        in
        let t_newdocstrings, t_newcomments =
          List.partition_tf t_new.comments ~f:is_docstring
        in
        let f = ellipsis_cmt in
        let f x = Either.First.map ~f x |> Either.Second.map ~f in
        let diff_cmts =
          Sequence.append
            (Cmts.diff conf old_comments t_newcomments)
            (Fmt_odoc.diff conf old_docstrings t_newdocstrings)
          |> Sequence.map ~f
        in
        if not (Sequence.is_empty diff_cmts) then
          let old_ast = dump_ast ~suffix:".old" t.ast in
          let new_ast = dump_ast ~suffix:".new" t_new.ast in
          let args =
            [ ( "diff"
              , Some
                  (Sequence.sexp_of_t
                     (Either.sexp_of_t String.sexp_of_t String.sexp_of_t)
                     diff_cmts ) )
            ; ("old ast", Option.map old_ast ~f:String.sexp_of_t)
            ; ("new ast", Option.map new_ast ~f:String.sexp_of_t) ]
            |> List.filter_map ~f:(fun (s, f_opt) ->
                   Option.map f_opt ~f:(fun f -> (s, f)) )
          in
          internal_error `Comment args ) ;
      (* Too many iteration ? *)
      if i >= conf.max_iters then (
        Caml.flush_all () ;
        Error (Unstable {iteration= i; prev= prev_source; next= fmted}) )
      else
        (* All good, continue *)
        print_check ~i:(i + 1) ~conf ~prev_source:fmted t_new
  in
  try print_check ~i:1 ~conf ~prev_source parsed with
  | Sys_error msg -> Error (User_error msg)
  | exn -> Error (Ocamlformat_bug {exn})

let parse_result fragment conf (opts : Conf.opts) ~source ~input_name =
  match parse fragment conf ~source with
  | exception exn ->
      if opts.format_invalid_files then (
        match parse fragment conf ~source:(recover fragment source) with
        | exception exn -> Error (Invalid_source {exn})
        | parsed ->
            Format.fprintf Format.err_formatter
              "Warning: %s is invalid, recovering.\n%!" input_name ;
            Ok parsed )
      else Error (Invalid_source {exn})
  | parsed -> Ok parsed

let parse_and_format fragment ?output_file ~input_name ~source conf opts =
  Ocaml_common.Location.input_name := input_name ;
  let open Result.Monad_infix in
  parse_result fragment conf opts ~source ~input_name
  >>= fun parsed ->
  format fragment ?output_file ~input_name ~prev_source:source ~parsed conf
    opts
