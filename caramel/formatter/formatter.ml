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

(** OCamlFormat *)

open Ocamlformat_lib

module R_op = struct
  let ( let+ ) = Rresult.R.( >>| )

  let ( let* ) = Rresult.R.( >>= )
end

type action = In_place

type input = { sources : Fpath.t list; action : action }

;;
Caml.at_exit (Format.pp_print_flush Format.err_formatter)

;;
Caml.at_exit (Format_.pp_print_flush Format_.err_formatter)

type kind = Kind : _ list Migrate_ast.Traverse.fragment -> kind

let kind_of_ext : Fpath.t -> (kind, _) result =
 fun fname ->
  match Filename.extension (Fpath.to_string fname) with
  | ".ml" -> Ok (Kind Migrate_ast.Traverse.Use_file)
  | ".mli" -> Ok (Kind Migrate_ast.Traverse.Signature)
  | _ -> Error (`Unsupported_file_extension fname)

let default_config : Conf.t =
  {
    align_cases = true;
    align_constructors_decl = true;
    align_variants_decl = true;
    assignment_operator = `Begin_line;
    break_before_in = `Auto;
    break_cases = `All;
    break_collection_expressions = `Wrap;
    break_fun_decl = `Smart;
    break_fun_sig = `Smart;
    break_infix = `Fit_or_vertical;
    break_infix_before_func = false;
    break_separators = `After;
    break_sequences = true;
    break_string_literals = `Auto;
    break_struct = true;
    cases_exp_indent = 2;
    cases_matching_exp_indent = `Normal;
    comment_check = true;
    disable = false;
    disambiguate_non_breaking_match = true;
    doc_comments = `Before;
    doc_comments_padding = 2;
    doc_comments_tag_only = `Fit;
    dock_collection_brackets = true;
    exp_grouping = `Parens;
    extension_indent = 2;
    extension_sugar = `Always;
    field_space = `Loose;
    function_indent = 2;
    function_indent_nested = `Auto;
    if_then_else = `Keyword_first;
    indent_after_in = 2;
    indicate_multiline_delimiters = `Space;
    indicate_nested_or_patterns = `Space;
    infix_precedence = `Parens;
    leading_nested_match_parens = true;
    let_and = `Compact;
    let_binding_indent = 2;
    let_binding_spacing = `Sparse;
    let_module = `Sparse;
    let_open = `Short;
    margin = 80;
    match_indent = 2;
    match_indent_nested = `Always;
    max_indent = Some 10;
    max_iters = 5;
    module_item_spacing = `Sparse;
    nested_match = `Wrap;
    ocp_indent_compat = false;
    parens_ite = true;
    parens_tuple = `Always;
    parens_tuple_patterns = `Always;
    parse_docstrings = true;
    quiet = true;
    sequence_blank_line = `Preserve_one;
    sequence_style = `Separator;
    single_case = `Sparse;
    space_around_arrays = true;
    space_around_lists = true;
    space_around_records = true;
    space_around_variants = true;
    stritem_extension_indent = 2;
    type_decl = `Sparse;
    type_decl_indent = 2;
    wrap_comments = true;
    wrap_fun_args = true;
  }

let default_opts : Conf.opts =
  { debug = false; margin_check = true; format_invalid_files = true }

let format { sources; action } =
  match action with
  | In_place ->
      let results =
        List.map
          (fun filename ->
            let open R_op in
            let input_name = Fpath.to_string filename in
            let* (Kind kind) = kind_of_ext filename in
            let* source =
              Bos.OS.File.read filename
              |> Rresult.R.reword_error (fun e -> `File_read_error e)
            in
            let* formatted =
              Translation_unit.parse_and_format kind ?output_file:None
                ~input_name ~source default_config default_opts
              |> Rresult.R.reword_error (fun e -> `Translation_error e)
            in
            let* () =
              if not (String.equal formatted source) then
                Bos.OS.File.write filename formatted
                |> Rresult.R.reword_error (fun e -> `File_write_error e)
              else Ok ()
            in

            Ok ())
          sources
      in
      List.fold_left
        (fun acc result ->
          match (result, acc) with
          | Error err, Ok () -> Error [ err ]
          | Error err, Error errs -> Error (err :: errs)
          | _ -> acc)
        (Ok ()) results
