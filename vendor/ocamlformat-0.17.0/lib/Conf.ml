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

(** Configuration options *)

type t =
  { align_cases: bool
  ; align_constructors_decl: bool
  ; align_variants_decl: bool
  ; assignment_operator: [`Begin_line | `End_line]
  ; break_before_in: [`Fit_or_vertical | `Auto]
  ; break_cases: [`Fit | `Nested | `Toplevel | `Fit_or_vertical | `All]
  ; break_collection_expressions: [`Wrap | `Fit_or_vertical]
  ; break_infix: [`Wrap | `Fit_or_vertical]
  ; break_infix_before_func: bool
  ; break_fun_decl: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_fun_sig: [`Wrap | `Fit_or_vertical | `Smart]
  ; break_separators: [`Before | `After]
  ; break_sequences: bool
  ; break_string_literals: [`Auto | `Never]
  ; break_struct: bool
  ; cases_exp_indent: int
  ; cases_matching_exp_indent: [`Normal | `Compact]
  ; comment_check: bool
  ; disable: bool
  ; disambiguate_non_breaking_match: bool
  ; doc_comments: [`Before | `Before_except_val | `After_when_possible]
  ; doc_comments_padding: int
  ; doc_comments_tag_only: [`Fit | `Default]
  ; dock_collection_brackets: bool
  ; exp_grouping: [`Parens | `Preserve]
  ; extension_indent: int
  ; field_space: [`Tight | `Loose | `Tight_decl]
  ; function_indent: int
  ; function_indent_nested: [`Always | `Auto | `Never]
  ; if_then_else: [`Compact | `Fit_or_vertical | `Keyword_first | `K_R]
  ; indent_after_in: int
  ; indicate_multiline_delimiters: [`No | `Space | `Closing_on_separate_line]
  ; indicate_nested_or_patterns: [`Space | `Unsafe_no]
  ; infix_precedence: [`Indent | `Parens]
  ; leading_nested_match_parens: bool
  ; let_and: [`Compact | `Sparse]
  ; let_binding_indent: int
  ; let_binding_spacing: [`Compact | `Sparse | `Double_semicolon]
  ; let_module: [`Compact | `Sparse]
  ; margin: int
  ; match_indent: int
  ; match_indent_nested: [`Always | `Auto | `Never]
  ; max_indent: int option
  ; max_iters: int
  ; module_item_spacing: [`Compact | `Preserve | `Sparse]
  ; nested_match: [`Wrap | `Align]
  ; ocp_indent_compat: bool
  ; parens_ite: bool
  ; parens_tuple: [`Always | `Multi_line_only]
  ; parens_tuple_patterns: [`Always | `Multi_line_only]
  ; parse_docstrings: bool
  ; quiet: bool
  ; sequence_blank_line: [`Compact | `Preserve_one]
  ; sequence_style: [`Before | `Separator | `Terminator]
  ; single_case: [`Compact | `Sparse]
  ; space_around_arrays: bool
  ; space_around_lists: bool
  ; space_around_records: bool
  ; space_around_variants: bool
  ; stritem_extension_indent: int
  ; type_decl: [`Compact | `Sparse]
  ; type_decl_indent: int
  ; wrap_comments: bool
  ; wrap_fun_args: bool }

let profile_option_names = ["p"; "profile"]

open Cmdliner

let warn_raw, collect_warnings =
  let delay_warning = ref false in
  let delayed_warning_list = ref [] in
  let warn_ s =
    if !delay_warning then delayed_warning_list := s :: !delayed_warning_list
    else Format.eprintf "%s" s
  in
  let collect_warnings f =
    let old_flag, old_list = (!delay_warning, !delayed_warning_list) in
    delay_warning := true ;
    delayed_warning_list := [] ;
    let res = f () in
    let collected = List.rev !delayed_warning_list in
    delay_warning := old_flag ;
    delayed_warning_list := old_list ;
    (res, fun () -> List.iter ~f:warn_ collected)
  in
  (warn_, collect_warnings)

let warn ?filename ?lnum fmt =
  Format.kasprintf
    (fun s ->
      let loc : string =
        match (filename, lnum) with
        | Some file, Some lnum ->
            Format.asprintf "File %a, line %d:@\n" Fpath.pp file lnum
        | Some file, None -> Format.asprintf "File %a@\n" Fpath.pp file
        | None, _ -> ""
      in
      warn_raw (Format.asprintf "%sWarning: %s@\n" loc s) )
    fmt

module C = Config_option.Make (struct
  type config = t

  let profile_option_names = profile_option_names

  let warn (config : config) fmt =
    Format.kasprintf (fun s -> if not config.quiet then warn "%s" s) fmt
end)

let info =
  let doc = "A tool to format OCaml code." in
  let man =
    [ `S Cmdliner.Manpage.s_description
    ; `P "$(tname) automatically formats OCaml code."
    ; `S (C.section_name `Formatting)
    ; `P
        "Unless otherwise noted, any option \
         $(b,--)$(i,option)$(b,=)$(i,VAL) detailed in this section can be \
         set in many ways, its value is determined in the following order \
         (of increasing priority): the default value is used if no other \
         value is specified. The value of a boolean option $(b,--foo) or \
         $(b,--no-foo) can be modified in an $(b,.ocamlformat) \
         configuration file with '$(b,foo = ){$(b,true),$(b,false)}', it \
         can be done for any other option with an '$(b,option = )$(i,VAL)' \
         line (*), or using the OCAMLFORMAT environment variable: \
         $(b,OCAMLFORMAT=)$(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL), \
         or as an optional parameter on the command line, or with a global \
         $(b,[@@@ocamlformat \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute \
         in the processed file, or with an $(b,[@@ocamlformat \
         \")$(i,option)$(b,=)$(i,VAL)$(b,\"]) attribute on expression in \
         the processed file."
    ; `P
        "(*) $(b,.ocamlformat) files in current and all ancestor \
         directories for each input file are used, as well as the global \
         $(b,ocamlformat) file defined in $(b,\\$XDG_CONFIG_HOME) or in \
         $(b,\\$HOME/.config) if $(b,\\$XDG_CONFIG_HOME) is undefined. The \
         global $(b,ocamlformat) file has the lowest priority, then the \
         closer the directory is to the processed file, the higher the \
         priority. The global $(b,ocamlformat) file is only used when the \
         option $(b,enable-outside-detected-project) is set."
    ; `P
        "If the $(b,disable) option is not set, an $(b,.ocamlformat-ignore) \
         file specifies files that OCamlFormat should ignore. Each line in \
         an $(b,.ocamlformat-ignore) file specifies a filename relative to \
         the directory containing the $(b,.ocamlformat-ignore) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments."
    ; `P
        "If the $(b,disable) option is set, an $(b,.ocamlformat-enable) \
         file specifies files that OCamlFormat should format even when the \
         $(b,disable) option is set. Each line in an \
         $(b,.ocamlformat-enable) file specifies a filename relative to the \
         directory containing the $(b,.ocamlformat-enable) file. \
         Shell-style regular expressions are supported. Lines starting with \
         $(b,#) are ignored and can be used as comments." ]
  in
  Term.info "ocamlformat" ~version:Version.version ~doc ~man

(** Options affecting formatting *)
module Formatting = struct
  let section = `Formatting

  let align_cases =
    let doc = "Align match/try cases horizontally." in
    let names = ["align-cases"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with align_cases= x})
      (fun conf -> conf.align_cases)

  let align_constructors_decl =
    let doc = "Align type declarations horizontally." in
    let names = ["align-constructors-decl"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with align_constructors_decl= x})
      (fun conf -> conf.align_constructors_decl)

  let align_variants_decl =
    let doc = "Align type variants declarations horizontally." in
    let names = ["align-variants-decl"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with align_variants_decl= x})
      (fun conf -> conf.align_variants_decl)

  let assignment_operator =
    let doc = "Position of the assignment operator." in
    let names = ["assignment-operator"] in
    let all =
      [ ( "end-line"
        , `End_line
        , "$(b,end-line) positions assignment operators (`:=` and `<-`) at \
           the end of the line and breaks after it if the whole assignment \
           expression does not fit on a single line." )
      ; ( "begin-line"
        , `Begin_line
        , "$(b,begin-line) positions assignment operators (`:=` and `<-`) \
           at the beginning of the line and breaks before it if the whole \
           assignment expression does not fit on a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with assignment_operator= x})
      (fun conf -> conf.assignment_operator)

  let break_before_in =
    let doc =
      "Whether the line should break before the $(i,in) keyword of a \
       $(i,let) binding."
    in
    let names = ["break-before-in"] in
    let all =
      [ ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) will always break the line before the \
           $(i,in) keyword if the whole $(i,let) binding does not fit on a \
           single line." )
      ; ( "auto"
        , `Auto
        , "$(b,auto) will only break the line if the $(i,in) keyword does \
           not fit on the previous line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_before_in= x})
      (fun conf -> conf.break_before_in)

  let break_cases =
    let doc = "Break pattern match cases." in
    let names = ["break-cases"] in
    let all =
      [ ( "fit"
        , `Fit
        , "Specifying $(b,fit) lets pattern matches break at the margin \
           naturally." )
      ; ( "nested"
        , `Nested
        , "$(b,nested) forces a break after nested or-patterns to highlight \
           the case body. Note that with $(b,nested), the \
           $(b,indicate-nested-or-patterns) option is not needed, and so \
           ignored." )
      ; ( "toplevel"
        , `Toplevel
        , "$(b,toplevel) forces top-level cases (i.e. not nested \
           or-patterns) to break across lines, otherwise break naturally at \
           the margin." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) tries to fit all or-patterns on the same \
           line, otherwise breaks." )
      ; ( "all"
        , `All
        , "$(b,all) forces all pattern matches to break across lines." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_cases= x})
      (fun conf -> conf.break_cases)

  let break_collection_expressions =
    let doc =
      "Break collection expressions (lists and arrays) elements by elements."
    in
    let names = ["break-collection-expressions"] in
    let all =
      [ ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line." )
      ; ( "wrap"
        , `Wrap
        , "$(b,wrap) will group simple expressions and try to format them \
           in a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_collection_expressions= x})
      (fun conf -> conf.break_collection_expressions)

  let break_fun_decl =
    let doc = "Style for function declarations and types." in
    let names = ["break-fun-decl"] in
    let all =
      [ ("wrap", `Wrap, "$(b,wrap) breaks only if necessary.")
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line." )
      ; ( "smart"
        , `Smart
        , "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_fun_decl= x})
      (fun conf -> conf.break_fun_decl)

  let break_fun_sig =
    let doc = "Style for function signatures." in
    let names = ["break-fun-sig"] in
    let all =
      [ ("wrap", `Wrap, "$(b,wrap) breaks only if necessary.")
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks arguments if they do not \
           fit on a single line." )
      ; ( "smart"
        , `Smart
        , "$(b,smart) is like $(b,fit-or-vertical) but try to fit arguments \
           on their line if they fit." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_fun_sig= x})
      (fun conf -> conf.break_fun_sig)

  let break_infix =
    let doc = "Break sequence of infix operators." in
    let names = ["break-infix"] in
    let all =
      [ ( "wrap"
        , `Wrap
        , "$(b,wrap) will group simple expressions and try to format them \
           in a single line." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks expressions if they do \
           not fit on a single line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_infix= x})
      (fun conf -> conf.break_infix)

  let break_infix_before_func =
    let doc =
      "Break infix operators whose right arguments are anonymous functions \
       specially: do not break after the operator so that the first line of \
       the function appears docked at the end of line after the operator."
    in
    let names = ["break-infix-before-func"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with break_infix_before_func= x})
      (fun conf -> conf.break_infix_before_func)

  let break_separators =
    let doc =
      "Break before or after separators such as `;` in list or record \
       expressions."
    in
    let names = ["break-separators"] in
    let all =
      [ ( "after"
        , `After
        , "$(b,after) breaks the expressions after the separator." )
      ; ( "before"
        , `Before
        , "$(b,before) breaks the expressions before the separator." ) ]
    in
    C.choice ~names ~all ~doc ~section
      ~removed_values:
        [ C.removed_value ~name:"after-and-docked" ~version:"0.12"
            ~msg:
              "One can get a similar behaviour by setting \
               `break-separators=after`, `space-around-lists=false`, and \
               `dock-collection-brackets=false`." ]
      (fun conf x -> {conf with break_separators= x})
      (fun conf -> conf.break_separators)

  let break_sequences =
    let doc =
      "Force sequence expressions to break irrespective of margin."
    in
    let names = ["break-sequences"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with break_sequences= x})
      (fun conf -> conf.break_sequences)

  let break_string_literals =
    let doc = "Break string literals." in
    let names = ["break-string-literals"] in
    let all =
      [ ( "auto"
        , `Auto
        , "$(b,auto) mode breaks lines at newlines and wraps string \
           literals at the margin." )
      ; ( "never"
        , `Never
        , "$(b,never) mode formats string literals as they are parsed, in \
           particular, with escape sequences expanded." ) ]
    in
    C.choice ~names ~all ~doc ~section
      ~removed_values:
        (C.removed_values
           ~names:["newlines"; "newlines-and-wrap"; "wrap"]
           ~version:"0.12"
           ~msg:
             "It has been replaced by the new default `auto` value, which \
              breaks lines at newlines and wraps string literals at the \
              margin." )
      (fun conf x -> {conf with break_string_literals= x})
      (fun conf -> conf.break_string_literals)

  let break_struct =
    let doc = "Break struct-end module items." in
    let names = ["break-struct"] in
    let all =
      [ ( "force"
        , `Force
        , "$(b,force) will break struct-end phrases unconditionally." )
      ; ( "natural"
        , `Natural
        , "$(b,natural) will break struct-end phrases naturally at the \
           margin." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with break_struct= Poly.(x = `Force)})
      (fun conf -> if conf.break_struct then `Force else `Natural)

  let cases_exp_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of cases expressions ($(docv) columns). See also the \
       $(b,cases-matching-exp-indent) and $(b,nested-match) options."
    in
    let names = ["cases-exp-indent"] in
    C.any Arg.int ~names ~default:4 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with cases_exp_indent= x})
      (fun conf -> conf.cases_exp_indent)

  let cases_matching_exp_indent =
    let doc =
      "Indentation of cases right-hand sides which are `match` or `try` \
       expressions."
    in
    let names = ["cases-matching-exp-indent"] in
    let all =
      [ ( "normal"
        , `Normal
        , "$(b,normal) indents as it would any other expression." )
      ; ( "compact"
        , `Compact
        , "$(b,compact) forces an indentation of 2, unless \
           $(b,nested-match) is set to $(b,align) and we're on the last \
           case." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with cases_matching_exp_indent= x})
      (fun conf -> conf.cases_matching_exp_indent)

  let disable =
    let doc =
      "Disable ocamlformat. This is used in attributes to locally disable \
       automatic code formatting. One can also use $(b,[@@@ocamlformat \
       \"enable\"]) instead of $(b,[@@@ocamlformat \"disable=false\"])."
    in
    C.flag ~names:["disable"] ~default:false ~doc ~section
      (fun conf x -> {conf with disable= x})
      (fun conf -> conf.disable)

  let disambiguate_non_breaking_match =
    let doc =
      "Add parentheses around matching constructs that fit on a single line."
    in
    C.flag
      ~names:["disambiguate-non-breaking-match"]
      ~default:false ~doc ~section
      (fun conf x -> {conf with disambiguate_non_breaking_match= x})
      (fun conf -> conf.disambiguate_non_breaking_match)

  let doc_comments =
    let doc = "Doc comments position." in
    let names = ["doc-comments"] in
    let all =
      [ ( "after-when-possible"
        , `After_when_possible
        , "$(b,after-when-possible) puts doc comments after the \
           corresponding code. This option has no effect on variant \
           declarations because that would change their meaning and on \
           structures, signatures and objects for readability." )
      ; ( "before-except-val"
        , `Before_except_val
        , "$(b,before-except-val) puts doc comments before the \
           corresponding code, but puts doc comments of $(b,val) and \
           $(b,external) declarations after the corresponding declarations."
        )
      ; ( "before"
        , `Before
        , "$(b,before) puts comments before the corresponding code." ) ]
    in
    C.choice ~names ~all ~doc ~section
      ~removed_values:
        [ C.removed_value ~name:"after" ~version:"0.14.2"
            ~msg:
              "This value has been renamed `after-when-possible` to take \
               into account the technical limitations of ocamlformat, the \
               behavior is unchanged." ]
      (fun conf x -> {conf with doc_comments= x})
      (fun conf -> conf.doc_comments)

  let doc_comments_padding =
    let docv = "PADDING" in
    let doc =
      "Add $(docv) spaces before doc comments in type declarations."
    in
    let names = ["doc-comments-padding"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~section
      (fun conf x -> {conf with doc_comments_padding= x})
      (fun conf -> conf.doc_comments_padding)

  let doc_comments_tag_only =
    let doc = "Position of doc comments with only tags." in
    let names = ["doc-comments-tag-only"] in
    let all =
      [ ("default", `Default, "$(b,default) means no special treatment.")
      ; ("fit", `Fit, "$(b,fit) puts doc comments on the same line.") ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with doc_comments_tag_only= x})
      (fun conf -> conf.doc_comments_tag_only)

  let ( (* doc_comments_val *) ) =
    let names = ["doc-comments-val"] in
    let version = "0.16.0" in
    let msg =
      "If you are using `doc-comments-val=before` in combination with \
       `doc-comments=before` then only `doc-comments=before` is now \
       required to achive the same behavior. If you are using \
       `doc-comments-val=before` in combination with `doc-comments=after` \
       this behavior is not available anymore. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=before` \
       please now use `doc-comments=before-except-val`. If you are using \
       `doc-comments-val=after` in combination with `doc-comments=after` \
       then only `doc-comments=after-when-possible` is now required to \
       achieve the same behavior. If you are using `doc-comments-val=unset` \
       the same behavior can now be achieved by setting `doc-comments` \
       only."
    in
    C.removed_option ~names ~version ~msg

  let dock_collection_brackets =
    let doc =
      "Dock the brackets of lists, arrays and records, so that when the \
       collection does not fit on a single line the brackets are opened on \
       the preceding line and closed on the following line."
    in
    let names = ["dock-collection-brackets"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with dock_collection_brackets= x})
      (fun conf -> conf.dock_collection_brackets)

  let concrete_syntax_preserved_msg =
    "Concrete syntax will now always be preserved."

  let ( (* escape_chars *) ) =
    let names = ["escape-chars"] in
    let version = "0.16.0" in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~version ~msg

  let ( (* escape_strings *) ) =
    let names = ["escape-strings"] in
    let version = "0.16.0" in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~version ~msg

  let exp_grouping =
    let doc = "Style of expression grouping." in
    let names = ["exp-grouping"] in
    let all =
      [ ( "parens"
        , `Parens
        , "$(b,parens) groups expressions using parentheses." )
      ; ( "preserve"
        , `Preserve
        , "$(b,preserve) preserves the original grouping syntax \
           (parentheses or $(i,begin)/$(i,end))." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with exp_grouping= x})
      (fun conf -> conf.exp_grouping)

  let extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of items inside extension nodes ($(docv) columns)."
    in
    let names = ["extension-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~section
      (fun conf x -> {conf with extension_indent= x})
      (fun conf -> conf.extension_indent)

  let ( (* extension_sugar *) ) =
    let names = ["extension-sugar"] in
    let version = "0.17.0" in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~version ~msg

  let field_space =
    let doc =
      "Whether or not to use a space between a field name and the rhs. This \
       option affects records and objects."
    in
    let names = ["field-space"] in
    let all =
      [ ("loose", `Loose, "$(b,loose) does.")
      ; ( "tight"
        , `Tight
        , "$(b,tight) does not use a space between a field name and the \
           punctuation symbol (`:` or `=`)." )
      ; ( "tight-decl"
        , `Tight_decl
        , "$(b,tight-decl) is $(b,tight) for declarations and $(b,loose) \
           for instantiations." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with field_space= x})
      (fun conf -> conf.field_space)

  let function_indent =
    let docv = "COLS" in
    let doc = "Indentation of function cases ($(docv) columns)." in
    let names = ["function-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~section
      (fun conf x -> {conf with function_indent= x})
      (fun conf -> conf.function_indent)

  let function_indent_nested =
    let doc =
      "Whether the $(b,function-indent) parameter should be applied even \
       when in a sub-block."
    in
    let names = ["function-indent-nested"] in
    let all =
      [ ( "never"
        , `Never
        , "$(b,never) only applies $(b,function-indent) if the function \
           block starts a line." )
      ; ("always", `Always, "$(b,always) always apply $(b,function-indent).")
      ; ( "auto"
        , `Auto
        , "$(b,auto) applies $(b,function-indent) when seen fit." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with function_indent_nested= x})
      (fun conf -> conf.function_indent_nested)

  let if_then_else =
    let doc = "If-then-else formatting." in
    let names = ["if-then-else"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) tries to format an if-then-else expression on a \
           single line." )
      ; ( "fit-or-vertical"
        , `Fit_or_vertical
        , "$(b,fit-or-vertical) vertically breaks branches if they do not \
           fit on a single line." )
      ; ( "keyword-first"
        , `Keyword_first
        , "$(b,keyword-first) formats if-then-else expressions such that \
           the if-then-else keywords are the first on the line." )
      ; ( "k-r"
        , `K_R
        , "$(b,k-r) formats if-then-else expressions with parentheses that \
           match the K&R style." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with if_then_else= x})
      (fun conf -> conf.if_then_else)

  let indent_after_in =
    let docv = "COLS" in
    let doc =
      "Indentation ($(docv) columns) after `let ... in`, unless followed by \
       another `let`."
    in
    let names = ["indent-after-in"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with indent_after_in= x})
      (fun conf -> conf.indent_after_in)

  let indicate_multiline_delimiters =
    let doc =
      "How to indicate that two matching delimiters live on different lines."
    in
    let names = ["indicate-multiline-delimiters"] in
    let all =
      [ ( "no"
        , `No
        , "$(b, no) doesn't do anything special to indicate the closing \
           delimiter." )
      ; ( "space"
        , `Space
        , "$(b,space) prints a space inside the delimiter to indicate the \
           matching one is on a different line." )
      ; ( "closing-on-separate-line"
        , `Closing_on_separate_line
        , "$(b, closing-on-separate-line) makes sure that the closing \
           delimiter is on its own line." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with indicate_multiline_delimiters= x})
      (fun conf -> conf.indicate_multiline_delimiters)

  let indicate_nested_or_patterns =
    let doc =
      "Control whether or not to indicate nested or-pattern using \
       indentation."
    in
    let names = ["indicate-nested-or-patterns"] in
    let all =
      [ ( "unsafe-no"
        , `Unsafe_no
        , "$(b,unsafe-no) does not indicate nested or-patterns. Warning: \
           this can produce confusing code where a short body of a match \
           case is visually hidden by surrounding long patterns, leading to \
           misassociation between patterns and body expressions." )
      ; ( "space"
        , `Space
        , "$(b,space) starts lines of nested or-patterns with \" |\" rather \
           than \"| \"." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with indicate_nested_or_patterns= x})
      (fun conf -> conf.indicate_nested_or_patterns)

  let infix_precedence =
    let doc =
      "Use indentation or also discretionary parentheses to explicitly \
       disambiguate precedences of infix operators."
    in
    let names = ["infix-precedence"] in
    let all =
      [ ( "indent"
        , `Indent
        , "$(b,indent) uses indentation to explicitly disambiguate \
           precedences of infix operators." )
      ; ( "parens"
        , `Parens
        , "$(b,parens) uses parentheses to explicitly disambiguate \
           precedences of infix operators." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with infix_precedence= x})
      (fun conf -> conf.infix_precedence)

  let leading_nested_match_parens =
    let doc = "Nested match parens formatting." in
    let names = ["leading-nested-match-parens"] in
    C.flag ~default:false ~names ~doc ~section ~allow_inline:false
      (fun conf x -> {conf with leading_nested_match_parens= x})
      (fun conf -> conf.leading_nested_match_parens)

  let let_and =
    let doc = "Style of let_and." in
    let names = ["let-and"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format `let p = e and p = e` in a \
           single line." )
      ; ("sparse", `Sparse, "$(b,sparse) will always break between them.") ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with let_and= x})
      (fun conf -> conf.let_and)

  let let_binding_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of let binding expressions ($(docv) columns) if they do \
       not fit on a single line."
    in
    let names = ["let-binding-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with let_binding_indent= x})
      (fun conf -> conf.let_binding_indent)

  let let_binding_spacing =
    let doc = "Spacing between let binding." in
    let names = ["let-binding-spacing"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) spacing separates adjacent let bindings in a module \
           according to module-item-spacing." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) places two open lines between a multi-line \
           module-level let binding and the next." )
      ; ( "double-semicolon"
        , `Double_semicolon
        , "$(b,double-semicolon) places double semicolons and an open line \
           between a multi-line module-level let binding and the next." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with let_binding_spacing= x})
      (fun conf -> conf.let_binding_spacing)

  let let_module =
    let doc = "Module binding formatting." in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) does not break a line after the $(i,let module ... \
           =) and before the $(i,in) if the module declaration does not fit \
           on a single line." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) breaks a line after $(i,let module ... =) and before \
           the $(i,in) if the module declaration does not fit on a single \
           line." ) ]
    in
    C.choice ~names:["let-module"] ~all ~doc ~section
      (fun conf x -> {conf with let_module= x})
      (fun conf -> conf.let_module)

  let ( (* let_open *) ) =
    let names = ["let-open"] in
    let version = "0.17.0" in
    let msg = concrete_syntax_preserved_msg in
    C.removed_option ~names ~version ~msg

  let margin =
    let docv = "COLS" in
    let doc = "Format code to fit within $(docv) columns." in
    C.any Arg.int ~names:["m"; "margin"] ~default:80 ~doc ~docv ~section
      ~allow_inline:false
      (fun conf x -> {conf with margin= x})
      (fun conf -> conf.margin)

  let match_indent =
    let docv = "COLS" in
    let doc = "Indentation of match/try cases ($(docv) columns)." in
    let names = ["match-indent"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~section
      (fun conf x -> {conf with match_indent= x})
      (fun conf -> conf.match_indent)

  let match_indent_nested =
    let doc =
      "Whether the $(b,match-indent) parameter should be applied even when \
       in a sub-block."
    in
    let names = ["match-indent-nested"] in
    let all =
      [ ( "never"
        , `Never
        , "$(b,never) only applies $(b,match-indent) if the match block \
           starts a line." )
      ; ("always", `Always, "$(b,always) always apply $(b,match-indent).")
      ; ("auto", `Auto, "$(b,auto) applies $(b,match-indent) when seen fit.")
      ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with match_indent_nested= x})
      (fun conf -> conf.match_indent_nested)

  let default_max_indent =
    (* Creating a fresh formatter in case the value of max-indent has been
       changed for stdout. *)
    let fs = Format.formatter_of_buffer (Buffer.create 0) in
    Int.to_string (Format.pp_get_max_indent fs ())

  let max_indent =
    let docv = "COLS" in
    let doc =
      "Maximum offset ($(docv) columns) added to a new line in addition to \
       the offset of the previous line."
    in
    C.any
      Arg.(some ~none:default_max_indent int)
      ~names:["max-indent"] ~doc ~docv ~section ~default:None
      ~allow_inline:false
      (fun conf x -> {conf with max_indent= x})
      (fun conf -> conf.max_indent)

  let module_item_spacing =
    let doc = "Spacing between items of structures and signatures." in
    let names = ["module-item-spacing"] in
    let all =
      [ ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break a line between two items." )
      ; ( "preserve"
        , `Preserve
        , "$(b,preserve) will not leave open lines between one-liners of \
           similar sorts unless there is an open line in the input." )
      ; ( "compact"
        , `Compact
        , "$(b,compact) will not leave open lines between one-liners of \
           similar sorts." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with module_item_spacing= x})
      (fun conf -> conf.module_item_spacing)

  let nested_match =
    let doc =
      "Style of a pattern-matching nested in the last case of another \
       pattern-matching."
    in
    let names = ["nested-match"] in
    let all =
      [ ( "wrap"
        , `Wrap
        , "$(b,wrap) wraps the nested pattern-matching with parentheses and \
           adds indentation." )
      ; ( "align"
        , `Align
        , "$(b,align) vertically aligns the nested pattern-matching under \
           the encompassing pattern-matching." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with nested_match= x})
      (fun conf -> conf.nested_match)

  let ocp_indent_compat =
    let doc =
      "Attempt to generate output which does not change (much) when \
       post-processing with ocp-indent."
    in
    let names = ["ocp-indent-compat"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with ocp_indent_compat= x})
      (fun conf -> conf.ocp_indent_compat)

  let parens_ite =
    let doc =
      "Uses parentheses around if-then-else branches that spread across \
       multiple lines."
    in
    let names = ["parens-ite"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with parens_ite= x})
      (fun conf -> conf.parens_ite)

  let parens_tuple =
    let doc = "Parens tuple expressions." in
    let names = ["parens-tuple"] in
    let all =
      [ ( "always"
        , `Always
        , "$(b,always) always uses parentheses around tuples." )
      ; ( "multi-line-only"
        , `Multi_line_only
        , "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuples." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with parens_tuple= x})
      (fun conf -> conf.parens_tuple)

  let parens_tuple_patterns =
    let doc = "Parens tuple patterns." in
    let names = ["parens-tuple-patterns"] in
    let all =
      [ ( "multi-line-only"
        , `Multi_line_only
        , "$(b,multi-line-only) mode will try to skip parens for \
           single-line tuple patterns." )
      ; ( "always"
        , `Always
        , "$(b,always) always uses parentheses around tuples patterns." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with parens_tuple_patterns= x})
      (fun conf -> conf.parens_tuple_patterns)

  let parse_docstrings =
    let doc = "Parse and format docstrings." in
    let names = ["parse-docstrings"] in
    C.flag ~default:false ~names ~doc ~section
      (fun conf x -> {conf with parse_docstrings= x})
      (fun conf -> conf.parse_docstrings)

  let sequence_blank_line =
    let doc = "Blank line between expressions of a sequence." in
    let names = ["sequence-blank-line"] in
    let all =
      [ ( "preserve-one"
        , `Preserve_one
        , "$(b,preserve) will keep a blank line between two expressions of \
           a sequence if the input contains at least one." )
      ; ( "compact"
        , `Compact
        , "$(b,compact) will not keep any blank line between expressions of \
           a sequence." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with sequence_blank_line= x})
      (fun conf -> conf.sequence_blank_line)

  let sequence_style =
    let doc = "Style of sequence." in
    let names = ["sequence-style"] in
    let all =
      [ ( "terminator"
        , `Terminator
        , "$(b,terminator) only puts spaces after semicolons." )
      ; ( "separator"
        , `Separator
        , "$(b,separator) puts spaces before and after semicolons." )
      ; ( "before"
        , `Before
        , "$(b,before) breaks the sequence before semicolons." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with sequence_style= x})
      (fun conf -> conf.sequence_style)

  let single_case =
    let doc =
      "Style of pattern matching expressions with only a single case."
    in
    let names = ["single-case"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format a single case on a single line."
        )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break the line before a single case." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with single_case= x})
      (fun conf -> conf.single_case)

  let space_around_arrays =
    let doc = "Add a space inside the delimiters of arrays." in
    let names = ["space-around-arrays"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with space_around_arrays= x})
      (fun conf -> conf.space_around_arrays)

  let space_around_lists =
    let doc = "Add a space inside the delimiters of lists." in
    let names = ["space-around-lists"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with space_around_lists= x})
      (fun conf -> conf.space_around_lists)

  let space_around_records =
    let doc = "Add a space inside the delimiters of records." in
    let names = ["space-around-records"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with space_around_records= x})
      (fun conf -> conf.space_around_records)

  let space_around_variants =
    let doc = "Add a space inside the delimiters of variants." in
    let names = ["space-around-variants"] in
    C.flag ~default:true ~names ~doc ~section
      (fun conf x -> {conf with space_around_variants= x})
      (fun conf -> conf.space_around_variants)

  let stritem_extension_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of structure items inside extension nodes ($(docv) \
       columns)."
    in
    let names = ["stritem-extension-indent"] in
    C.any Arg.int ~names ~default:0 ~doc ~docv ~section
      (fun conf x -> {conf with stritem_extension_indent= x})
      (fun conf -> conf.stritem_extension_indent)

  let type_decl =
    let doc = "Style of type declaration." in
    let names = ["type-decl"] in
    let all =
      [ ( "compact"
        , `Compact
        , "$(b,compact) will try to format constructors and records \
           definition in a single line." )
      ; ( "sparse"
        , `Sparse
        , "$(b,sparse) will always break between constructors and record \
           fields." ) ]
    in
    C.choice ~names ~all ~doc ~section
      (fun conf x -> {conf with type_decl= x})
      (fun conf -> conf.type_decl)

  let type_decl_indent =
    let docv = "COLS" in
    let doc =
      "Indentation of type declarations ($(docv) columns) if they do not \
       fit on a single line."
    in
    let names = ["type-decl-indent"] in
    C.any Arg.int ~names ~default:2 ~doc ~docv ~section ~allow_inline:false
      (fun conf x -> {conf with type_decl_indent= x})
      (fun conf -> conf.type_decl_indent)

  let wrap_comments =
    let doc =
      "Wrap comments and docstrings. Comments and docstrings are divided \
       into paragraphs by open lines (two or more consecutive newlines), \
       and each paragraph is wrapped at the margin. Multi-line comments \
       with vertically-aligned asterisks on the left margin are not \
       wrapped. Consecutive comments with both left and right margin \
       aligned are not wrapped either."
    in
    C.flag ~default:false ~names:["wrap-comments"] ~doc ~section
      (fun conf x -> {conf with wrap_comments= x})
      (fun conf -> conf.wrap_comments)

  let wrap_fun_args =
    let default = true in
    let doc = "Style for function call." in
    let names = ["wrap-fun-args"] in
    C.flag ~default ~names ~doc ~section
      (fun conf wrap_fun_args -> {conf with wrap_fun_args})
      (fun conf -> conf.wrap_fun_args)
end

(* Flags that can be modified in the config file that don't affect formatting *)

let project_root_witness = [".git"; ".hg"; "dune-project"]

let section = `Operational

let docs = C.section_name section

let comment_check =
  let default = true in
  let doc =
    "Control whether to check comments and documentation comments. Unsafe \
     to turn off. May be set in $(b,.ocamlformat)."
  in
  C.flag ~default ~names:["comment-check"] ~doc ~section
    (fun conf x -> {conf with comment_check= x})
    (fun conf -> conf.comment_check)

let disable_conf_attrs =
  let doc = "Disable configuration in attributes." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-attrs"] ~doc ~docs)

let disable_conf_files =
  let doc = "Disable .ocamlformat configuration files." in
  mk ~default:false
    Arg.(value & flag & info ["disable-conf-files"] ~doc ~docs)

let disable_outside_detected_project =
  let doc =
    Format.sprintf
      "$(b,Warning:) this option is $(b,deprecated) and will be removed in \
       OCamlFormat v1.0."
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["disable-outside-detected-project"] ~doc ~docs)

let enable_outside_detected_project =
  let witness =
    String.concat ~sep:" or "
      (List.map project_root_witness ~f:(fun name ->
           Format.sprintf "$(b,%s)" name ) )
  in
  let doc =
    Format.sprintf
      "Read $(b,.ocamlformat) config files outside the current project. The \
       project root of an input file is taken to be the nearest ancestor \
       directory that contains a %s file. Formatting is enabled even if no \
       $(b,.ocamlformat) configuration file is found."
      witness
  in
  let default = false in
  mk ~default
    Arg.(value & flag & info ["enable-outside-detected-project"] ~doc ~docs)

let max_iters =
  let docv = "N" in
  let doc =
    "Fail if output of formatting does not stabilize within $(docv) \
     iterations. May be set in $(b,.ocamlformat)."
  in
  C.any Arg.int ~names:["n"; "max-iters"] ~default:10 ~doc ~docv ~section
    (fun conf x -> {conf with max_iters= x})
    (fun conf -> conf.max_iters)

let quiet =
  let doc = "Quiet. May be set in $(b,.ocamlformat)." in
  C.flag ~default:false ~names:["q"; "quiet"] ~doc ~section
    (fun conf x -> {conf with quiet= x})
    (fun conf -> conf.quiet)

(* Other Flags *)

let check =
  let doc =
    "Check whether the input files already are formatted. Mutually \
     exclusive with --inplace and --output."
  in
  mk ~default:false Arg.(value & flag & info ["check"] ~doc ~docs)

let config =
  let doc =
    "Aggregate options. Options are specified as a comma-separated list of \
     pairs: \
     $(i,option)$(b,=)$(i,VAL)$(b,,)...$(b,,)$(i,option)$(b,=)$(i,VAL)."
  in
  let env = Arg.env_var "OCAMLFORMAT" in
  let default = [] in
  let assoc = Arg.(pair ~sep:'=' string string) in
  let list_assoc = Arg.(list ~sep:',' assoc) in
  mk ~default
    Arg.(
      value & opt list_assoc default & info ["c"; "config"] ~doc ~docs ~env)

let debug =
  let doc = "Generate debugging output." in
  let default = false in
  mk ~default Arg.(value & flag & info ["g"; "debug"] ~doc ~docs)

let inplace =
  let doc = "Format in-place, overwriting input file(s)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["i"; "inplace"] ~doc ~docs)

type file = Stdin | File of string

let inputs =
  let docv = "SRC" in
  let file_or_dash =
    let parse, print = Arg.non_dir_file in
    let print fmt = function
      | Stdin -> print fmt "<standard input>"
      | File x -> print fmt x
    in
    let parse = function
      | "-" -> `Ok Stdin
      | s -> (
        match parse s with `Ok x -> `Ok (File x) | `Error x -> `Error x )
    in
    (parse, print)
  in
  let doc =
    "Input files. At least one is required, and exactly one without \
     $(b,--inplace). If $(b,-) is passed, will read from stdin."
  in
  let default = [] in
  mk ~default
    Arg.(value & pos_all file_or_dash default & info [] ~doc ~docv ~docs)

type kind = Kind : _ list Migrate_ast.Traverse.fragment -> kind

let kind : kind option ref =
  let doc = "Parse file with unrecognized extension as an implementation." in
  let impl = (Some (Kind Use_file), Arg.info ["impl"] ~doc ~docs) in
  let doc = "Parse file with unrecognized extension as an interface." in
  let intf = (Some (Kind Signature), Arg.info ["intf"] ~doc ~docs) in
  let doc = "Deprecated. Same as $(b,impl)." in
  let use_file = (Some (Kind Use_file), Arg.info ["use-file"] ~doc ~docs) in
  let default = None in
  mk ~default Arg.(value & vflag default [impl; intf; use_file])

let margin_check =
  let doc = "Emit a warning if the formatted output exceeds the margin." in
  mk ~default:false Arg.(value & flag & info ["margin-check"] ~doc ~docs)

let name =
  let docv = "NAME" in
  let doc =
    "Name of input file for use in error reporting and starting point when \
     searching for '.ocamlformat' files. Defaults to the input file name. \
     Some options can be specified in configuration files named \
     '.ocamlformat' in the same or a parent directory of $(docv), see \
     documentation of other options for details."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some string) default & info ["name"] ~doc ~docs ~docv)

let ocp_indent_options =
  let unsupported ocp_indent = (ocp_indent, ([], "")) in
  let alias ocp_indent ocamlformat =
    ( ocp_indent
    , ( [ocamlformat]
      , Printf.sprintf "$(b,%s) is an alias for $(b,%s)." ocp_indent
          ocamlformat ) )
  in
  let multi_alias ocp_indent l_ocamlformat =
    ( ocp_indent
    , ( l_ocamlformat
      , Format.asprintf "$(b,%s) sets %a." ocp_indent
          (Format.pp_print_list
             ~pp_sep:(fun fs () -> Format.fprintf fs " and ")
             (fun fs x -> Format.fprintf fs "$(b,%s)" x) )
          l_ocamlformat ) )
  in
  [ alias "base" "let-binding-indent"
  ; alias "type" "type-decl-indent"
  ; alias "in" "indent-after-in"
  ; multi_alias "with" ["function-indent"; "match-indent"]
  ; alias "match_clause" "cases-exp-indent"
  ; alias "ppx_stritem_ext" "stritem-extension-indent"
  ; alias "max_indent" "max-indent"
  ; multi_alias "strict_with"
      ["function-indent-nested"; "match-indent-nested"]
  ; unsupported "strict_else"
  ; unsupported "strict_comments"
  ; unsupported "align_ops"
  ; unsupported "align_params" ]

let ocp_indent_config =
  let doc =
    let open Format in
    let supported =
      let only_doc (_, (_, doc)) =
        Option.some_if (not (String.is_empty doc)) doc
      in
      let l = List.filter_map ocp_indent_options ~f:only_doc in
      if List.is_empty l then ""
      else
        asprintf " %a"
          (pp_print_list
             ~pp_sep:(fun fs () -> fprintf fs "@ ")
             (fun fs s -> fprintf fs "%s" s) )
          l
    in
    asprintf "Read .ocp-indent configuration files.%s" supported
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["ocp-indent-config"] ~doc ~docs)

let output =
  let docv = "DST" in
  let doc =
    "Output file. Mutually exclusive with --inplace. Write to stdout if \
     omitted."
  in
  let default = None in
  mk ~default
    Arg.(
      value
      & opt (some string) default
      & info ["o"; "output"] ~doc ~docs ~docv)

let format_invalid_files : [`Never | `Auto] option ref =
  let doc =
    "How invalid (unparsable) files are formatted. $(b,never) doesn't \
     format invalid files and the parsing error will be printed. $(b,auto) \
     will print invalid parts of the input as verbatim text. The default \
     value is $(b,never). This option is experimental."
  in
  let docv = "{never|auto}" in
  let never = ("never", `Never) in
  let auto = ("auto", `Auto) in
  let default = Some `Never in
  mk ~default
    Arg.(
      value
      & opt (some (enum [never; auto])) None
      & info ["format-invalid-files"] ~doc ~docs ~docv)

let print_config =
  let doc =
    "Print the configuration determined by the environment variable, the \
     configuration files, preset profiles and command line. Attributes are \
     not considered. If many input files are specified, only print the \
     configuration for the first file. If no input file is specified, print \
     the configuration for the root directory if specified, or for the \
     current working directory otherwise."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["print-config"] ~doc ~docs)

let root =
  let docv = "DIR" in
  let doc =
    "Root of the project. If specified, only take into account .ocamlformat \
     configuration files inside $(docv) and its subdirectories."
  in
  let default = None in
  mk ~default
    Arg.(value & opt (some dir) default & info ["root"] ~doc ~docs ~docv)

let no_version_check =
  let doc =
    "Do not check that the version matches the one specified in \
     .ocamlformat."
  in
  let default = false in
  mk ~default Arg.(value & flag & info ["no-version-check"] ~doc ~docs)

let ignore_invalid_options =
  let doc = "Ignore invalid options (e.g. in .ocamlformat)." in
  let default = false in
  mk ~default Arg.(value & flag & info ["ignore-invalid-option"] ~doc ~docs)

let ocamlformat_profile =
  { align_cases= false
  ; align_constructors_decl= false
  ; align_variants_decl= false
  ; assignment_operator= `End_line
  ; break_before_in= `Fit_or_vertical
  ; break_cases= `Nested
  ; break_collection_expressions= `Fit_or_vertical
  ; break_infix= `Wrap
  ; break_infix_before_func= true
  ; break_fun_decl= `Wrap
  ; break_fun_sig= `Wrap
  ; break_separators= `Before
  ; break_sequences= false
  ; break_string_literals= `Auto
  ; break_struct= true
  ; cases_exp_indent= 4
  ; cases_matching_exp_indent= `Compact
  ; comment_check= true
  ; disable= false
  ; disambiguate_non_breaking_match= false
  ; doc_comments= `Before_except_val
  ; doc_comments_padding= 2
  ; doc_comments_tag_only= `Default
  ; dock_collection_brackets= false
  ; exp_grouping= `Parens
  ; extension_indent= 2
  ; field_space= `Tight
  ; function_indent= 2
  ; function_indent_nested= `Never
  ; if_then_else= `Compact
  ; indent_after_in= 0
  ; indicate_multiline_delimiters= `Space
  ; indicate_nested_or_patterns= `Space
  ; infix_precedence= `Indent
  ; leading_nested_match_parens= false
  ; let_and= `Compact
  ; let_binding_indent= 2
  ; let_binding_spacing= `Compact
  ; let_module= `Compact
  ; margin= 80
  ; match_indent= 0
  ; match_indent_nested= `Never
  ; max_indent= None
  ; max_iters= 10
  ; module_item_spacing= `Sparse
  ; nested_match= `Wrap
  ; ocp_indent_compat= false
  ; parens_ite= false
  ; parens_tuple= `Always
  ; parens_tuple_patterns= `Multi_line_only
  ; parse_docstrings= false
  ; quiet= false
  ; sequence_blank_line= `Compact
  ; sequence_style= `Separator
  ; single_case= `Compact
  ; space_around_arrays= false
  ; space_around_lists= false
  ; space_around_records= false
  ; space_around_variants= false
  ; stritem_extension_indent= 0
  ; type_decl= `Compact
  ; type_decl_indent= 2
  ; wrap_comments= false
  ; wrap_fun_args= true }

let conventional_profile =
  { align_cases= C.default Formatting.align_cases
  ; align_constructors_decl= C.default Formatting.align_constructors_decl
  ; align_variants_decl= C.default Formatting.align_variants_decl
  ; assignment_operator= C.default Formatting.assignment_operator
  ; break_before_in= C.default Formatting.break_before_in
  ; break_cases= C.default Formatting.break_cases
  ; break_collection_expressions=
      C.default Formatting.break_collection_expressions
  ; break_infix= C.default Formatting.break_infix
  ; break_infix_before_func= C.default Formatting.break_infix_before_func
  ; break_fun_decl= C.default Formatting.break_fun_decl
  ; break_fun_sig= C.default Formatting.break_fun_sig
  ; break_separators= C.default Formatting.break_separators
  ; break_sequences= C.default Formatting.break_sequences
  ; break_string_literals= C.default Formatting.break_string_literals
  ; break_struct= Poly.(C.default Formatting.break_struct = `Force)
  ; cases_exp_indent= C.default Formatting.cases_exp_indent
  ; cases_matching_exp_indent= C.default Formatting.cases_matching_exp_indent
  ; comment_check= C.default comment_check
  ; disable= C.default Formatting.disable
  ; disambiguate_non_breaking_match=
      C.default Formatting.disambiguate_non_breaking_match
  ; doc_comments= C.default Formatting.doc_comments
  ; doc_comments_padding= C.default Formatting.doc_comments_padding
  ; doc_comments_tag_only= C.default Formatting.doc_comments_tag_only
  ; dock_collection_brackets= C.default Formatting.dock_collection_brackets
  ; exp_grouping= C.default Formatting.exp_grouping
  ; extension_indent= C.default Formatting.extension_indent
  ; field_space= C.default Formatting.field_space
  ; function_indent= C.default Formatting.function_indent
  ; function_indent_nested= C.default Formatting.function_indent_nested
  ; if_then_else= C.default Formatting.if_then_else
  ; indent_after_in= C.default Formatting.indent_after_in
  ; indicate_multiline_delimiters=
      C.default Formatting.indicate_multiline_delimiters
  ; indicate_nested_or_patterns=
      C.default Formatting.indicate_nested_or_patterns
  ; infix_precedence= C.default Formatting.infix_precedence
  ; leading_nested_match_parens=
      C.default Formatting.leading_nested_match_parens
  ; let_and= C.default Formatting.let_and
  ; let_binding_indent= C.default Formatting.let_binding_indent
  ; let_binding_spacing= C.default Formatting.let_binding_spacing
  ; let_module= C.default Formatting.let_module
  ; margin= C.default Formatting.margin
  ; match_indent= C.default Formatting.match_indent
  ; match_indent_nested= C.default Formatting.match_indent_nested
  ; max_indent= C.default Formatting.max_indent
  ; max_iters= C.default max_iters
  ; module_item_spacing= C.default Formatting.module_item_spacing
  ; nested_match= C.default Formatting.nested_match
  ; ocp_indent_compat= C.default Formatting.ocp_indent_compat
  ; parens_ite= C.default Formatting.parens_ite
  ; parens_tuple= C.default Formatting.parens_tuple
  ; parens_tuple_patterns= C.default Formatting.parens_tuple_patterns
  ; parse_docstrings= C.default Formatting.parse_docstrings
  ; quiet= C.default quiet
  ; sequence_blank_line= C.default Formatting.sequence_blank_line
  ; sequence_style= C.default Formatting.sequence_style
  ; single_case= C.default Formatting.single_case
  ; space_around_arrays= C.default Formatting.space_around_arrays
  ; space_around_lists= C.default Formatting.space_around_lists
  ; space_around_records= C.default Formatting.space_around_records
  ; space_around_variants= C.default Formatting.space_around_variants
  ; stritem_extension_indent= C.default Formatting.stritem_extension_indent
  ; type_decl= C.default Formatting.type_decl
  ; type_decl_indent= C.default Formatting.type_decl_indent
  ; wrap_comments= C.default Formatting.wrap_comments
  ; wrap_fun_args= C.default Formatting.wrap_fun_args }

let compact_profile =
  { ocamlformat_profile with
    break_before_in= `Auto
  ; break_cases= `Fit
  ; break_collection_expressions= `Wrap
  ; break_infix= `Wrap
  ; break_fun_decl= `Wrap
  ; break_fun_sig= `Wrap
  ; break_sequences= false
  ; break_struct= false
  ; doc_comments_tag_only= `Fit
  ; exp_grouping= `Parens
  ; field_space= `Tight
  ; if_then_else= `Compact
  ; indicate_nested_or_patterns= `Space
  ; leading_nested_match_parens= false
  ; let_and= `Compact
  ; let_binding_spacing= `Compact
  ; let_module= `Compact
  ; module_item_spacing= `Compact
  ; single_case= `Compact
  ; space_around_arrays= false
  ; space_around_lists= false
  ; space_around_records= false
  ; space_around_variants= false
  ; type_decl= `Compact
  ; wrap_fun_args= true }

let sparse_profile =
  { ocamlformat_profile with
    break_before_in= `Fit_or_vertical
  ; break_cases= `Nested
  ; break_collection_expressions= `Fit_or_vertical
  ; break_infix= `Fit_or_vertical
  ; break_fun_decl= `Smart
  ; break_fun_sig= `Smart
  ; break_sequences= true
  ; break_struct= true
  ; field_space= `Loose
  ; if_then_else= `Keyword_first
  ; indicate_nested_or_patterns= `Space
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_binding_spacing= `Sparse
  ; let_module= `Sparse
  ; module_item_spacing= `Sparse
  ; single_case= `Sparse
  ; sequence_blank_line= `Preserve_one
  ; space_around_arrays= true
  ; space_around_lists= true
  ; space_around_records= true
  ; space_around_variants= true
  ; type_decl= `Sparse
  ; wrap_fun_args= false }

let janestreet_profile =
  { align_constructors_decl= false
  ; align_cases= false
  ; align_variants_decl= false
  ; assignment_operator= `Begin_line
  ; break_before_in= `Fit_or_vertical
  ; break_cases= `Fit_or_vertical
  ; break_collection_expressions=
      ocamlformat_profile.break_collection_expressions
  ; break_infix= `Fit_or_vertical
  ; break_infix_before_func= true
  ; break_fun_decl= `Fit_or_vertical
  ; break_fun_sig= `Fit_or_vertical
  ; break_separators= `Before
  ; break_sequences= true
  ; break_string_literals= `Auto
  ; break_struct= ocamlformat_profile.break_struct
  ; cases_exp_indent= 2
  ; cases_matching_exp_indent= `Normal
  ; comment_check= true
  ; disable= false
  ; disambiguate_non_breaking_match= false
  ; doc_comments= `Before
  ; doc_comments_padding= 1
  ; doc_comments_tag_only= `Fit
  ; dock_collection_brackets= false
  ; exp_grouping= `Parens
  ; extension_indent= 2
  ; field_space= `Loose
  ; function_indent= 2
  ; function_indent_nested= `Never
  ; if_then_else= `Keyword_first
  ; indent_after_in= 0
  ; indicate_multiline_delimiters= `No
  ; indicate_nested_or_patterns= `Unsafe_no
  ; infix_precedence= `Parens
  ; leading_nested_match_parens= true
  ; let_and= `Sparse
  ; let_binding_indent= 2
  ; let_binding_spacing= `Double_semicolon
  ; let_module= `Sparse
  ; margin= 90
  ; match_indent= 0
  ; match_indent_nested= `Never
  ; max_indent= None
  ; max_iters= ocamlformat_profile.max_iters
  ; module_item_spacing= `Compact
  ; nested_match= `Wrap
  ; ocp_indent_compat= true
  ; parens_ite= true
  ; parens_tuple= `Multi_line_only
  ; parens_tuple_patterns= `Multi_line_only
  ; parse_docstrings= false
  ; quiet= ocamlformat_profile.quiet
  ; sequence_blank_line= `Compact
  ; sequence_style= `Terminator
  ; single_case= `Sparse
  ; space_around_arrays= true
  ; space_around_lists= true
  ; space_around_records= true
  ; space_around_variants= true
  ; stritem_extension_indent= 0
  ; type_decl= `Sparse
  ; type_decl_indent= 2
  ; wrap_comments= false
  ; wrap_fun_args= false }

let selected_profile_ref = ref (Some conventional_profile)

let (_profile : t option C.t) =
  let doc =
    "Select a preset profile which sets $(i,all) options, overriding lower \
     priority configuration."
  in
  let names = profile_option_names in
  let all =
    [ ( "conventional"
      , Some conventional_profile
      , "The $(b,conventional) profile aims to be as familiar and \
         \"conventional\" appearing as the available options allow." )
    ; ( "default"
      , Some conventional_profile
      , "$(b,default) is an alias for the $(b,conventional) profile." )
    ; ( "compact"
      , Some compact_profile
      , "The $(b,compact) profile is similar to $(b,ocamlformat) but opts \
         for a generally more compact code style." )
    ; ( "sparse"
      , Some sparse_profile
      , "The $(b,sparse) profile is similar to $(b,ocamlformat) but opts \
         for a generally more sparse code style." )
    ; ( "ocamlformat"
      , Some ocamlformat_profile
      , "The $(b,ocamlformat) profile aims to take advantage of the \
         strengths of a parsetree-based auto-formatter, and to limit the \
         consequences of the weaknesses imposed by the current \
         implementation. This is a style which optimizes for what the \
         formatter can do best, rather than to match the style of any \
         existing code. General guidelines that have directed the design \
         include: Legibility, in the sense of making it as hard as possible \
         for quick visual parsing to give the wrong interpretation, is of \
         highest priority; Whenever possible the high-level structure of \
         the code should be obvious by looking only at the left margin, in \
         particular, it should not be necessary to visually jump from left \
         to right hunting for critical keywords, tokens, etc; All else \
         equal compact code is preferred as reading without scrolling is \
         easier, so indentation or white space is avoided unless it helps \
         legibility; Attention has been given to making some syntactic \
         gotchas visually obvious." )
    ; ( "janestreet"
      , Some janestreet_profile
      , "The $(b,janestreet) profile is used at Jane Street." ) ]
  in
  C.choice ~names ~all ~doc ~section
    (fun conf p ->
      selected_profile_ref := p ;
      let new_conf = Option.value p ~default:conf in
      (* The quiet option is cummulative *)
      {new_conf with quiet= new_conf.quiet || conf.quiet} )
    (fun _ -> !selected_profile_ref)

let ocp_indent_normal_profile =
  [ ("base", "2")
  ; ("type", "2")
  ; ("in", "0")
  ; ("with", "0")
  ; ("match_clause", "2")
  ; ("ppx_stritem_ext", "2")
  ; ("max_indent", "4")
  ; ("strict_with", "never")
  ; ("strict_else", "always")
  ; ("strict_comments", "false")
  ; ("align_ops", "true")
  ; ("align_params", "auto") ]

let ocp_indent_apprentice_profile =
  [ ("base", "2")
  ; ("type", "4")
  ; ("in", "2")
  ; ("with", "2")
  ; ("match_clause", "4")
  ; ("ppx_stritem_ext", "2")
  ; ("strict_with", "never")
  ; ("strict_else", "always")
  ; ("strict_comments", "false")
  ; ("align_ops", "true")
  ; ("align_params", "always") ]

let ocp_indent_janestreet_profile =
  [ ("base", "2")
  ; ("type", "2")
  ; ("in", "0")
  ; ("with", "0")
  ; ("match_clause", "2")
  ; ("ppx_stritem_ext", "2")
  ; ("max_indent", "2")
  ; ("strict_with", "auto")
  ; ("strict_else", "always")
  ; ("strict_comments", "true")
  ; ("align_ops", "true")
  ; ("align_params", "always") ]

let string_of_user_error = function
  | `Malformed line -> Format.sprintf "Invalid format %S" line
  | `Misplaced (name, _) -> Format.sprintf "%s not allowed here" name
  | `Unknown (name, _) -> Format.sprintf "Unknown option %S" name
  | `Bad_value (name, msg) -> Format.sprintf "For option %S: %s" name msg

let parse_line config ~from s =
  let update ~config ~from ~name ~value =
    let name = String.strip name in
    let value = String.strip value in
    match (name, from) with
    | "version", `File _ ->
        if String.equal Version.version value || !no_version_check then
          Ok config
        else
          Error
            (`Bad_value
              ( name
              , Format.sprintf "expecting %S but got %S" Version.version
                  value ) )
    | name, `File x ->
        C.update ~config ~from:(`Parsed (`File x)) ~name ~value ~inline:false
    | name, `Attribute ->
        if !disable_conf_attrs then (
          warn "Configuration in attribute %S ignored." s ;
          Ok config )
        else
          C.update ~config
            ~from:(`Parsed `Attribute)
            ~name ~value ~inline:true
  in
  let update_ocp_indent_option ~config ~from ~name ~value =
    let equal = String.equal in
    match List.Assoc.find ocp_indent_options ~equal name with
    | None -> Ok config
    | Some (l, _doc) ->
        let update_one config name = update ~config ~from ~name ~value in
        List.fold_result l ~init:config ~f:update_one
  in
  let rec update_many ~config ~from = function
    | [] -> Ok config
    | (name, value) :: t -> (
      match update_ocp_indent_option ~config ~from ~name ~value with
      | Ok c -> update_many ~config:c ~from t
      | Error e -> Error e )
  in
  let s =
    match String.index s '#' with
    | Some i -> String.sub s ~pos:0 ~len:i
    | None -> s
  in
  let s = String.strip s in
  match String.split ~on:'=' s with
  | [] | [""] -> Ok config
  | [name; value] ->
      let name = String.strip name in
      let value = String.strip value in
      if List.Assoc.mem ocp_indent_options ~equal:String.equal name then
        update_ocp_indent_option ~config ~from ~name ~value
      else update ~config ~from ~name ~value
  | [s] -> (
    match String.strip s with
    | "" -> impossible "previous match"
    (* special case for disable/enable *)
    | "enable" -> update ~config ~from ~name:"disable" ~value:"false"
    | "normal" -> update_many ~config ~from ocp_indent_normal_profile
    | "apprentice" -> update_many ~config ~from ocp_indent_apprentice_profile
    | "JaneStreet" ->
        Result.( >>= )
          (update ~config ~from ~name:"profile" ~value:"janestreet")
          (fun config ->
            update_many ~config ~from ocp_indent_janestreet_profile )
    | name -> update ~config ~from ~name ~value:"true" )
  | _ -> Error (`Malformed s)

let is_project_root ~root dir =
  match root with
  | Some root -> Fpath.equal dir root
  | None ->
      List.exists project_root_witness ~f:(fun name ->
          Fpath.(exists (dir / name)) )

let dot_ocp_indent = ".ocp-indent"

let dot_ocamlformat = ".ocamlformat"

let dot_ocamlformat_ignore = ".ocamlformat-ignore"

let dot_ocamlformat_enable = ".ocamlformat-enable"

let rec collect_files ~enable_outside_detected_project ~root ~segs ~ignores
    ~enables ~files =
  match segs with
  | [] | [""] -> (ignores, enables, files, None)
  | "" :: upper_segs ->
      collect_files ~enable_outside_detected_project ~root ~segs:upper_segs
        ~ignores ~enables ~files
  | _ :: upper_segs ->
      let sep = Fpath.dir_sep in
      let dir = String.concat ~sep (List.rev segs) |> Fpath.v in
      let ignores =
        let filename = Fpath.(dir / dot_ocamlformat_ignore) in
        if Fpath.exists filename then filename :: ignores else ignores
      in
      let enables =
        let filename = Fpath.(dir / dot_ocamlformat_enable) in
        if Fpath.exists filename then filename :: enables else enables
      in
      let files =
        let f_1 = Fpath.(dir / dot_ocamlformat) in
        let files =
          if Fpath.exists f_1 then `Ocamlformat f_1 :: files else files
        in
        let f_2 = Fpath.(dir / dot_ocp_indent) in
        if Fpath.exists f_2 then `Ocp_indent f_2 :: files else files
      in
      if is_project_root ~root dir && not enable_outside_detected_project
      then (ignores, enables, files, Some dir)
      else
        collect_files ~enable_outside_detected_project ~root ~segs:upper_segs
          ~ignores ~enables ~files

exception Conf_error of string

let failwith_user_errors ~kind errors =
  let open Format in
  let pp_error pp e = pp_print_string pp (string_of_user_error e) in
  let pp_errors = pp_print_list ~pp_sep:pp_print_newline pp_error in
  let msg = asprintf "Error while parsing %s:@ %a" kind pp_errors errors in
  raise (Conf_error msg)

let read_config_file conf filename_kind =
  match filename_kind with
  | `Ocp_indent _ when not !ocp_indent_config -> conf
  | `Ocp_indent filename | `Ocamlformat filename -> (
    try
      In_channel.with_file (Fpath.to_string filename) ~f:(fun ic ->
          let c, errors, _ =
            In_channel.fold_lines ic ~init:(conf, [], 1)
              ~f:(fun (conf, errors, num) line ->
                let from = `File (filename, num) in
                match parse_line conf ~from line with
                | Ok conf -> (conf, errors, Int.succ num)
                | Error _ when !ignore_invalid_options ->
                    warn ~filename ~lnum:num "ignoring invalid options %S"
                      line ;
                    (conf, errors, Int.succ num)
                | Error e -> (conf, e :: errors, Int.succ num) )
          in
          match List.rev errors with
          | [] -> c
          | l ->
              let kind =
                match filename_kind with
                | `Ocp_indent _ -> dot_ocp_indent
                | `Ocamlformat _ -> dot_ocamlformat
              in
              failwith_user_errors ~kind l )
    with Sys_error _ -> conf )

let update_using_env conf =
  let f (config, errors) (name, value) =
    match C.update ~config ~from:`Env ~name ~value ~inline:false with
    | Ok c -> (c, errors)
    | Error e -> (config, e :: errors)
  in
  let conf, errors = List.fold_left !config ~init:(conf, []) ~f in
  match List.rev errors with
  | [] -> conf
  | l -> failwith_user_errors ~kind:"OCAMLFORMAT environment variable" l

let xdg_config =
  let xdg_config_home =
    match Caml.Sys.getenv_opt "XDG_CONFIG_HOME" with
    | None | Some "" -> (
      match Caml.Sys.getenv_opt "HOME" with
      | None | Some "" -> None
      | Some home -> Some Fpath.(v home / ".config") )
    | Some xdg_config_home -> Some (Fpath.v xdg_config_home)
  in
  match xdg_config_home with
  | Some xdg_config_home ->
      let filename = Fpath.(xdg_config_home / "ocamlformat") in
      if Fpath.exists filename then Some filename else None
  | None -> None

let is_in_listing_file ~listings ~filename =
  let drop_line l = String.is_empty l || String.is_prefix l ~prefix:"#" in
  (* process deeper files first *)
  let listings = List.rev listings in
  List.find_map listings ~f:(fun listing_file ->
      let dir, _ = Fpath.split_base listing_file in
      try
        In_channel.with_file (Fpath.to_string listing_file) ~f:(fun ch ->
            let lines =
              In_channel.input_lines ch
              |> List.mapi ~f:(fun i s -> (i + 1, String.strip s))
              |> List.filter ~f:(fun (_, l) -> not (drop_line l))
            in
            List.find_map lines ~f:(fun (lno, line) ->
                match Fpath.of_string line with
                | Ok file_on_current_line -> (
                    let f = Fpath.(dir // file_on_current_line) in
                    if Fpath.equal filename f then Some (listing_file, lno)
                    else
                      try
                        let filename = Fpath.to_string filename in
                        let re =
                          let pathname = true and anchored = true in
                          let f = Fpath.to_string f in
                          Re.(Glob.glob ~pathname ~anchored f |> compile)
                        in
                        Option.some_if (Re.execp re filename)
                          (listing_file, lno)
                      with Re.Glob.Parse_error ->
                        warn ~filename:listing_file ~lnum:lno
                          "pattern %s cannot be parsed" line ;
                        None )
                | Error (`Msg msg) ->
                    warn ~filename:listing_file ~lnum:lno "%s" msg ;
                    None ) )
      with Sys_error err ->
        warn "ignoring %a, %s" Fpath.pp listing_file err ;
        None )

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  let vfile = Fpath.v file in
  let file_abs = Fpath.(vfile |> to_absolute |> normalize) in
  let dir = Fpath.(file_abs |> split_base |> fst) in
  let segs = Fpath.segs dir |> List.rev in
  let ignores, enables, files, project_root =
    collect_files ~enable_outside_detected_project ~root ~segs ~ignores:[]
      ~enables:[] ~files:[]
  in
  let files =
    match (xdg_config, enable_outside_detected_project) with
    | None, _ | Some _, false -> files
    | Some f, true -> `Ocamlformat f :: files
  in
  let files = if !disable_conf_files then [] else files in
  let conf =
    let init = conventional_profile in
    List.fold files ~init ~f:read_config_file
    |> update_using_env |> C.update_using_cmdline
  in
  let no_ocamlformat_files =
    let f = function `Ocamlformat _ -> false | `Ocp_indent _ -> true in
    List.for_all files ~f
  in
  if
    (not is_stdin) && no_ocamlformat_files
    && not enable_outside_detected_project
  then (
    (let why =
       match project_root with
       | Some root ->
           Format.sprintf
             "no [.ocamlformat] was found within the project (root: %s)"
             (Fpath.to_string ~relativize:true root)
       | None -> "no project root was found"
     in
     warn ~filename:vfile
       "Ocamlformat disabled because [--enable-outside-detected-project] is \
        not set and %s"
       why ) ;
    {conf with disable= true} )
  else
    let listings = if conf.disable then enables else ignores in
    match is_in_listing_file ~listings ~filename:file_abs with
    | Some (file, lno) ->
        let status = if conf.disable then "enabled" else "ignored" in
        if !debug then
          Format.eprintf "File %a: %s in %a:%d@\n" Fpath.pp file_abs status
            Fpath.pp file lno ;
        {conf with disable= not conf.disable}
    | None -> conf

let build_config ~enable_outside_detected_project ~root ~file ~is_stdin =
  let conf, warn_now =
    collect_warnings (fun () ->
        build_config ~enable_outside_detected_project ~root ~file ~is_stdin )
  in
  if not conf.quiet then warn_now () ;
  conf

let kind_of_ext fname =
  match Filename.extension fname with
  | ".ml" | ".mlt" | ".eliom" -> Some (Kind Use_file)
  | ".mli" | ".eliomi" -> Some (Kind Signature)
  | _ -> None

let validate_inputs () =
  match (!inputs, !kind, !name) with
  | [], _, _ -> Ok `No_input
  | [Stdin], None, None ->
      Error
        "Must specify at least one of --name, --impl or --intf when reading \
         from stdin"
  | [Stdin], Some kind, name -> Ok (`Stdin (name, kind))
  | [Stdin], None, Some name -> (
    match kind_of_ext name with
    | Some kind -> Ok (`Stdin (Some name, kind))
    | None ->
        Error
          "Cannot deduce file kind from passed --name. Please specify \
           --impl or --intf" )
  | [File f], Some kind, name -> Ok (`Single_file (kind, name, f))
  | [File f], None, name ->
      let kind =
        Option.value ~default:f name
        |> kind_of_ext
        |> Option.value ~default:(Kind Use_file)
      in
      Ok (`Single_file (kind, name, f))
  | _ :: _ :: _, Some _, _ ->
      Error "Cannot specify --impl or --intf with multiple inputs"
  | _ :: _ :: _, _, Some _ ->
      Error "Cannot specify --name with multiple inputs"
  | (_ :: _ :: _ as inputs), None, None ->
      List.map inputs ~f:(function
        | Stdin -> Error "Cannot specify stdin together with other inputs"
        | File f ->
            let kind =
              Option.value ~default:(Kind Use_file) (kind_of_ext f)
            in
            Ok (kind, f) )
      |> Result.all
      |> Result.map ~f:(fun files -> `Several_files files)

let validate_action () =
  match
    List.filter_map
      ~f:(fun s -> s)
      [ Option.map ~f:(fun o -> (`Output o, "--output")) !output
      ; Option.some_if !inplace (`Inplace, "--inplace")
      ; Option.some_if !check (`Check, "--check")
      ; Option.some_if !print_config (`Print_config, "--print-config") ]
  with
  | [] -> Ok `No_action
  | [(action, _)] -> Ok action
  | (_, a1) :: (_, a2) :: _ ->
      Error (Printf.sprintf "Cannot specify %s with %s" a1 a2)

type input = {kind: kind; name: string; file: file; conf: t}

type action =
  | In_out of input * string option
  | Inplace of input list
  | Check of input list
  | Print_config of t

let make_action ~enable_outside_detected_project ~root action inputs =
  let make_file ?(with_conf = fun c -> c) ?name kind file =
    let name = Option.value ~default:file name in
    let conf =
      with_conf
        (build_config ~enable_outside_detected_project ~root ~file:name
           ~is_stdin:false )
    in
    {kind; name; file= File file; conf}
  in
  let make_stdin ?(name = "<standard input>") kind =
    let conf =
      build_config ~enable_outside_detected_project ~root ~file:name
        ~is_stdin:false
    in
    {kind; name; file= Stdin; conf}
  in
  match (action, inputs) with
  | `Print_config, inputs ->
      let file, is_stdin =
        match inputs with
        | `Stdin _ -> ("-", true)
        | `Single_file (_, _, f) -> (f, false)
        | `Several_files ((_, f) :: _) -> (f, false)
        | `Several_files [] | `No_input ->
            let root = Option.value root ~default:(Fpath.cwd ()) in
            (Fpath.(root / dot_ocamlformat |> to_string), true)
      in
      let conf =
        build_config ~enable_outside_detected_project ~root ~file ~is_stdin
      in
      Ok (Print_config conf)
  | (`No_action | `Output _ | `Inplace | `Check), `No_input ->
      Error "Must specify at least one input file, or `-` for stdin"
  | (`No_action | `Output _), `Several_files _ ->
      Error
        "Must specify exactly one input file without --inplace or --check"
  | `Inplace, `Stdin _ ->
      Error "Cannot specify stdin together with --inplace"
  | `No_action, `Single_file (kind, name, f) ->
      Ok (In_out (make_file ?name kind f, None))
  | `No_action, `Stdin (name, kind) ->
      Ok (In_out (make_stdin ?name kind, None))
  | `Output output, `Single_file (kind, name, f) ->
      Ok (In_out (make_file ?name kind f, Some output))
  | `Output output, `Stdin (name, kind) ->
      Ok (In_out (make_stdin ?name kind, Some output))
  | `Inplace, `Single_file (kind, name, f) ->
      Ok (Inplace [make_file ?name kind f])
  | `Inplace, `Several_files files ->
      Ok (Inplace (List.map files ~f:(fun (kind, f) -> make_file kind f)))
  | `Check, `Single_file (kind, name, f) ->
      Ok (Check [make_file ?name kind f])
  | `Check, `Several_files files ->
      let f (kind, f) =
        make_file ~with_conf:(fun c -> {c with max_iters= 1}) kind f
      in
      Ok (Check (List.map files ~f))
  | `Check, `Stdin (name, kind) -> Ok (Check [make_stdin ?name kind])

type opts = {debug: bool; margin_check: bool; format_invalid_files: bool}

let validate () =
  let root =
    Option.map !root ~f:Fpath.(fun x -> v x |> to_absolute |> normalize)
  in
  let enable_outside_detected_project =
    !enable_outside_detected_project && Option.is_none root
  in
  if !disable_outside_detected_project then
    warn
      "option `--disable-outside-detected-project` is deprecated and will \
       be removed in OCamlFormat v1.0." ;
  match
    let open Result in
    validate_action ()
    >>= fun action ->
    validate_inputs ()
    >>= fun inputs ->
    make_action ~enable_outside_detected_project ~root action inputs
  with
  | exception Conf_error e -> `Error (false, e)
  | Error e -> `Error (false, e)
  | Ok action ->
      let format_invalid_files =
        match !format_invalid_files with Some `Auto -> true | _ -> false
      in
      let opts =
        {debug= !debug; margin_check= !margin_check; format_invalid_files}
      in
      `Ok (action, opts)

let action () = parse info validate

open Migrate_ast.Parsetree

let update ?(quiet = false) c {attr_name= {txt; loc}; attr_payload; _} =
  let result =
    match txt with
    | "ocamlformat" -> (
      match attr_payload with
      | PStr
          [ { pstr_desc=
                Pstr_eval
                  ( { pexp_desc= Pexp_constant (Pconst_string (str, _, None))
                    ; pexp_attributes= []
                    ; _ }
                  , [] )
            ; _ } ] ->
          parse_line ~from:`Attribute c str
          |> Result.map_error ~f:string_of_user_error
      | _ -> Error "Invalid format: String expected" )
    | _ when String.is_prefix ~prefix:"ocamlformat." txt ->
        Error
          (Format.sprintf "Invalid format: Unknown suffix %S"
             (String.chop_prefix_exn ~prefix:"ocamlformat." txt) )
    | _ -> Ok c
  in
  match result with
  | Ok conf -> conf
  | Error error ->
      let w = Warnings.Attribute_payload (txt, error) in
      if (not c.quiet) && not quiet then print_warning loc w ;
      c

let print_config = C.print_config
