open Sexplib.Std

module Error = struct
  type t =
    | End_of_file
    | Expected_expression of Lexer.span
    | Expected_type_expression of Lexer.span
    | Expected_name of Lexer.span
    | Expected_symbol of {
        expected : [ `One_of of Token.t list | `Exact of Token.t | `Unsure ];
        found : Lexer.span;
      }
    | Unexpected_token of Lexer.span
  [@@deriving sexp]

  let pp ppf error =
    let sexp = sexp_of_t error in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

  exception Parse_error of t

  let err e = raise (Parse_error e)

  let expected_symbol ~sym ~found =
    err (Expected_symbol { expected = sym; found })

  let expected_name span = err (Expected_name span)

  let unexpected_token span = expected_symbol ~sym:`Unsure ~found:span

  let expected_expression span = err (Expected_expression span)

  let expected_type_expression span = err (Expected_type_expression span)
end

type t = {
  mutable lexer : Lexer.t;
  mutable last_span : Lexer.span;
  mutable curr_span : Lexer.span;
}
[@@deriving sexp]

(*** Parsing actions ***********************************************************)

let next t =
  match Lexer.scan t.lexer with
  | Ok span ->
      t.last_span <- t.curr_span;
      t.curr_span <- span;
      (* Logs.debug (fun f -> f "token: %a" Token.pp t.curr_span.token);  *)
      ()
  | Error (`Lexer_error err) -> raise (Lexer.Lexer_error err)

let peek t =
  match Lexer.peek t.lexer with
  | Ok span -> span
  | Error (`Lexer_error err) -> raise (Lexer.Lexer_error err)

let expect token t =
  if t.curr_span.token = token then next t
  else Error.expected_symbol ~sym:(`Exact token) ~found:t.curr_span

(*** Combinators ***************************************************************)

let sep_by sep parser t =
  let rec collect nodes =
    match parser t with
    | exception _ -> nodes
    | node ->
        let nodes = node :: nodes in
        if t.curr_span.token = sep then (
          next t;
          collect nodes)
        else nodes
  in
  List.rev (collect [])

(*** Parsers *******************************************************************)

let parse_name t =
  match t.curr_span.token with
  | Token.Id path ->
      next t;
      Some (Parsetree_helper.id path)
  | _ -> None

let parse_visibility t =
  match t.curr_span.token with
  | Token.Pub ->
      next t;
      Parsetree_helper.Visibility.pub
  | _ -> Parsetree_helper.Visibility.priv

(*

*)

let parse_annotations_field t =
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  let value =
    match t.curr_span.token with
    | Token.Equal -> (
        next t;
        match t.curr_span.token with
        | Token.String str ->
            next t;
            Some str
        | _ -> Some "")
    | _ -> None
  in
  Parsetree_helper.Annot.field ~name ~value

let parse_annotation t =
  match t.curr_span.token with
  | Token.At ->
      next t;
      let name =
        match parse_name t with
        | Some id -> id
        | None -> Error.expected_name t.curr_span
      in

      let desc =
        match t.curr_span.token with
        | Token.Parens_left ->
            next t;
            let fields = sep_by Token.Comma parse_annotations_field t in
            expect Token.Parens_right t;
            Some (Parsetree_helper.Annot.map ~fields)
        | _ -> None
      in

      Some (Parsetree_helper.Annot.mk ~name ~desc)
  | _ -> None

let parse_annotations t =
  let rec collect_annotations acc =
    match parse_annotation t with
    | Some annot -> collect_annotations (annot :: acc)
    | None -> acc
  in
  List.rev (collect_annotations [])

(*

*)
let rec parse_type_expr t =
  let parse_one t =
    match t.curr_span.token with
    | Token.Type_var name ->
        next t;
        Parsetree_helper.Typ.var name
    | Token.Id path -> (
        next t;
        let id = Parsetree_helper.id path in
        match t.curr_span.token with
        | Token.Lesser_than ->
            next t;
            let args = sep_by Token.Comma parse_type_expr t in
            expect Token.Greater_than t;
            Parsetree_helper.Typ.apply ~id ~args
        | _ -> Parsetree_helper.Typ.id id)
    | _ -> Error.expected_symbol ~sym:(`One_of []) ~found:t.curr_span
  in

  let type_exprs = sep_by Token.Arrow parse_one t in
  let rec to_arrow exprs =
    match exprs with
    | [] -> Error.expected_type_expression t.curr_span
    | [ t ] -> t
    | t :: ts -> Parsetree_helper.Typ.arrow t (to_arrow ts)
  in
  to_arrow type_exprs

let parse_type_def_record_field t =
  let annot = parse_annotations t in
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  expect Token.Colon t;
  Parsetree_helper.Type.label_decl ~name ~type_:(parse_type_expr t) ~annot

let parse_type_def_label_decls t =
  expect Token.Brace_left t;
  let fields = sep_by Token.Comma parse_type_def_record_field t in
  expect Token.Brace_right t;
  fields

let parse_type_def_variant_constructor t =
  let annot = parse_annotations t in
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  let args =
    match t.curr_span.token with
    | Token.Brace_left ->
        let labels = parse_type_def_label_decls t in
        Parsetree_helper.Type.variant_record_args ~labels
    | Token.Parens_left ->
        next t;
        let parts = sep_by Token.Comma parse_type_expr t in
        expect Token.Parens_right t;
        Parsetree_helper.Type.variant_tuple_args ~parts
    | _ -> Parsetree_helper.Type.variant_tuple_args ~parts:[]
  in
  Parsetree_helper.Type.variant_constructor ~name ~args ~annot

let parse_type_def_variant t =
  let constructors = sep_by Token.Pipe parse_type_def_variant_constructor t in
  Parsetree_helper.Type.variant ~constructors

let parse_type_def t ~annot =
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  expect Token.Equal t;
  let desc =
    match t.curr_span.token with
    | Token.Pipe ->
        next t;
        parse_type_def_variant t
    | _ -> Error.expected_symbol ~sym:(`Exact Token.Pipe) ~found:t.curr_span
  in
  Parsetree_helper.Type.mk ~name ~desc ~annot

(*

  Parse patterns:

  * Binding
  * Literals
    * Strings
    * Atoms
    * Numbers
    * Tuples
    * Lists
  * Records
  * Variants

*)
let rec parse_pattern t =
  match t.curr_span.token with
  | Token.Any ->
      next t;
      Parsetree_helper.Pat.any
  | Token.Id name ->
      next t;
      Parsetree_helper.Pat.bind (Parsetree_helper.id name)
  | Token.Parens_left -> parse_pattern_tuple t
  | Token.Bracket_left -> parse_pattern_list t
  | _ -> Error.expected_symbol ~sym:(`Exact Token.Pipe) ~found:t.curr_span

and parse_pattern_tuple t =
  expect Token.Parens_left t;
  let parts = sep_by Token.Comma parse_pattern t in
  expect Token.Parens_right t;
  Parsetree_helper.Pat.tuple ~parts

(*
  Parse List patternessions:

  Empty list: [] -> Nil
  List: [a, b, c] -> [a | [ b | [ c | [] ] ] ]
  Cons: [a, ...b] -> [a | b]

*)
and parse_pattern_list t =
  expect Token.Bracket_left t;
  let init = sep_by Token.Comma parse_pattern t in

  let last =
    match t.curr_span.token with
    | Token.Bracket_right -> Parsetree_helper.Pat.nil
    | Token.Dot_dot_dot ->
        next t;
        parse_pattern t
    | _ ->
        Error.expected_symbol
          ~sym:(`One_of [ Token.Comma; Token.Bracket_right ])
          ~found:t.curr_span
  in

  let rec make_list init last =
    match init with
    | [] -> Parsetree_helper.Pat.nil
    | [ head ] -> Parsetree_helper.Pat.list ~head ~tail:last
    | head :: xs -> Parsetree_helper.Pat.list ~head ~tail:(make_list xs last)
  in

  expect Token.Bracket_right t;

  make_list init last

(*

  Parse expressions:

  * Variables
  * Literals
    * Strings
    * Atoms
    * Numbers
    * Tuples
    * Lists
  * Records
  * Variants
  * Function calls

*)
let rec parse_expression t =
  let parse_one t =
    match t.curr_span.token with
    | Token.Id name -> (
        next t;
        let name = Parsetree_helper.Expr.var (Parsetree_helper.id name) in
        match t.curr_span.token with
        | Token.Parens_left -> parse_expr_call ~name t
        | _ -> name)
    | Token.Atom name ->
        next t;
        Parsetree_helper.Expr.lit_atom name
    | Token.String str ->
        next t;
        Parsetree_helper.Expr.lit_str str
    | Token.Parens_left -> parse_expr_tuple t
    | Token.Bracket_left -> parse_expr_list t
    | Token.Match -> parse_expr_match t
    | _ ->
        Error.expected_symbol
          ~sym:
            (`One_of
              [
                Token.Atom "atom";
                Token.Id "id";
                Token.String "string";
                Token.Parens_left;
                Token.Bracket_left;
              ])
          ~found:t.curr_span
  in

  let exprs = sep_by Token.Semicolon parse_one t in
  let rec to_seq exprs =
    match exprs with
    | [] -> Error.expected_expression t.curr_span
    | [ e ] -> e
    | e :: es -> Parsetree_helper.Expr.seq e (to_seq es)
  in
  to_seq exprs

and parse_expr_call ~name t =
  expect Token.Parens_left t;
  let args = sep_by Token.Comma parse_expression t in
  expect Token.Parens_right t;
  Parsetree_helper.Expr.call ~name ~args

and parse_expr_tuple t =
  expect Token.Parens_left t;
  let parts = sep_by Token.Comma parse_expression t in
  expect Token.Parens_right t;
  Parsetree_helper.Expr.tuple ~parts

(*
  Parse List expressions:

  Empty list: [] -> Nil
  List: [a, b, c] -> [a | [ b | [ c | [] ] ] ]
  Cons: [a, ...b] -> [a | b]

*)
and parse_expr_list t =
  expect Token.Bracket_left t;
  let init = sep_by Token.Comma parse_expression t in

  let last =
    match t.curr_span.token with
    | Token.Bracket_right -> Parsetree_helper.Expr.nil
    | Token.Dot_dot_dot ->
        next t;
        parse_expression t
    | _ ->
        Error.expected_symbol
          ~sym:(`One_of [ Token.Comma; Token.Bracket_right ])
          ~found:t.curr_span
  in

  let rec make_list init last =
    match init with
    | [] -> Parsetree_helper.Expr.nil
    | [ head ] -> Parsetree_helper.Expr.list ~head ~tail:last
    | head :: xs -> Parsetree_helper.Expr.list ~head ~tail:(make_list xs last)
  in

  expect Token.Bracket_right t;

  make_list init last

and parse_expr_match t =
  expect Token.Match t;
  let expr = parse_expression t in
  expect Token.Brace_left t;
  expect Token.Pipe t;
  let cases = sep_by Token.Pipe parse_case_branch t in
  expect Token.Brace_right t;
  Parsetree_helper.Expr.match_ ~expr ~cases

and parse_case_branch t =
  let lhs = parse_pattern t in
  expect Token.Arrow t;
  let rhs = parse_expression t in
  Parsetree_helper.Expr.case ~lhs ~rhs

(*

  Parse blocks of code that are surrounded by braces.

  ```
  { expr? }
  ```

*)
let parse_block t =
  expect Token.Brace_left t;
  let expr = parse_expression t in
  expect Token.Brace_right t;
  expr

let parse_fun_arg t = (Parsetree.No_label, parse_pattern t)

let parse_fun_decl t ~annot ~visibility =
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  expect Token.Parens_left t;
  let args = sep_by Token.Comma parse_fun_arg t in
  expect Token.Parens_right t;
  let body = parse_block t in
  Parsetree_helper.Fun_decl.mk ~name ~args ~visibility ~annot ~body

let parse_extern t ~annot ~visibility =
  expect Token.External t;
  let name =
    match parse_name t with
    | Some name -> name
    | None -> Error.expected_name t.curr_span
  in
  expect Token.Colon t;
  let type_sig = parse_type_expr t in
  expect Token.Equal t;
  let symbol =
    match t.curr_span.token with
    | Token.String str ->
        next t;
        str
    | _ ->
        Error.expected_symbol ~sym:(`Exact (Token.String "a string"))
          ~found:t.curr_span
  in
  Parsetree_helper.Ext.mk ~name ~type_sig ~symbol ~annot ~visibility

(*

  Parse blocks of code that are surrounded by braces.

  ```
  { expr? }
  ```

*)
let parse_structure_item t =
  let annot = parse_annotations t in

  let visibility = parse_visibility t in

  let node =
    match t.curr_span.token with
    | Token.External ->
        Parsetree_helper.Str.extern (parse_extern t ~annot ~visibility)
    | Token.Fn ->
        next t;
        Parsetree_helper.Str.fun_ (parse_fun_decl t ~annot ~visibility)
    | Token.Type ->
        next t;
        Parsetree_helper.Str.type_ (parse_type_def t ~annot)
    | _ -> Error.unexpected_token t.curr_span
  in
  node

let parse t =
  let rec parse_all acc =
    match parse_structure_item t with
    | (exception
        Error.Parse_error
          (Expected_symbol { found = { token = Token.EOF; _ }; _ }))
    | (exception Error.Parse_error End_of_file) ->
        List.rev acc
    | item -> parse_all (item :: acc)
  in
  parse_all []

(*** API ***********************************************************************)

let parse t =
  match parse t with
  | exception Lexer.Lexer_error err -> Error (`Lexer_error err)
  | exception Error.Parse_error err -> Error (`Parse_error err)
  | res -> Ok res

let make ~lexer =
  let ( let* ) = Result.bind in
  let* last_span = Lexer.scan lexer in
  Ok { lexer; last_span; curr_span = last_span }
