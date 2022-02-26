(**

   Version: 0

   This rudimentary macro expander will scan the parse tree, build an
   environment of macro definitions, and evaluate all the macros found both as
   annotations in module structure items and within expressions.

   It allows macros to be composed, calling macros from within macros, and
   quasiquotation allows to use the surface syntax of Caramel to define both
   new structure items and expressions to be returned from the macros.

   For example, the following macro creates a new function in a module that
   returns whatever the macro argument `name` is:

   ```caramel
   macro hello(name) {
     quote {
       pub fn hello() {
         unquote(name)
       }
     }
   }
   ```

   To evaluate the macro definitions themselves, a subset of Caramel is made
   available that is interpreted with the `Interpreter` module.

   For macros specified as annotations on structure items, the structure item
   itself is reified into a data structure that can be manipualted through the
   interpreter.

   Note that the interpreter evalutes an untyped language.

   Things to improve:

   * The environment currently has only a single level of bindings - this
     prevents macros from using intermediate variables that call out to
     other functions.

   * The interpreter evaluation logic feels very messy

   * Build a cleaner reification of the AST values - we should write out the
     type definitions of the Caramel parsetree in Caramel.

*)

open Parsetree
open Parsetree_helper

module Error = struct
  open Sexplib.Std

  type t =
    [ `Cannot_parse_unevaluated_unquoted_expressions of Parsetree.expr
    | `Derive_macro_must_take_an_argument of Parsetree.id
    | `Derive_macro_not_found of Parsetree.id
    | `Field_access_error of Parsetree.id * (Parsetree.id * expr) list
    | `Macro_must_return_quoted_expressions of Parsetree.expr
    | `Macro_must_return_quoted_structure_item of Parsetree.id * Parsetree.expr
    | `Macro_recursion_limit_reached
    | `Match_error of Parsetree.expr * Parsetree.case list
    | `Parse_error of Parser.Error.t
    | `Type_error of Parsetree.expr list * Parsetree.expr list
    | `Unbound_macro_name of Parsetree.id
    | `Unbound_name_in_unquoted_expression of Parsetree.id
    | `Unbound_value_name of Parsetree.id
    | `Unquote_splicing_must_return_lists of Parsetree.expr
    | `Unsupported_reification_of_item of Parsetree.structure_item ]
  [@@deriving sexp]

  let pp ppf error =
    let sexp = sexp_of_t error in
    Format.fprintf ppf "%a" (Sexplib.Sexp.pp_hum_indent 2) sexp

  exception Runtime_error of t

  let err e = raise (Runtime_error e)

  let match_error ~expr ~cases = err (`Match_error (expr, cases))

  let macro_must_return_quoted_expressions ~macro =
    err (`Macro_must_return_quoted_expressions macro)

  let unbound_macro_name id = err (`Unbound_macro_name id)

  let unbound_value_name id = err (`Unbound_value_name id)

  let unbound_name_in_unquoted_expression id =
    err (`Unbound_name_in_unquoted_expression id)

  let derive_macro_not_found name = err (`Derive_macro_not_found name)

  let macro_must_return_quoted_structure_item ~name ~macro =
    err (`Macro_must_return_quoted_structure_item (name, macro))

  let field_access_error ~field ~fields =
    err (`Field_access_error (field, fields))

  let cannot_parse_unevaluated_unquoted_expressions expr =
    err (`Cannot_parse_unevaluated_unquoted_expressions expr)

  let macro_recursion_limit_reached () = err `Macro_recursion_limit_reached

  let macro_parser_error e = err (`Parse_error e)

  let type_error ~expected ~found = err (`Type_error (expected, found))

  let unquote_splicing_must_return_lists ~expr =
    err (`Unquote_splicing_must_return_lists expr)

  let derive_macro_must_take_an_argument ~name =
    err (`Derive_macro_must_take_an_argument name)

  let unsupported_reification ~item =
    err (`Unsupported_reification_of_item item)
end

module Env = struct
  open Sexplib.Std

  type t = {
    macros : (Parsetree.id, Parsetree.expr) Hashtbl.t;
    vars : (Parsetree.id, Parsetree.expr) Hashtbl.t;
    funs : (Parsetree.id, Parsetree.expr) Hashtbl.t;
    mutable level : int;
  }

  let empty =
    {
      macros = Hashtbl.create 1024;
      vars = Hashtbl.create 1024;
      funs = Hashtbl.create 1024;
      level = 0;
    }

  let inc_level ~env = env.level <- env.level + 1

  let dec_level ~env = env.level <- env.level - 1

  let macro_recursion_limit_reached ~env = env.level > 10

  let clear_names ~env = Hashtbl.clear env.vars

  let is_var ~env id = Hashtbl.mem env.vars id

  let find_var ~env id = Hashtbl.find_opt env.vars id

  let get_var ~env id =
    match find_var ~env id with
    | Some value -> value
    | None -> Error.unbound_value_name id

  (* TODO: handle extracting values when pattern matching and binding them *)
  let bind_names ~env bindings =
    List.iter
      (fun ((_, pat), value) ->
        match pat with
        | Pat_bind name -> Hashtbl.add env.vars name value
        | _ -> ())
      bindings;
    env

  let is_macro ~env id = Hashtbl.mem env.macros id

  let find_macro ~env id = Hashtbl.find_opt env.macros id

  let get_macro ~env id =
    match find_macro ~env id with
    | Some macro -> macro
    | None -> Error.unbound_macro_name id

  let bind_macros ~env parsetree =
    List.iter
      (fun item ->
        match item with
        | Str_macro ({ fn_name = Id name; _ } as macro) ->
            Hashtbl.add env.macros (Id name)
              (Expr.lambda ~args:macro.fn_args ~body:macro.fn_body)
        | _ -> ())
      parsetree;

    env
end

module Well_known_symbols = struct
  let symbols =
    [
      ( "-",
        function
        | [ Expr_literal (Lit_integer a); Expr_literal (Lit_integer b) ] ->
            let a = int_of_string a in
            let b = int_of_string b in
            Expr.lit_int (Int.to_string (a - b))
        | found ->
            Error.type_error ~found
              ~expected:[ Expr.lit_int "0"; Expr.lit_int "1" ] );
      ( "+",
        function
        | [ Expr_literal (Lit_integer a); Expr_literal (Lit_integer b) ] ->
            let a = int_of_string a in
            let b = int_of_string b in
            Expr.lit_int (Int.to_string (a + b))
        | found ->
            Error.type_error ~found
              ~expected:[ Expr.lit_int "0"; Expr.lit_int "1" ] );
    ]

  let table =
    let tbl = Hashtbl.create 1024 in
    List.iter (fun (id, fn) -> Hashtbl.add tbl (Id [ id ]) fn) symbols;
    tbl

  let find id = Hashtbl.find_opt table id

  let get id =
    match find id with
    | Some value -> value
    | None -> Error.unbound_value_name id

  let is_well_known id = Hashtbl.mem table id

  let eval id ~args =
    match Hashtbl.find_opt table id with
    | None -> Error.unbound_value_name id
    | Some fn -> fn args
end

(*
  Our Reader will accept a list of quasiquoted expressions and parse them into
  a Parsetree value
*)
module Reader = struct
  let read quasi =
    let rec read_quotes quotes acc =
      match quotes with
      | [] -> acc
      | Quasiquote tokens :: rest ->
          let tokens = List.filter (fun x -> x <> Token.Quote) tokens in
          read_quotes rest (acc @ tokens)
      | Unquote_splicing expr :: _ | Unquote expr :: _ ->
          Error.cannot_parse_unevaluated_unquoted_expressions expr
    in
    let tokens = read_quotes quasi [] in
    let token_provider = Token_provider.from_tokens ~tokens in
    Parser.make ~token_provider

  let parse_expression quasi =
    let ( let* ) = Result.bind in
    let* reader = read quasi in
    match Parser.parse_expression reader with
    | exception Parser.Error.Parse_error e -> Error.macro_parser_error e
    | expr -> Ok expr

  let parse_structure_item quasi =
    let ( let* ) = Result.bind in
    let* reader = read quasi in
    match Parser.parse_structure_item reader with
    | exception Parser.Error.Parse_error e -> Error.macro_parser_error e
    | expr -> Ok expr

  let id_to_token (Id path) = String.concat "." path

  let sep_by i xs =
    let rec aux xs =
      match xs with x :: rest -> x :: i :: aux rest | [] -> []
    in
    List.concat (aux xs)

  let list_to_tokens a b =
    match b with
    | [] -> [ Token.Bracket_left ] @ a @ [ Token.Bracket_right ]
    | _ ->
        [ Token.Bracket_left ] @ a @ [ Token.Dot_dot_dot ] @ b
        @ [ Token.Bracket_right ]

  let tuple_to_tokens parts =
    [ Token.Brace_left ] @ sep_by [ Token.Comma ] parts @ [ Token.Brace_right ]

  let rec expression_to_tokens expr =
    match expr with
    | Expr_call (fn, fn_args) ->
        let args = List.map expression_to_tokens fn_args in
        expression_to_tokens fn @ [ Token.Parens_left ]
        @ sep_by [ Token.Comma ] args
        @ [ Token.Parens_right ]
    | Expr_var name -> [ Token.Id (id_to_token name) ]
    | Expr_quote quasi -> [ Token.Quote ] @ quote_to_tokens quasi
    | Expr_lambda (fn_args, body) ->
        let args = List.map (fun (_, pat) -> pattern_to_tokens pat) fn_args in
        [ Token.Fn; Token.Parens_left ]
        @ sep_by [ Token.Comma ] args
        @ [ Token.Parens_right; Token.Brace_left ]
        @ expression_to_tokens body @ [ Token.Brace_right ]
    | Expr_cons (a, b) ->
        list_to_tokens (expression_to_tokens a) (expression_to_tokens b)
    | Expr_nil -> list_to_tokens [] []
    | Expr_match (expr, cases) ->
        [ Token.Match ] @ expression_to_tokens expr @ [ Token.Brace_left ]
        @ cases_to_tokens cases @ [ Token.Brace_right ]
    | Expr_let (pat, body, expr) ->
        [ Token.Let ] @ pattern_to_tokens pat @ [ Token.Equal ]
        @ expression_to_tokens body @ [ Token.Semicolon ]
        @ expression_to_tokens expr
    | Expr_seq (a, b) ->
        expression_to_tokens a @ [ Token.Semicolon ] @ expression_to_tokens b
    | Expr_tuple parts -> tuple_to_tokens (List.map expression_to_tokens parts)
    | Expr_record fields ->
        let fields = fields_to_tokens expression_to_tokens fields in
        [ Token.Brace_left ] @ fields @ [ Token.Brace_right ]
    | Expr_constructor (name, body) ->
        let body =
          match body with
          | Ctr_record fields ->
              let fields = fields_to_tokens expression_to_tokens fields in
              [ Token.Brace_left ] @ fields @ [ Token.Brace_right ]
          | Ctr_tuple parts ->
              let parts =
                sep_by [ Token.Comma ] (List.map expression_to_tokens parts)
              in
              [ Token.Brace_left ] @ parts @ [ Token.Brace_right ]
        in
        [ Token.Id (id_to_token name) ] @ body
    | Expr_field (expr, field) ->
        expression_to_tokens expr @ [ Token.Dot; Token.Id (id_to_token field) ]
    | Expr_open (id, expr) ->
        [ Token.Open; Token.Id (id_to_token id) ] @ expression_to_tokens expr
    | Expr_literal lit -> lit_to_token lit

  and pattern_to_tokens pat =
    match pat with
    | Pat_any -> [ Token.Any ]
    | Pat_bind id -> [ Token.Id (id_to_token id) ]
    | Pat_cons (a, b) ->
        list_to_tokens (pattern_to_tokens a) (pattern_to_tokens b)
    | Pat_nil -> list_to_tokens [] []
    | Pat_tuple parts -> tuple_to_tokens (List.map pattern_to_tokens parts)
    | Pat_literal lit -> lit_to_token lit
    | Pat_record (fields, exhaustive) ->
        let fields = fields_to_tokens pattern_to_tokens fields in
        let tail =
          match exhaustive with
          | Exhaustive -> []
          | Partial -> [ Token.Comma; Token.Any ]
        in
        [ Token.Brace_left ] @ fields @ tail @ [ Token.Brace_right ]
    | Pat_constructor (id, body) ->
        let body =
          match body with
          | Ctp_record (fields, exhaustive) ->
              let tail =
                match exhaustive with
                | Exhaustive -> []
                | Partial -> [ Token.Comma; Token.Any ]
              in
              let fields = fields_to_tokens pattern_to_tokens fields in
              [ Token.Brace_left ] @ fields @ tail @ [ Token.Brace_right ]
          | Ctp_tuple parts ->
              let parts =
                sep_by [ Token.Comma ] (List.map pattern_to_tokens parts)
              in
              [ Token.Brace_left ] @ parts @ [ Token.Brace_right ]
        in
        [ Token.Id (id_to_token id) ] @ body

  and quote_to_tokens quasi =
    List.concat_map
      (fun q ->
        match q with
        | Quasiquote tokens -> tokens
        | Unquote_splicing expr ->
            [ Token.Unquote_splicing ] @ expression_to_tokens expr
        | Unquote expr -> [ Token.Unquote ] @ expression_to_tokens expr)
      quasi

  and fields_to_tokens :
      type a. (a -> Token.t list) -> (id * a) list -> Token.t list =
   fun f fields ->
    sep_by [ Token.Comma ]
      (List.map
         (fun (id, value) -> [ Token.Id (id_to_token id) ] @ f value)
         fields)

  and cases_to_tokens cases =
    List.concat_map
      (fun { cs_lhs; cs_rhs } ->
        [ Token.Pipe ] @ pattern_to_tokens cs_lhs @ [ Token.Arrow ]
        @ expression_to_tokens cs_rhs)
      cases

  and lit_to_token lit =
    match lit with
    | Lit_string str -> [ Token.String str ]
    | Lit_integer int -> [ Token.Integer int ]
    | Lit_atom atom -> [ Token.Atom atom ]
end

(**
   Small interpreter for a subset of Caramel that is valid for Macro expressions.
*)
module Interpreter = struct
  let rec eval ~is_quoted ~env expr =
    if Env.macro_recursion_limit_reached ~env then
      Error.macro_recursion_limit_reached ();
    match expr with
    (**************** macro reduction *********************************)
    (* NOTE: all evaluations should begin with an Expr_call that is NOT an id *)
    | Expr_call (Expr_lambda (args, body), fn_args) when is_quoted = false ->
        Env.inc_level ~env;
        let fn_args = List.map (eval ~is_quoted ~env) fn_args in
        let bindings = List.combine args fn_args in
        let env = Env.bind_names ~env bindings in
        let result = eval ~is_quoted:false ~env body in
        (* Logs.debug (fun f -> f "macro: %a" Parsetree.pp_expr result); *)
        Env.clear_names ~env;
        Env.dec_level ~env;
        result
    | Expr_call (Expr_var id, fn_args)
      when Env.is_macro ~env id && is_quoted = false ->
        Env.inc_level ~env;
        let macro = Env.get_macro ~env id in
        let macro_call = Expr.call ~name:macro ~args:fn_args in
        let result = eval ~is_quoted:false ~env macro_call in
        Env.dec_level ~env;
        result
    | Expr_call (Expr_var id, args)
      when Well_known_symbols.is_well_known id && is_quoted = false ->
        Env.inc_level ~env;
        let args = List.map (eval ~is_quoted ~env) args in
        let fn = Well_known_symbols.get id in
        let result = fn args in
        Env.dec_level ~env;
        result
    | Expr_quote quasi -> eval_quote ~is_quoted ~env quasi
    (**************** more reduction *******************************************)
    | Expr_var id when is_quoted = false -> Env.get_var ~env id
    | Expr_match (expr, cases) when is_quoted = false ->
        eval_match ~env (eval ~is_quoted ~env expr) cases
    | Expr_field (value, field) when is_quoted = false ->
        let value = eval ~is_quoted ~env value in
        eval_field_access value field
    | Expr_let (pat, body, expr) when is_quoted = false ->
        let value = eval ~is_quoted ~env body in
        let env = Env.bind_names ~env [ ((No_label, pat), value) ] in
        eval ~is_quoted ~env expr
    (**************** recursion boilerplate :( *********************************)
    | Expr_call (name, args) ->
        let args = List.map (eval ~is_quoted ~env) args in
        Expr.call ~name ~args
    | Expr_match (expr, cases) ->
        Expr.match_
          ~expr:(eval ~is_quoted ~env expr)
          ~cases:
            (List.map
               (fun { cs_lhs; cs_rhs } ->
                 { cs_lhs; cs_rhs = eval ~is_quoted ~env cs_rhs })
               cases)
    | Expr_field (expr, field) ->
        Expr.field_access ~expr:(eval ~is_quoted ~env expr) ~field
    | Expr_lambda (args, body) ->
        Expr.lambda ~args ~body:(eval ~is_quoted ~env body)
    | Expr_cons (a, b) ->
        Expr.list ~head:(eval ~is_quoted ~env a) ~tail:(eval ~is_quoted ~env b)
    | Expr_let (pat, body, expr) ->
        Expr.let_ ~pat
          ~body:(eval ~is_quoted ~env body)
          ~expr:(eval ~is_quoted ~env expr)
    | Expr_seq (a, b) ->
        Expr.seq (eval ~is_quoted ~env a) (eval ~is_quoted ~env b)
    | Expr_tuple parts ->
        Expr.tuple ~parts:(List.map (eval ~is_quoted ~env) parts)
    | Expr_record fields ->
        Expr.record
          ~fields:(List.map (fun (k, v) -> (k, eval ~is_quoted ~env v)) fields)
    | Expr_constructor (name, Ctr_tuple parts) ->
        Expr.constructor_tuple ~name
          ~parts:(List.map (eval ~is_quoted ~env) parts)
    | Expr_constructor (name, Ctr_record fields) ->
        Expr.constructor_record ~name
          ~fields:(List.map (fun (k, v) -> (k, eval ~is_quoted ~env v)) fields)
    | Expr_var _ | Expr_open _ | Expr_literal _ | Expr_nil -> expr

  and eval_quote ~is_quoted ~env quasi =
    let rec splice expr =
      match expr with
      | Expr_cons (h, Expr_nil) -> Reader.expression_to_tokens h
      | Expr_cons (h, t) ->
          let h = Reader.expression_to_tokens h in
          h @ [ Token.Comma ] @ splice t
      | _ -> Error.unquote_splicing_must_return_lists ~expr
    in

    let quote =
      List.concat_map
        (fun q ->
          match q with
          | Unquote_splicing expr -> splice (eval ~is_quoted ~env expr)
          | Unquote expr ->
              Reader.expression_to_tokens (eval ~is_quoted ~env expr)
          | Quasiquote tokens -> tokens)
        quasi
    in
    Expr.quote ~quote:[ Quasiquote quote ]

  and eval_field_access value field =
    match value with
    | Expr_record fields -> (
        match List.assoc_opt field fields with
        | Some value -> value
        | None -> Error.field_access_error ~field ~fields)
    | _ ->
        Logs.debug (fun f -> f "can't access %a %a" pp_expr value pp_id field);
        exit 21

  (* TODO(leandro): rewrite this because holy shit its so ugly *)
  and eval_match ~env expr cases =
    let flatten_bindings results =
      let rec aux results (res, names) =
        match results with
        | [] -> (res, List.rev names)
        | (false, _) :: _ -> (false, [])
        | (true, binds) :: rest -> aux rest (true, binds @ names)
      in
      aux results (false, [])
    in

    let rec match_ pat expr =
      match (pat, expr) with
      | Pat_bind name, _ -> (true, [ (name, expr) ])
      | Pat_any, _ -> (true, [])
      | Pat_nil, Expr_nil -> (true, [])
      | Pat_cons (h1, t1), Expr_cons (h2, t2) -> (
          match match_ h1 h2 with
          | true, names -> (
              match match_ t1 t2 with
              | true, more_names -> (true, more_names @ names)
              | _ -> (false, []))
          | _ -> (false, []))
      | Pat_tuple t1, Expr_tuple t2 ->
          let pairs = List.combine t1 t2 in
          let results = List.map (fun (pat, expr) -> match_ pat expr) pairs in
          flatten_bindings results
      | Pat_literal lit1, Expr_literal lit2 -> (lit1 = lit2, [])
      | Pat_constructor (id1, pat), Expr_constructor (id2, expr) when id1 = id2
        -> (
          match (pat, expr) with
          | Ctp_record (f0, _), Ctr_record f1 ->
              List.map
                (fun (id, pat) ->
                  match List.assoc_opt id f1 with
                  | Some expr -> match_ pat expr
                  | None -> (false, []))
                f0
              |> flatten_bindings
          | Ctp_tuple p0, Ctr_tuple p1 ->
              let pairs = List.combine p0 p1 in
              List.map (fun (pat, expr) -> match_ pat expr) pairs
              |> flatten_bindings
          | _ -> (false, []))
      | _, _ -> (false, [])
    in

    let run_case { cs_lhs; cs_rhs } =
      match match_ cs_lhs expr with
      | true, names -> Ok (cs_rhs, names)
      | _ -> Error `no_match
    in

    let rec check_cases cases =
      match cases with
      | [] -> Error `unexpected_match
      | case :: rest -> (
          match run_case case with
          | Ok result -> Ok result
          | Error `no_match -> check_cases rest)
    in

    match check_cases cases with
    | Ok (result, bindings) ->
        let bindings =
          List.map (fun (id, expr) -> ((No_label, Pat_bind id), expr)) bindings
        in
        let env = Env.bind_names ~env bindings in
        eval ~is_quoted:false ~env result
    | Error `unexpected_match -> Error.match_error ~expr ~cases
end

module Reifier = struct
  let rec reify_structure_item item =
    match item with
    | Str_type { typ_name = Id [ name ]; typ_annot = annot; typ_desc; _ } ->
        let name = Expr.lit_str name in
        let annot = reify_annotations annot in

        let kind =
          match typ_desc with
          | Type_variant { tyk_constructors } ->
              reify_variant_decl tyk_constructors
          | Type_record { tyk_labels } -> reify_record_decl tyk_labels
          | Type_abstract ->
              Expr.constructor_record ~name:(id "Abstract") ~fields:[]
          | Type_alias _typ_expr ->
              Expr.constructor_record ~name:(id "Alias")
                ~fields:
                  [
                    Expr.field ~name:(id "name")
                      ~expr:(Expr.lit_str "MISSING_TYPE_EXPR");
                  ]
        in

        let fields =
          [
            Expr.field ~name:(id "name") ~expr:name;
            Expr.field ~name:(id "kind") ~expr:kind;
            Expr.field ~name:(id "annot") ~expr:annot;
          ]
        in
        Expr.record ~fields
    | _ -> Error.unsupported_reification ~item

  (*
    Variant {
      constructors: [
        Constructor {
          name: "Hello",
          args: ...
        }
      ]
    }
  *)
  and reify_variant_decl ctrs =
    Expr.constructor_record ~name:(id "Variant")
      ~fields:
        [
          Expr.field ~name:(id "constructors")
            ~expr:
              (ctrs
              |> List.map (fun { ctr_name; ctr_args; ctr_annot } ->
                     Expr.constructor_record ~name:(id "Constructor")
                       ~fields:
                         [
                           Expr.field ~name:(id "name")
                             ~expr:(Expr.lit_str (id_to_string ctr_name));
                           Expr.field ~name:(id "args")
                             ~expr:
                               (match ctr_args with
                               | Record labels -> reify_record_decl labels
                               | Tuple parts -> reify_tuple_ctr parts);
                           Expr.field ~name:(id "annot")
                             ~expr:(reify_annotations ctr_annot);
                         ])
              |> Expr.from_list);
        ]

  (*
    Tuple(t0, t1, t2)
  *)
  and reify_tuple_ctr _parts =
    Expr.constructor_tuple ~name:(id "Tuple") ~parts:[]

  (*
    Record {
      fields: [
        Field { name: "name", type: "string" }
      ]
    }
  *)
  and reify_record_decl labels =
    Expr.constructor_record ~name:(id "Record")
      ~fields:
        [
          Expr.field ~name:(id "fields")
            ~expr:(List.map reify_record_label labels |> Expr.from_list);
        ]

  and reify_record_label { lbl_name; lbl_type = _; lbl_annot } =
    Expr.constructor_record ~name:(id "Field")
      ~fields:
        [
          Expr.field ~name:(id "name")
            ~expr:(Expr.lit_str (id_to_string lbl_name));
          Expr.field ~name:(id "annot") ~expr:(reify_annotations lbl_annot);
        ]

  and reify_annotations annots =
    let fields =
      List.map
        (fun ann ->
          let fields =
            match ann.ann_desc with
            | Some (Map fields) ->
                List.map
                  (fun (id, expr) ->
                    let expr =
                      match expr with
                      | Some e -> e
                      | None -> Expr.lit_atom "nil"
                    in
                    Expr.field ~name:id ~expr)
                  fields
            | None -> []
          in
          Expr.field ~name:ann.ann_name ~expr:(Expr.record ~fields))
        annots
    in
    Expr.record ~fields
end

let rec expand_expr ~env expr =
  match expr with
  | Expr_call (Expr_var id, args) when Env.is_macro ~env id -> (
      let top_args = List.map (expand_expr ~env) args in
      let macro = Env.get_macro ~env id in

      match macro with
      | Expr_lambda (args, body) -> (
          let bindings = List.combine args top_args in
          let env = Env.bind_names ~env bindings in
          match Interpreter.eval ~is_quoted:false ~env body with
          | Expr_quote quasi -> (
              Env.clear_names ~env;
              match Reader.parse_expression quasi with
              | Ok expr -> expr
              | Error (`Lexer_error e) -> Lexer.err e)
          | _ -> Error.macro_must_return_quoted_expressions ~macro)
      | _ -> Error.unbound_macro_name id)
  (***** recursion boilerplate :( ******)
  | Expr_field (expr, field) ->
      Expr.field_access ~expr:(expand_expr ~env expr) ~field
  | Expr_lambda (args, body) -> Expr.lambda ~args ~body:(expand_expr ~env body)
  | Expr_call (name, args) ->
      let args = List.map (expand_expr ~env) args in
      Expr.call ~name ~args
  | Expr_cons (a, b) ->
      Expr.list ~head:(expand_expr ~env a) ~tail:(expand_expr ~env b)
  | Expr_quote _quasi -> expr
  | Expr_match (expr, cases) ->
      Expr.match_ ~expr:(expand_expr ~env expr)
        ~cases:
          (List.map
             (fun { cs_lhs; cs_rhs } ->
               { cs_lhs; cs_rhs = expand_expr ~env cs_rhs })
             cases)
  | Expr_let (pat, body, expr) ->
      Expr.let_ ~pat ~body:(expand_expr ~env body) ~expr:(expand_expr ~env expr)
  | Expr_seq (a, b) -> Expr.seq (expand_expr ~env a) (expand_expr ~env b)
  | Expr_tuple parts -> Expr.tuple ~parts:(List.map (expand_expr ~env) parts)
  | Expr_constructor (name, Ctr_tuple parts) ->
      Expr.constructor_tuple ~name ~parts:(List.map (expand_expr ~env) parts)
  | Expr_constructor (name, Ctr_record fields) ->
      Expr.constructor_record ~name
        ~fields:(List.map (fun (k, v) -> (k, expand_expr ~env v)) fields)
  | Expr_record fields ->
      Expr.record
        ~fields:(List.map (fun (k, v) -> (k, expand_expr ~env v)) fields)
  | Expr_open _ | Expr_literal _ | Expr_nil | Expr_var _ -> expr

let expand ~env item =
  match item with
  | Str_fun fn -> Str_fun { fn with fn_body = expand_expr ~env fn.fn_body }
  | _ -> item

module Annotation = struct
  let expand_annot ~env:_ ~annot:_ _item = []

  let expand_derive ~env ~annot item =
    let do_expand name item =
      match Env.find_macro ~env name with
      | Some (Expr_lambda ([], _)) ->
          Error.derive_macro_must_take_an_argument ~name
      | Some (Expr_lambda (args, body) as macro) -> (
          let item_expr = Reifier.reify_structure_item item in
          let bindings = List.combine args [ item_expr ] in
          let env = Env.bind_names ~env bindings in
          match Interpreter.eval ~is_quoted:false ~env body with
          | Expr_quote quasi -> (
              Env.clear_names ~env;
              match Reader.parse_structure_item quasi with
              | Ok str_item -> str_item
              | Error (`Lexer_error e) -> Lexer.err e)
          | _ -> Error.macro_must_return_quoted_expressions ~macro)
      | _ -> Error.unbound_macro_name name
    in

    let rec fold_derives kvs item acc =
      match kvs with
      | [] -> acc
      | (name, _) :: rest -> fold_derives rest item (do_expand name item :: acc)
    in
    let derives =
      match annot.ann_desc with Some (Map derives) -> derives | None -> []
    in

    fold_derives derives item []

  (*

    NOTE(leandro): Expand items based on annotation macros. These run _before_
    expression macros since they generate more code (types, funs, externs, etc).

  *)
  let expand_annotations ~env item =
    let annotations = Parsetree_helper.Str.annotations item in

    (* NOTE: runs ONLY the derive annotation first *)
    let rec run_derive item annotations acc new_items =
      match annotations with
      | [] -> (new_items, acc)
      | ({ ann_name = Id name; _ } as annot) :: rest when name = [ "derive" ] ->
          run_derive item rest acc (expand_derive ~env ~annot item @ new_items)
      | annot :: rest -> run_derive item rest (annot :: acc) new_items
    in

    (* NOTE: runs all other annotations *)
    let rec run_annot item annotations new_items =
      match annotations with
      | [] -> new_items
      | annot :: rest ->
          run_annot item rest (expand_annot ~env ~annot item @ new_items)
    in

    let new_items, other_annotations = run_derive item annotations [] [] in
    let all_new_items = run_annot item other_annotations new_items in

    item :: all_new_items
end

let run parsetree =
  match
    let env = Env.bind_macros ~env:Env.empty parsetree in
    parsetree
    |> List.concat_map (Annotation.expand_annotations ~env)
    |> List.map (fun item ->
           match item with
           | Str_mod_expr (Mod_decl mod_decl) ->
               Str_mod_expr
                 (Mod_decl
                    {
                      mod_decl with
                      mod_items =
                        mod_decl.mod_items
                        |> List.concat_map (Annotation.expand_annotations ~env)
                        |> List.map (expand ~env);
                    })
           | _ -> item)
    |> List.map (expand ~env)
  with
  | exception Error.Runtime_error e -> Error (`Runtime_error e)
  | parsetree -> Ok parsetree
