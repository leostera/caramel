open Import
open Ts_types

type test =
  { snippet : string
  ; loc : Lexing.position
  ; exn : exn
  }

let name_table (defns : Unresolved.t list) =
  List.map defns ~f:(fun (def : _ Named.t) -> (def.name, def))
  |> String.Map.of_list_reducei ~f:(fun name v1 v2 ->
         let open Unresolved in
         match (v1.Named.data, v2.data) with
         | Enum_anon _, _ -> v1
         | _, Enum_anon _ -> v2
         | _, _ ->
           if v1 = v2 then
             v1
           else
             let open Dyn.Encoder in
             Code_error.raise "definition conflict" [ ("name", string name) ])

let resolve_all (defns : Unresolved.t list) =
  let names = name_table defns in
  let defns = String.Map.values names in
  let rename_change_field =
    object
      inherit Unresolved.map as super

      method! field x =
        if x.name = "change" then
          match x.data with
          | Single { typ = Ident "number"; optional = true } ->
            let typ = Unresolved.Ident "TextDocumentSyncKind" in
            let data = Unresolved.Single { typ; optional = true } in
            super#field { x with data }
          | _ -> super#field x
        else
          super#field x
    end
  in
  let unresolved = List.map defns ~f:rename_change_field#t in
  Ts_types.resolve_all unresolved ~names

let test_snippets s =
  let fails, succs =
    List.partition_map s ~f:(fun s ->
        let lexbuf = Lexing.from_string s in
        try Right (Ts_parser.main Ts_lexer.token lexbuf)
        with exn -> Left { snippet = s; loc = lexbuf.lex_curr_p; exn })
  in
  ignore (resolve_all (List.concat succs));
  fails

let pp_results ppf tests =
  List.iteri tests ~f:(fun i { snippet; loc; exn } ->
      Format.pp_print_string ppf snippet;
      Format.fprintf ppf "line: %d char: %d@." loc.pos_lnum
        (loc.pos_cnum - loc.pos_bol);
      Format.fprintf ppf "%d. exn: %s@.%!" (i + 1) (Printexc.to_string exn);
      Format.fprintf ppf "@.---@.")

let of_snippets s =
  List.concat_map s ~f:(fun s ->
      let lexbuf = Lexing.from_string s in
      try Ts_parser.main Ts_lexer.token lexbuf with _exn -> [])
