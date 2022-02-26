open Sexplib.Std

type t = { mutable lexer : Lexer.t; mutable tokens : Token.t list option }
[@@deriving sexp]

let make ~lexer = { lexer; tokens = None }

let from_tokens ~tokens =
  { lexer = Lexer.make ~filename:"_none_" ~contents:""; tokens = Some tokens }

let next t =
  match t.tokens with
  | None -> Lexer.scan t.lexer
  | Some [] -> Ok Span.eof
  | Some (token :: tokens) ->
      t.tokens <- Some tokens;
      Ok (Span.of_token ~token)

let peek t =
  match t.tokens with
  | None -> Lexer.peek t.lexer
  | Some [] -> Ok Span.eof
  | Some (token :: _) -> Ok (Span.of_token ~token)
