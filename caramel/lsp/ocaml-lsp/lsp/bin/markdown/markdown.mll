{
  open Import
}

let ws = [' ' '\t' '\r']*

rule md acc b = parse
  | "```typescript[@ignore]" ws "\n" { ts acc b true lexbuf }
  | "```typescript" ws "\n" { ts acc b false lexbuf }
  | eof { acc }
  | _ { md acc b lexbuf }

and ts acc b ignored = parse
  (* We parse typescript comments because they can include mrakdown fragments *)
  | "/*" as s { Buffer.add_string b s; comment acc b ignored lexbuf }
  | "```"
    { let acc =
        if ignored then begin
          Buffer.clear b; acc
        end else
          let snippet = Buffer.contents b in
          Buffer.clear b;
          snippet :: acc
      in
      md acc b lexbuf
    }
  | "\r\n" { Buffer.add_string b "\n"; ts acc b ignored lexbuf }
  | _ as c
    { Buffer.add_char b c;
      ts acc b ignored lexbuf
    }
  | eof { Code_error.raise "unterminated typescript snippet" [] }

and comment acc b ignored = parse
  | "*/" as s { Buffer.add_string b s; ts acc b ignored lexbuf }
  | "\r\n" { Buffer.add_string b "\n"; comment acc b ignored lexbuf }
  | _ as c { Buffer.add_char b c; comment acc b ignored lexbuf }
  | eof { failwith "unterminated comment" }

{
  let read_typescript lexbuf =
    let b = Buffer.create 512 in
    List.rev (md [] b lexbuf)
}
