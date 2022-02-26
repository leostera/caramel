  $ cat main.caramel
  external format: string -> list<'a> -> unit = "io:format"
  
  pub fn main(args) {
    match args {
    | [] -> format("~s\n", [:bye_bye])
    | [x, ...rest] ->
        format("Hello, ~s!\n", [ x ]);
        main(rest)
    }
  }
  $ caramel parse --file main.caramel --dump-tokens --debug
  caramel: [DEBUG] (External (Id format) Colon (Id string) Arrow (Id list)
                     Lesser_than (Type_var a) Greater_than Arrow (Id unit)
                     Equal (String io:format) Pub Fn (Id main) Parens_left
                     (Id args) Parens_right Brace_left Match (Id args)
                     Brace_left Pipe Bracket_left Bracket_right Arrow
                     (Id format) Parens_left (String "~s\\n") Comma
                     Bracket_left (Atom bye_bye) Bracket_right Parens_right
                     Pipe Bracket_left (Id x) Comma Dot_dot_dot (Id rest)
                     Bracket_right Arrow (Id format) Parens_left
                     (String "Hello, ~s!\\n") Comma Bracket_left (Id x)
                     Bracket_right Parens_right Semicolon (Id main) Parens_left
                     (Id rest) Parens_right Brace_right Brace_right)
