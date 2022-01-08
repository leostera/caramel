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
  $ caramel parse --file main.caramel --dump-caml --debug
  caramel: [DEBUG] external format : string -> 'a list -> unit = "io:format"
                   let rec main args =
                     match args with
                     | [] -> format "~s\\n" [`bye_bye]
                     | x::rest -> (format "Hello, ~s!\\n" [x]; main rest)
