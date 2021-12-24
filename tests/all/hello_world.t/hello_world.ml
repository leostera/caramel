external format : string -> 'a list -> unit = "io:format"

let rec main x =
  match x with
  | [] -> format "~s\n" [ "bye!" ]
  | x :: rest ->
      format "Hello, ~s!\n" [ x ];
      main rest
