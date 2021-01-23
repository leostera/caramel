(* This function is recursive, so we must mark it with `let rec` *)
let rec say_hello names =

  (* We can use `match ... with` to inspect values like a Switch/Case *)
  match names with

  (* If there are no names, then the list is empty! and we're done *)
  | [] ->
      (* We return a *unit* (the empty parens: ()). This is the closest we have
       * in Caramel to the usual `:ok` or `ok` atoms returned in Elixir / Erlang
       * to indicate that nothing happened, but everything's okay
       *)
      ()

  (* If there is at least one name, we can proceed... *)
  | n :: names' ->
      Io.format "Hello, ~s!\n" [n];
      say_hello names'

let main args = say_hello args
