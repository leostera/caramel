type valid
type invalid

type 'a state = { counter: int; flag: bool }

let empty : unit -> 'a state = fun () -> { counter = 0; flag = false }

let make : unit -> invalid state = fun () -> empty ()

let validate : invalid state -> (valid state, string) result =
  fun { counter; flag } -> Ok { counter; flag }

let run : valid state -> unit = fun { counter; _ } ->
  let _ = print_string "counter=" in
  let _ = print_int counter in
  let _ = print_newline () in
  ()

let start () =
  let empty = make () in
  match validate empty with
  | Ok s -> run s
  | Error err -> print_string err
