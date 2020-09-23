type valid
type invalid

type 'a state = { counter: int; flag: bool }

let empty : 'a state = { counter = 0; flag = false }

let make : unit -> invalid state = fun () -> empty

let validate : invalid state -> (valid state, string) result =
  fun { counter; flag } -> Ok { counter; flag }

let run : valid state -> unit = fun { counter; _ } ->
  print_string "counter=";
  print_int counter;
  print_newline ();
  ()

let _ = (make ())
  |> validate
  |> (function
  | Ok s -> run s
  | Error err -> print_string err)
