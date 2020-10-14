let initial_agenda () = [
  ("erlang", `programming_language, `origin "swedish");
  ("ocaml", `programming_language, `origin "french");
  ("elixir", `programming_language, `origin "brazilian");
]

let run () =
  let ia = initial_agenda () in
  let names = Lists.map (fun (name, _, _) -> name ) ia in
  let origins = Lists.map (fun (_, _, `origin origin) -> origin ) ia in
  Lists.zip names origins
