let initial_agenda () = [
  ("erlang", `programming_language, `origin "swedish");
  ("ocaml", `programming_language, `origin "french");
  ("elixir", `programming_language, `origin "brazilian");
]

let run () =
  let ia = initial_agenda () in
  let origin_names = Lists.map (fun (n, _, `origin o) -> o, n) ia in
  let m = Maps.merge (Maps.empty ()) (Maps.from_list origin_names) in
  m
