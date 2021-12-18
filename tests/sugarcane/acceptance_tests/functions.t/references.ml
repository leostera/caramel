let operator _ =
  Lists.foldl ( + ) 1 [1; 2; 3]

let transorm _ =
  let transforms = [ Binary.first; Binary.last ] in
  Lists.map (fun g -> g "Hello World") transforms
