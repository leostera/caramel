let fmt () =
  let str = "Hello" in
  Io.format "~p" [str];
  let ints = 1 in
  Io.format "~p" [ints]
