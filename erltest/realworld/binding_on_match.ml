
let rec f x =
  function
  | [] -> []
  | a :: b -> x a :: (f x b)

