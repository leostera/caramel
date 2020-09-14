
let iff cond t f =
  match cond with
  | true -> t
  | false -> f
