let f _arg0 _arg1 =
  let lambda = fun x1 _x2 _x3 -> x1 in
  lambda ()

let g _arg0 = f ()
