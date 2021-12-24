let list0 () = []

let list1 () = [ 1; 2 ]

let list2 () = [ [ 1 ]; [ 2 ] ]

let nested () =
  let nested1 () = list0 () in
  let nested2 () = [ list2 (); [ nested1 () ] ] in
  [ nested2 () ]
