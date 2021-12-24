let unit () = ()

let tuple2 () = (1, unit ())

let tuple3 () = (1, tuple2 (), 3)

let tuple4 () = (1, 2, tuple3 (), 4)

let tuple5 () = (1, 2, 3, tuple4 (), 5)
