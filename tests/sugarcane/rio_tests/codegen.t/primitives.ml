let list0 () = []

let list1 () = [ 1; 2 ]

let list2 () = [ [ 1 ]; [ 2 ] ]

let list3 () = [ list2 (); [ list1 () ] ]

let nested () =
  let nested1 () = list0 () in
  let nested2 () = [ list2 (); [ nested1 () ] ] in
  [ nested2 () ]

let unit () = ()

let tuple2 () = (1, unit ())

let tuple3 () = (1, tuple2 (), 3)

let tuple4 () = (1, 2, tuple3 (), 4)

let tuple5 () = (1, 2, 3, tuple4 (), 5)

let polyvar0 () = `poly

let polyvar1 () = `what1 (polyvar0 ())

let polyvar2 () = `what2 (`poly, polyvar1 ())

type r0 = { _0 : int }

type r1 = { _0 : int; _1 : r0 }

type r2 = { _0 : int; _1 : string; _2 : r1 }

let r0 () = { _0 = 1 }

let r1 () = { _0 = 1; _1 = r0 () }

let r2 () = { _0 = 1; _1 = "record"; _2 = r1 () }

let f0 () = { _0 = 1 }._0

let f1 () = (r0 ())._0

let f2 r = r._0

let f3 { _2; _ } = _2

type v = V | V1 of int | V2 of int * int | VR of { field : bool }

let v0 () = V

let v1 () = V1 1

let v2 () = V2 (1, 2)

let vr () = VR { field = false }

type gadt = Hello : string -> gadt

let gadt () = Hello "what"
