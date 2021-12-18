let unit () = ()
let tuple2 () = (1, 2)
let tuple3 () = (1, 2, 3)
let tuple4 () = (1, 2, 3, 4)
let tuple5 () = (1, 2, 3, 4, 5)

let polyvar0 () = `what
let polyvar1 () = `what_ 1
let polyvar2 () = `what_ (1, true)

type r0 = { _0: int }
type r1 = { _0: int; _1: string }
type r2 = { _0: int; _1: string; _2 : bool }
let r0 () = { _0 = 1 }
let r1 () = { _0 = 1; _1 = "record" }
let r2 () = { _0 = 1; _1 = "record"; _2 = true }

let f0 () = { _0 = 1 }._0
let f1 () = (r0 ())._0
let f2 r = r._0
let f3 { _2; _}  = _2

type v = | V | V1 of int | V2 of int * int | VR of {field:bool}
let v0 () = V
let v1 () = V1 1
let v2 () = V2 (1, 2)
let vr () = VR { field = false}

type gadt = | Hello: string -> gadt
let gadt () = Hello "what"
