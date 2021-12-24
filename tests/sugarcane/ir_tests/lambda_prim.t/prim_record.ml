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
