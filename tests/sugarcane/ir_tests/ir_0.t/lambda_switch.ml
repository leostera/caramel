let switch0 x =
  match x with
  | "joe" -> 1
  | "mike" -> 2
  | "robert" -> 3
  | "xavier" -> 4
  | _ -> 5

let switch1 x =
  match x with
  | 0 -> "zero"
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | _ -> "other"

type v = V | V1 of int | V2 of int * int | V3 of int * int * int

let switch2 x =
  match x with V -> 0 | V1 i -> i | V2 (_, i) -> i | V3 (_, _, i) -> i

(* NOTE: this is optimized as a field access since we're accessing the first
   value of every variant *)
let switch3 x =
  match x with V -> 0 | V1 i -> i | V2 (i, _) -> i | V3 (i, _, _) -> i

type r0 = { _0 : int; _1 : bool; _2 : string }

let switch4 x =
  match x with
  | { _0 = 1; _ } -> 1
  | { _1 = true; _ } -> 2
  | { _2 = "hello"; _ } -> 3
  | _ -> 0

type gadt = Hello : string * (string -> unit) -> gadt

let switch5 x = match x with Hello (s, fn) -> fn s
