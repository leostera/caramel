type alias = int

type opaque = string list

type hidden

type 'a list =
  | Cons of ('a * 'a list)
  | Nil

type 'value tree =
  | Leaf of 'value
  | Node of 'value * 'value tree * 'value

type 'a option =
  | Some of 'a
  | None

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type ('a, 'b) triplet = 'a * 'a * 'b

type 'a phantom = Phantom | Phantom_with_value of int

type record = { a: string; b: int }

type inlined_record =
  | Simpler of bool
  | Many of bool * bool
  | Compound of { ir_a: float; ir_b: bool }

type compound = {
  c_a: inlined_record ;
  c_b: bool phantom ;
  c_c: (int, bool) triplet ;
  c_d: record ;
}

type poly = [ `Poly_one | `Poly_two of int ]

