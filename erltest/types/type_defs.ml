type opaque = string

type hidden

type 'a list =
  | Cons of ('a * 'a list)
  | Nil

type 'a option =
  | Some of 'a
  | None

type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type ('a, 'b) triplet = 'a * 'a * 'b

type 'a phantom = Phantom

type poly = [ `Poly_one | `Poly_two of int ]

type record = { a: string; b: int }

type inlined_record =
  | Compound of { ir_a: float; ir_b: bool }

type compound = {
  c_a: inlined_record ;
  c_b: bool phantom ;
  c_c: (int, bool) triplet ;
  c_d: record ;
}
