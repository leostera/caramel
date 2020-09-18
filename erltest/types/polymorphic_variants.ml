type poly = [ `poly_a | `poly_b of int ]

(* FIXME: poly_ext did not include the constructors from poly *)
type poly_ext = [ | poly | `poly_c of string ]
