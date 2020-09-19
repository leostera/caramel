type plain = [ `poly_zero ]

type 'a poly = [ | plain | `poly_a of 'a | `poly_b of int ]

type 'a poly_ext = [ | 'a poly | `poly_c of string ]

type poly_ext_fixed = [ | int poly | `poly_c of string ]

type 'a poly_ext_phantom = [ | int poly | `poly_c of string ]
