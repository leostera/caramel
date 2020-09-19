type poly = [ `poly_a | `poly_b of int ]

type poly_ext = [ | poly | `poly_c of string ]

type poly_ext_2 = [ | poly | poly_ext | `poly_d ]
