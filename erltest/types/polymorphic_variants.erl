% Source code generated with Caramel.
-module(polymorphic_variants).
-export_type([plain/0]).
-export_type([poly/1]).
-export_type([poly_ext/1]).
-export_type([poly_ext_fixed/0]).
-export_type([poly_ext_phantom/1]).


-type plain() :: poly_zero
               .

-type poly(A) :: plain()
               | {poly_a, A}
               | {poly_b, integer()}
               .

-type poly_ext(A) :: poly(A)
                   | {poly_c, string()}
                   .

-type poly_ext_fixed() :: poly(integer())
                        | {poly_c, string()}
                        .

-type poly_ext_phantom(_A) :: poly(integer())
                            | {poly_c, string()}
                            .


