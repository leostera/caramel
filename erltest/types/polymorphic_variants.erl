% Source code generated with Caramel.
-module(polymorphic_variants).
-export_type([poly/0]).
-export_type([poly_ext/0]).
-export_type([poly_ext_2/0]).


-type poly() :: poly_a
              | {poly_b, integer()}
              .

-type poly_ext() :: poly()
                  | {poly_c, string()}
                  .

-type poly_ext_2() :: poly()
                    | poly_ext()
                    | poly_d
                    .


