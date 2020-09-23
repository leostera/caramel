% Source code generated with Caramel.
-module(opaque_and_hidden).
-export_type([opaque/0]).
-export_type([visible/0]).


-type visible() :: integer().

-opaque opaque() :: string().

-opaque hidden() :: reference().


