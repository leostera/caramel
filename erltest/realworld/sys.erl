% Source code generated with Caramel.
-module(sys).
-export_type([backend_type/0]).

-export([backend_type/1]).

-type backend_type() :: native
                      | bytecode
                      | {other, string()}
                      .

backend_type() -> {other, <<"BEAM">>}.


