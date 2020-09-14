% Source code generated with Caramel.
-module(mixed_exports).

-export_type([opaque/0]).

-export([id/1]).

-type opaque() :: string().

-type hidden() :: ref().

id(X) -> X.

secret({}) -> {}.


