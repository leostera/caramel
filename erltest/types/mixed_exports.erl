% Source code generated with Caramel.
-module(mixed_exports).

-export_type([opaque/0]).

-export([id/1]).

-type opaque() :: string().

-type hidden() :: reference().

id(X) -> X.

secret({}) -> {}.


