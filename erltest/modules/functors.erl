% Source code generated with Caramel.
-module(functors).

-export([run/1]).

run() ->
  Zero = empty(),
  One = concat(Zero, 1),
  Two = <+>(One, 1),
  Two.


