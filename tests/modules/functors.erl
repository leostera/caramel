% Source code generated with Caramel.
-module(functors).

-export([run/0]).

run() ->
  Zero = empty(),
  One = concat(Zero, 1),
  Two = intadd:'<+>'(One, 1),
  Two.


