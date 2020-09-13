% Source code generated with Caramel.
-module(simple_type).

-export_type([axis/0]).
-export_type([point/0]).

-type point() :: #{ x :: any()
, y :: any()
}.
-type axis() :: x 
| y
| z
.

