% Source code generated with Caramel.
-module(simple_type).

-export_type([axis/0]).
-export_type([p3d/0]).
-export_type([point/0]).


-type p3d() :: {int(), int(), int()}.

-type point() :: #{ x => int()
                  , y => int()
                  }.

-type axis() :: x
              | y
              | z
              .


