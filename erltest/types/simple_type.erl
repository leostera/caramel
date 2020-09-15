% Source code generated with Caramel.
-module(simple_type).

-export_type([axis/0]).
-export_type([p3d/0]).
-export_type([point/0]).


-type p3d() :: {integer(), integer(), integer()}.

-type point() :: #{ x => integer()
                  , y => integer()
                  }.

-type axis() :: x
              | y
              | z
              .


