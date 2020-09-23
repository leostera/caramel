% Source code generated with Caramel.
-module(result).
-export_type([result/2]).


-type result(Ok, Err) :: {ok, Ok}
                       | {error, Err}
                       .


