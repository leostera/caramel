  $ caramel compile *.ml *.mli
  Compiling result.erl	OK
  Compiling option.erl	OK
  Unbound type constructor Erlang.process
  [1]
  $ cat *.erl
  % Source code generated with Caramel.
  -module(option).
  -export_type([t/1]).
  
  
  -type t(A) :: none
              | {some, A}
              .
  
  
  % Source code generated with Caramel.
  -module(result).
  -export_type([result/2]).
  
  
  -type result(Ok, Err) :: {ok, Ok}
                         | {error, Err}
                         .
  
  

