  $ caramel compile *.ml *.mli
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
  
  

