  $ cat a.ml
  (* ocaml module *)
  let f x = x
  
  $ caramelc compile a.ml
  Compiling a.erl	OK
  $ cat a.erl
  % Source code generated with Caramel.
  -module(a).
  
  -export([f/1]).
  
  -spec f(any()) -> any().
  f(X) -> X.
  
  
  $ caramelc check a.erl
  $ caramelc verify a.erl a.ml
