  $ cat >recv.erl <<EOF
  > -module(recv).
  > -export([recv/0]).
  > recv() ->
  >   receive
  >     {int, I} -> print_int(I);
  >     {str, B} -> print_string(B)
  >   end.
  > EOF
  $ caramelc compile --dump-ast recv.erl
  noop
  $ cat recv.ml
