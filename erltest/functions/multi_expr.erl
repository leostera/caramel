% Source code generated with Caramel.
-module(multi_expr).


-export([hello/1]).

hello({}) ->
  Text = <<hello, joe!>>,
  print_string(Text).


