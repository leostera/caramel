-module(constants).

f() ->
  %% Integers
  _ = 1,

  %% Floats
  _ = 1.0,

  %% Binaries
  _ = <<"hello">>,

  %% Strings
  _ = "hello",

  %% IO Lists
  _ = ["hello", ["world"]],

  %% Chars
  _ = 'c',

  %% atoms
  _ = hello,
  _ = 'Hello_!world.1'.
