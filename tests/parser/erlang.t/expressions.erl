-module(expressions).

f() ->
  _ = (A)(),
  A = ((3 * (1 + 2)) ! g()),

  _ = (f()),

  %% Let bindings
  A = 1,
  B = A,
  _ = C,

  %% Using variables
  _ = f(A),

  %% Function references
  A = fun f/0,
  A = fun expressions:f/0,
  A = fun A:f/0,
  A = fun A:B/0,
  A = fun expressions:B/0,

  %% Lambdas
  A = fun () -> ok end,
  A = fun (B) -> B end,

  %% application
  _ = A(),
  _ = f(),
  _ = expressions:f(),
  _ = A:f(),
  _ = A:B(),
  _ = expressions:B(),

  %% Map expressions
  A = #{ a => 1, b => #{ c => d } },
  B = A#{ b => 2 },

  %% Record expressions
  % A = #record{ a=1 },
  % B = A#record{ a=1 },

  %% List expressions
  A = [],
  A = [1,2,3],
  A = [1|[2|[3|[]]]],

  %% Tuple expressions
  A = {},
  A = {1},
  A = {1, {2}},

  %% Try catch after
  _ = try A
      catch
        throw:Reason -> ok;
        C:R:S -> ok
      after
        ok
      end,

  %% Catch & throw
  A = catch B,
  A = throw(B),

  _ = case A of
        _ when is_number(B) -> ok
      end,

  %% Receive expression
  _ = receive
        _ -> ok
      after
        1000 -> ok
      end,

  ok.
