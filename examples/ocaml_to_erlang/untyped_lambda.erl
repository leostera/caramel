% Source code generated with Caramel.
-module(untyped_lambda).
-export_type([binding/0]).
-export_type([ctx/0]).
-export_type([expr/0]).
-export_type([info/0]).
-export_type([t/0]).

-export([fresh_name/2]).
-export([index_to_name/2]).
-export([is_bound/2]).
-export([pp_term/2]).
-export([sample/0]).
-export([with_info/1]).

-type info() :: #{ line => integer()
                 , col => integer()
                 }.

-type expr() :: {var, integer(), integer()}
              | {abs, binary(), t()}
              | {app, t(), t()}
              .

-type t() :: #{ expr => expr()
              , info => info()
              }.

-type binding() :: name_bind
                 .

-type ctx() :: list({binary(), binding()}).

-spec with_info(expr()) -> t().
with_info(T) ->
  #{ expr => T
   , info => #{ line => 0
 , col => 0
 }
   }.

-spec is_bound(ctx(), binary()) -> boolean().
is_bound(Ctx, X) ->
  case Ctx of
    [] -> false;
    [{Y, _} | Rest] -> case erlang:'=:='(X, Y) of
  true -> true;
  false -> is_bound(Rest, X)
end
  end.

-spec fresh_name(ctx(), binary()) -> {list({binary(), binding()}), binary()}.
fresh_name(Ctx, X) ->
  case is_bound(Ctx, X) of
    true -> fresh_name(Ctx, << X/binary, <<"'">>/binary >>);
    false -> {[{X, name_bind} | Ctx], X}
  end.

-spec index_to_name(ctx(), integer()) -> option:t(binary()).
index_to_name(Ctx, N) ->
  case Ctx of
    [] -> none;
    [{X, _} | Xs] -> case erlang:'=:='(N, 0) of
  true -> {some, X};
  false -> index_to_name(Xs, erlang:'-'(N, 1))
end
  end.

-spec pp_term(ctx(), t()) -> ok.
pp_term(Ctx, #{ expr := Expr, info := Info }) ->
  case Expr of
    {abs, Name, Term} -> {Ctx2, Name2} = fresh_name(Ctx, Name),
io:format(<<"(lambda ~p.\n\t">>, [Name2 | []]),
pp_term(Ctx, Term),
io:format(<<")">>, []);
    {app, Fn, Arg} -> io:format(<<"(">>, []),
pp_term(Ctx, Fn),
pp_term(Ctx, Arg),
io:format(<<")">>, []);
    {var, X, N} -> case erlang:'=:='(erlang:length(Ctx), N) of
  true -> case index_to_name(Ctx, X) of
  {some, Y} -> io:format(<<"~p">>, [Y | []]);
  none -> io:format(<<"[name was missing: ~p]">>, [X | []])
end;
  false -> io:format(<<"[bad index: ~p]">>, [N | []])
end
  end.

-spec sample() -> t().
sample() -> with_info({abs, <<"sample">>, with_info({app, with_info({abs, <<"f">>, with_info({var, 0, 0})}), with_info({var, 0, 1})})}).


