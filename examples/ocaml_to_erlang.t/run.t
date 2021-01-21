  $ caramel compile untyped_lambda.ml
  Compiling untyped_lambda.erl	OK
  $ cat untyped_lambda.erl
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
      true -> fresh_name(Ctx, caramel_runtime:binary_concat(X, <<"'">>));
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
  
  
  $ caramel compile simple_types.ml
  Compiling simple_types.erl	OK
  $ cat simple_types.erl
  % Source code generated with Caramel.
  -module(simple_types).
  -export_type([binding/0]).
  -export_type([ctx/0]).
  -export_type([info/0]).
  -export_type([result/2]).
  -export_type([t/0]).
  -export_type([type_/0]).
  
  -export([bind/3]).
  -export([find_binding/2]).
  -export([run/0]).
  -export([t_to_str/1]).
  -export([type_of/2]).
  
  -type result(Ok, Err) :: {ok, Ok}
                         | {error, Err}
                         .
  
  -type info() :: ok.
  
  -type type_() :: {type_arrow, type_(), type_()}
                 | type_bool
                 .
  
  -type t() :: {term_var, info(), integer(), integer()}
             | {term_abs, info(), binary(), type_(), t()}
             | {term_app, info(), t(), t()}
             | {term_true, info()}
             | {term_false, info()}
             | {term_if, info(), t(), t(), t()}
             .
  
  -type binding() :: name_bind
                   | {var_bind, type_()}
                   .
  
  -type ctx() :: list({binary(), binding()}).
  
  -spec t_to_str(type_()) -> binary().
  t_to_str(Type_) ->
    case Type_ of
      {type_arrow, Ta, Tr} -> Ta2 = t_to_str(Ta),
  Tr2 = t_to_str(Tr),
  caramel_runtime:binary_concat(<<"(lambda ">>, caramel_runtime:binary_concat(Ta2, caramel_runtime:binary_concat(<<" -> ">>, caramel_runtime:binary_concat(Tr2, <<")">>))));
      type_bool -> <<"bool">>
    end.
  
  -spec bind(ctx(), binary(), binding()) -> ctx().
  bind(Ctx, Name, Binding) -> [{Name, Binding} | Ctx].
  
  -spec find_binding(ctx(), integer()) -> option:t(binding()).
  find_binding(Ctx, I) ->
    case Ctx of
      [] -> none;
      [{_x, B} | Rest] -> case erlang:'=:='(I, 0) of
    true -> {some, B};
    false -> find_binding(Rest, erlang:'-'(I, 1))
  end
    end.
  
  -spec type_of(ctx(), t()) -> result:t(type_(), binary()).
  type_of(Ctx, Term) ->
    case Term of
      {term_var, _info, I, _} -> case find_binding(Ctx, I) of
    none -> {error, <<"Unbound_name">>};
    {some, {var_bind, T}} -> {ok, T};
    {some, name_bind} -> {error, <<"Untyped_binding">>}
  end;
      {term_abs, _info, Name, Arg_type, Ret_type} -> Ctx2 = bind(Ctx, Name, {var_bind, Arg_type}),
  case type_of(Ctx2, Ret_type) of
    {ok, Ret_type2} -> {ok, {type_arrow, Arg_type, Ret_type2}};
    Err -> Err
  end;
      {term_app, _info, Fn, Arg} -> case {type_of(Ctx, Fn), type_of(Ctx, Arg)} of
    {{ok, {type_arrow, Arr_t, Ret_t}}, {ok, Arg_t}} -> case erlang:'=:='(Arr_t, Arg_t) of
    true -> {ok, Ret_t};
    false -> A = t_to_str(Arr_t),
  B = t_to_str(Arg_t),
  {error, caramel_runtime:binary_concat(<<"Expected parameter to be of type:  ">>, caramel_runtime:binary_concat(A, caramel_runtime:binary_concat(<<" but instead found: ">>, B)))}
  end;
    {{ok, _}, _} -> {error, <<"Expected_arrow_type">>};
    {Err, _} -> Err
  end;
      {term_false, _} -> {ok, type_bool};
      {term_true, _} -> {ok, type_bool};
      {term_if, _, T_if, T_then, T_else} -> case {type_of(Ctx, T_if), type_of(Ctx, T_then), type_of(Ctx, T_else)} of
    {{ok, type_bool}, {ok, A}, {ok, B}} -> case erlang:'=:='(A, B) of
    true -> {ok, A};
    false -> {error, <<"then and else branches must have the same type">>}
  end;
    {_, _, _} -> {error, <<"condition must be boolean">>}
  end
    end.
  
  -spec run() -> ok.
  run() ->
    Print_result = fun
    (R) ->
    case R of
      {error, Str} -> io:format(<<"ERROR: ~s\n\n">>, [Str | []]);
      _ -> io:format(<<"~p\n\n">>, [R | []])
    end
  end,
    F = {term_abs, ok, <<"f">>, {type_arrow, type_bool, type_bool}, {term_true, ok}},
    App1 = {term_app, ok, F, F},
    App2 = {term_app, ok, F, {term_abs, ok, <<"g">>, type_bool, {term_true, ok}}},
    App3 = {term_var, ok, 0, 0},
    Ctx = [{<<"">>, {var_bind, type_bool}} | []],
    Print_result(type_of([], App1)),
    Print_result(type_of([], App2)),
    Print_result(type_of([], App3)),
    Print_result(type_of(Ctx, App3)).
  
  

  $ caramel compile using_lists.ml
  Compiling using_lists.erl	OK
  $ cat using_lists.erl
  % Source code generated with Caramel.
  -module(using_lists).
  
  -export([initial_agenda/0]).
  -export([run/0]).
  
  -spec initial_agenda() -> list({binary(), programming_language
              , {origin, binary()}
              }).
  initial_agenda() -> [{<<"erlang">>, programming_language, {origin, <<"swedish">>}} | [{<<"ocaml">>, programming_language, {origin, <<"french">>}} | [{<<"elixir">>, programming_language, {origin, <<"brazilian">>}} | []]]].
  
  -spec run() -> beam__maps:t(binary(), binary()).
  run() ->
    Ia = initial_agenda(),
    Origin_names = lists:map(fun
    ({N, _, {origin, O}}) -> {O, N}
  end, Ia),
    M = maps:merge(maps:new(), maps:from_list(Origin_names)),
    M.
  
  
