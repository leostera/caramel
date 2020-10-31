  $ caramelc compile gen_tcp.ml echo.ml
  File "echo.ml", lines 6-9, characters 2-13:
  6 | ..let Ok conn = Gen_tcp.accept socket in
  7 |   let handler = Process.make (fun _self _recv -> handle conn ) in
  8 |   Gen_tcp.controlling_process conn handler;
  9 |   loop socket
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  File "echo.ml", lines 12-13, characters 2-13:
  12 | ..let Ok socket = Gen_tcp.listen port [Active false; Packet Http] in
  13 |   loop socket.
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  Compiling gen_tcp.erl	OK
  Compiling echo.erl	OK
  $ cat echo.erl
  % Source code generated with Caramel.
  -module(echo).
  
  -export([handle/1]).
  -export([loop/1]).
  -export([start/1]).
  
  -spec handle(gen_tcp:connection()) -> ok.
  handle(Conn) ->
    gen_tcp:send(Conn, <<"hello world">>),
    gen_tcp:close(Conn).
  
  -spec loop(gen_tcp:socket()) -> any().
  loop(Socket) ->
    case gen_tcp:accept(Socket) of
      {ok, Conn} -> Handler = process:make(fun
    (_self, _recv) -> handle(Conn)
  end),
  gen_tcp:controlling_process(Conn, Handler),
  loop(Socket)
    end.
  
  -spec start(integer()) -> erlang:pid().
  start(Port) -> process:make(fun
    (_self, _recv) ->
    case gen_tcp:listen(Port, [{active, false} | [{packet, http} | []]]) of
      {ok, Socket} -> loop(Socket)
    end
  end).
  
  
