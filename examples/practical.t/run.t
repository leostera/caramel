  $ caramel compile gen_tcp_types.ml gen_tcp.ml echo.ml
  File "echo.ml", lines 6-9, characters 2-13:
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  File "echo.ml", lines 13-14, characters 6-17:
  Warning 8: this pattern-matching is not exhaustive.
  Here is an example of a case that is not matched:
  Error _
  Compiling gen_tcp_types.erl	OK
  Compiling echo.erl	OK
  $ cat gen_tcp.erl
  cat: gen_tcp.erl: No such file or directory
  [1]
  $ cat gen_tcp_types.erl
  % Source code generated with Caramel.
  -module(gen_tcp_types).
  -export_type([connection/0]).
  -export_type([listen_opt/0]).
  -export_type([packet/0]).
  -export_type([socket/0]).
  
  
  -opaque socket() :: reference().
  
  -opaque connection() :: reference().
  
  -type packet() :: http
                  | line
                  .
  
  -type listen_opt() :: {active, boolean()}
                      | {packet, packet()}
                      .
  
  
  $ cat echo.erl
  % Source code generated with Caramel.
  -module(echo).
  
  -export([handle/1]).
  -export([loop/1]).
  -export([start/1]).
  
  -spec handle(gen_tcp_types:connection()) -> ok.
  handle(Conn) ->
    gen_tcp:send(Conn, <<"hello world">>),
    gen_tcp:close(Conn).
  
  -spec loop(gen_tcp_types:socket()) -> _.
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
  
  
