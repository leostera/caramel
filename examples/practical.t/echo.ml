let handle conn =
  Gen_tcp.send conn "hello world";
  Gen_tcp.close conn

let rec loop socket =
  let (Ok conn) = Gen_tcp.accept socket in
  let handler = Process.make (fun _self _recv -> handle conn) in
  Gen_tcp.controlling_process conn handler;
  loop socket

let start port =
  Process.make (fun _self _recv ->
      let (Ok socket) = Gen_tcp.listen port [ Active false; Packet Http ] in
      loop socket)
