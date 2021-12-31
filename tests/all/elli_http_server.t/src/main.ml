open Erlang

let not_found () =
  Elli.reply 404 []
    {|<!doctype html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Not Found</title>
  </head>
  <body>
    <h1>Not Found</h1>
  </body>
</html>|}

let handle req args =
  let meth = Elli.Request.method_ req in
  let path = Elli.Request.path req in
  Io.format "Handling ~p\n" [ (meth, path) ];
  match (meth, path) with
  | Elli.GET, [ "hello"; you ] -> Elli.reply 200 [] ("Hello, " ^ you ^ "!")
  | _ -> not_found ()

let handle_event _event _data _args = `ok
