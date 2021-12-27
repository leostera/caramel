# Simple HTTP Server with Elli

To run this test end to end, you will need:

* erlang
* rebar3
* caramel

And run 2 terminals. In the first one you can run the server with this commands:

```sh
./compile.sh
rebar3 shell
```

In the second one you can use curl to talk to the server:

```sh
$ curl http://localhost:2112/hello/joe
Hello, joe!
```
