# Caramel
> :candy: An Erlang backend to the OCaml compiler

Read more about the OCaml to Erlang compiler here: [`./erlcomp`](./erlcomp).

## Getting started

You can bootstrap the system by running:

```sh
caramel $ ./configure
caramel $ make
caramel $ make caramelc
```

After that you should have a `caramelc` binary to play around with. For the time
being it has the same interface as `ocamlc`, which means that you need to 
pass it in `.ml` and `.mli` files _in the right order_.

You can use `caramelc -depent -sort *.ml *.mli` to get a sorted list or just run
this (assuming `caramelc` is in your `PATH`):

```sh
my_project $ caramelc -c $(caramelc -depend -sort *.ml *.mli)
```

