# Architecture

The new Caramel compiler is structured in subpackages that take care of
distinct phases.

To see how these are hooked together, have a look at runner.ml

```
[ Caramel Source Language ]
	|
	| pkg: Caramel.Syntax
	|
[ Caramel Parsetree ]
	|
	| pkg: Caramel.Typing
	|
[ OCaml Lambda + OCaml Typedtree ]
	|
	| pkg: Caramel.Sugarcane
	|
[ Caramel IR ]--------> pkg: Caramel.Caminito
	|					˄                       ˅
	|         ╰───────────────────────╯
	|
	| pkg: Caramel.Rio
	˅
[ Erlang Code ]
```


