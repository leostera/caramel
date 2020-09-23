# Notes 

## maindriver.ml

When passing `-o` will call `Compenv.process_deferred_actions` with Some output name

when calling `to_bytecode` we already get the typed tree!

so we can just hack into the `implementation` function and turn that into erlang :smirk:

---

# ROADMAP

## Done

* Small example `math.ml` compiled after type-checking is successful down to `math.erl`

* Map out preliminary Erlang AST (borrow structure from Lumen) 

* Structure into driver + ast + printer + conversion

## Upcoming

* Convert typedtree into erlang ast

* Print erlang ast 


---

# Differences from OCaml

No `ref` and no `mutable`.

# Differences from Erlang

It compiles to a strict subset of Erlang, so everything should look & feel
familiar here.
