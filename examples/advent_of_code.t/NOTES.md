
This:

```ocaml
let run_days days = Lists.foreach run_one days
```

should generate this:

```erlang
run_days(Days) -> lists:foreach(fun runner:run_one/1, Days).
```
