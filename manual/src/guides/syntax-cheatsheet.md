# Syntax Cheatsheet

Here is a comparison between common Erlang and Elixir syntaxes and the OCaml
and Reason syntaxes that are supported by Caramel.

## Types

### Variants and Unions

**Erlang**:
```erlang
-type bool() :: true | false.
-type foo() :: bar() | baz().
```

**Elixir**
```elixir
@type bool() :: true | false
@type foo() :: bar() | baz()
```

**OCaml**

```ocaml
type bool = True | False
type foo = Bar of bar | Baz of baz
type foo = [ `Bar of bar | `Baz of baz ]
type foo = [ bar | baz ] (* for when bar and baz are polymorphic variants *)
```

### Records

**Erlang**:
```erlang
-type pair(A, B) :: #{ fst => A, snd => B }.
```

**Elixir**
```elixir
@type pair(a, b) :: %{ :fst => a(), :snd => b() }
```

**OCaml**

```ocaml
type ('a, 'b) pair = { fst : 'a; snd : 'b }
```

## Expressions and Values

### Atoms

| Erlang | Elixir | OCaml | Reason | Description |
| --- | --- | --- | --- | --- | 
| `ok` | `:ok` | <code>`ok</code> | <code>`ok</code> | Atoms in Caramel are treated as polymorphic variants. |
| `'ok'` | `:'ok'` | --- | --- | Quoted atoms are unsupported in Caramel. |

### Comments

| Erlang | Elixir | OCaml | Reason |
| --- | --- | --- | --- |
| `% comment` | `# comment` | `(* comment *)` | `// comment` |

### Variables

| Erlang | Elixir | OCaml | Reason |
| --- | --- | --- | --- |
| `A = 1` | `a = 1` | `let a = 1` | `let a = 1` |
| `A2 = A + 1` | `a = 1` | `let a' = a + 1` | `let a' = a + 1` |

### Binary Strings, and Charlists

| Erlang | Elixir | OCaml | Reason |
| --- | --- | --- | --- |
| `<<"binary">>` | `"binary"` | `"binary"` | `"binary"` |
| `"string"` | `'binary'` | `['b'; 'i'; 'n'; 'a'; 'r'; 'y']` | `['b', 'i', 'n', 'a', 'r', 'y']` |

### Function Calls

| Erlang | Elixir | OCaml | Reason |
| --- | --- | --- | --- |
| `Lambda()` | `lambda.()` | `lambda ()` | `lambda()` |
| `local()` | `local()` | `local ()` | `local()` |
| `mod:fn()` | `Mod.fn()` | `Mod.fn ()` | `Mod.fn()` |
| `A:F()` | `apply(A, F, [])` | --- | --- |

Dynamically dispatched calles are not supported in Caramel, because we can't
know the type of the arguments they will have, or the type of the value they
would return.

### If expressions

**Erlang**:
```erlang
if 
  Foo -> Bar;
  _ -> Baz
end.
```

**Elixir**
```elixir
if(foo, do: bar, else: baz)

if foo do
  bar
else 
  baz
end
```

**OCaml**

```ocaml
if foo then bar else baz
```

**Reason**
```reason
foo ? bar : baz
```

### Match / Case / Switch expression

**Erlang**:
```erlang
case Foo of
  Bar -> Baz
end.
```

**Elixir**
```elixir
case foo do
  bar -> baz
end
```

**OCaml**

```ocaml
match foo with
| bar -> baz
```

**Reason**
```reason
switch foo {
| bar => baz
}
```
