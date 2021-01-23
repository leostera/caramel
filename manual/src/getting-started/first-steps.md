# First Steps

This section teaches you some of the fundamentals of Caramel.

> *NOTE*: this section is using a standard library that mostly reflects how
> Erlang works. We are working on a Standard Library that feels more idiomatic
> to Caramel.

It assumes you have some prior knowledge of functional programming, like
Erlang or Elixir, and some knowledge of static typing, like from TypeScript
or Java.

**Steps**
* [Hello World](#hello-world)
* [Say Hello](#say-hello)
* [Working with Types](#working-with-types)
* Custom Types
* Processes and Message Passing

### Prerequisites

You will need a working Erlang/OTP installation. You can get one from your
package manager:

```
# For Homebrew on OS X
brew install erlang

# For Ubuntu and Debian
apt-get install erlang

# For Fedora
yum install erlang

# For FreeBSD
pkg install erlang
```

## Hello World

Caramel is a language that relies on the Erlang VM, just like Elixir or
Gleam. This means that we need to compile our code, and run it from within
Erlang.

Let's write a Hello World example:

```ocaml
(* file: hello_world.ml *)
{{#include ../../../examples/manual.t/Fs0_hello_world.ml}}
```

We can compile it as follows:

```bash
$ caramel compile hello_world.ml
Compiling hello_world.erl OK
```

And run it with `escript`:

```
$ escript hello_world.erl
Hello World
```

`escript` is an Erlang utility to run an Erlang program as a script. It
defaults to running the `main/1` function of a module, and it passes all
command line arguments to it as a list of strings.

We can also load and run this module from a regular Erlang shell.

```
$ erl
Erlang/OTP 23 [erts-11.1.5] [source] [64-bit] [smp:64:64] [ds:64:64:10]
[async-threads:1] [hipe]

Eshell V11.1.5  (abort with ^G)
1> c(hello_world).
{ok,hello_world}
2> hello_world:main([]).
Hello World
ok
```

## Say Hello!

For our next example, we will take a list of names and will say hello to each of them.

```ocaml
(* file: say_hello.ml *)

{{#include ../../../examples/manual.t/Fs1_say_hello.ml}}
```

Lets run this program now:

```
$ escript say_hello.erl Joe Robert Mike
Hello, Joe!
Hello, Robert!
Hello, Mike!
```

## Working with Types

We said before that Caramel is a typed language, but we haven't seen any
types at work so far. How is this possible?

Caramel is a strict subset of OCaml and it has excellent type-inference. This
means that Caramel has **figured out the types for us**.

> **NOTE**: type inference is great, but sometimes it makes for less clear code
> than it should, consider annotating for clarity whenever some things start
> getting complex!

Let's write a program now that will add together 2 numbers, ignoring their
decimals.

Our test cases will be:

* `10 + 2 = 12`
* `10 + 2.7 = 12`

```ocaml
(* file: calc_add.ml *)
{{#include ../../../examples/manual.t/fs2_calc_add.ml}}
```

We can call it and see if it works:

```bash
$ escript calc_add.erl 10 2
10 + 2 = 12
```

But what happens when we try to add `10` and `2.7`?

```bash
λ escript calc_add.erl 10 2.7
escript: exception error: bad argument
  in function  list_to_integer/1
     called as list_to_integer("2.7")
  in call from erl_eval:do_apply/6 (erl_eval.erl, line 680)
  in call from erl_eval:expr/5 (erl_eval.erl, line 449)
  in call from escript:eval_exprs/5 (escript.erl, line 872)
  in call from erl_eval:local_func/6 (erl_eval.erl, line 567)
  in call from escript:interpret/4 (escript.erl, line 788)
  in call from escript:start/1 (escript.erl, line 277)
  in call from init:start_em/1
```

The `Erlang.list_to_integer/1` function tries to parse a string into an
Integer, not a Float. Since Erlang makes a distinction between those at this
level, we have to use another function for this: `Erlang.list_to_float/1`.

Lets change our second value to be parsed as a float:

```ocaml
(* ... *)
let b_int = Erlang.list_to_float b in
(* ... *)
```

When we try to compile, Caramel will show us an error message!


```bash
$ caramel compile calc_add.ml
File "fs3_calc_add_tagged.ml", line 8, characters 23-28:
Error: This expression has type float but an expression was expected of type
         int
```

Caramel has figured out that you are trying to add a float and an integer together
and its telling you that it can't _just do that_. 

> **NOTE**: Errors at the moment have the exact same copy and information as
> errors from the OCaml compiler. Over time this will be changed.


## Custom Types

## Processes and Message Passing
