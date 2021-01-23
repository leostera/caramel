# caramel

```
NAME
       caramel - Caramel compiler

SYNOPSIS
       caramel COMMAND ...

DESCRIPTION
       Caramel is a functional language for building type-safe, scalable, and
       maintainable applications.

COMMANDS
       compile
           Compile Caramel code to run on the Erlang VM.

       fmt Format Caramel code.

       parse
           (UNSTABLE) Helper command to parse sources and dump ASTs

       sort-deps
           Sort OCaml files by their dependencies on each other.

       version
           Show version information.

OPTIONS
       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --version
           Show version information.

SEE ALSO
       ocaml(1) erlang

AUTHORS
       Leandro Ostera.

LICENSE
       Copyright (C) 2020-present, Abstract Machines Lab Sweden AB

       Caramel is licensed under Apache License 2.0
```
