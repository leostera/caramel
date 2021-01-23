# caramel compile

```
NAME
       caramel-compile - Compile Caramel code to run on the Erlang VM.

SYNOPSIS
       caramel compile [OPTION]... SOURCES...

DESCRIPTION
        The Caramel takes as input OCaml sources and compiles them to Erlang
       code. 

ARGUMENTS
       SOURCES (required)
           A list of source files to compile

OPTIONS
       -d, --dump-ast
           Use this flag to print out to standard output the ASTs of the
           different representations being used during compilation. This is
           NOT suitable for programmatic usage, and its mostly used for
           debugging the compiler itself.

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --no-stdlib
           Use this flag to compile sources without opening the Standard
           Library by default.

       --stdlib-path=VAL (absent=/home/ostera/.opam/4.11.1/lib/caramel/stdlib
       or CARAMEL_STDLIB_PATH env)

       --version
           Show version information.

ENVIRONMENT
       These environment variables affect the execution of compile:

       CARAMEL_STDLIB_PATH
           See option --stdlib-path.

SEE ALSO
       ocaml(1) erlang

AUTHORS
       Leandro Ostera.

LICENSE
       Copyright (C) 2020-present, Abstract Machines Lab Sweden AB

       Caramel is licensed under Apache License 2.0
```
