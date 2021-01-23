# caramel fmt

```
NAME
       caramel-fmt - Format Caramel code.

SYNOPSIS
       caramel fmt [OPTION]... SOURCES...

DESCRIPTION
        Reformats Caramel source code. 

ARGUMENTS
       SOURCES (required)
           A list of source files to format

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
