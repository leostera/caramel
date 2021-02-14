# parse-wyc (parse what you can)

[![ocaml-ci](https://img.shields.io/badge/ocaml--ci-builds-informational?logo=ocaml&style=for-the-badge)](https://ci.ocamllabs.io/github/gpetiot/parse-wyc/)

Disclaimer: this is highly experimental!

Adapted from the recovery-parser of `merlin`.


## How to use it

The main part of the API of the library is:

```ocaml
module Make_parsable : sig
  val structure : string -> string

  val signature : string -> string

  val use_file : string -> string
end
```

`Make_parsable.structure input` tries to parse the `input` string as a structure node of the AST, if there are invalid parts in the input, they are wrapped inside `[%%invalid.ast.node "..."]` attributes so the output string can successfully be parsed as a structure node by the standard OCaml parser.

For a more fine-grained control on the invalid parts of the input, you can also use:

```ocaml
module Invalid_locations : sig
  val structure : Lexing.lexbuf -> Location.t list

  val signature : Lexing.lexbuf -> Location.t list

  val use_file : Lexing.lexbuf -> Location.t list
end
```

`Invalid_locations.structure` retrieves the list of locations in the input buffer that delimit invalid code parts.
