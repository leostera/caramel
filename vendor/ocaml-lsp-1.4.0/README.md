# OCaml-LSP

[![Build](https://github.com/ocaml/ocaml-lsp/workflows/Build%20and%20Test/badge.svg)](https://github.com/ocaml/ocaml-lsp/actions)

OCaml-LSP is a language server for OCaml that implements [Language Server Protocol](https://microsoft.github.io/language-server-protocol/) (LSP).

This project contains an implementation of a language server for OCaml and a standalone
library implementing LSP.

## Installation

We recommend to install the language server via a package manager such as
[opam](http://github.com/ocaml/opam) or [esy](https://github.com/esy/esy).

### Opam

To install the language server in the currently used opam [switch](https://opam.ocaml.org/doc/Manual.html#Switches):

```sh
$ opam install ocaml-lsp-server
```

*Note:* you will need to install `ocaml-lsp-server` in every switch where you would like
to use it.

### Esy

To add the language server to an esy project, run in terminal:

```
$ esy add @opam/ocaml-lsp-server
```

### Source

This project uses submodules to handle dependencies. This is done so that users
who install `ocaml-lsp-server` into their sandbox will not share dependency constraints on
the same packages that `ocaml-lsp-server` is using.

```sh
$ git clone --recurse-submodules http://github.com/ocaml/ocaml-lsp.git
$ cd ocaml-lsp
$ make all
```

## Usage

Once `ocaml-lsp-server` is installed, the executable is called `ocamllsp`. For
now, the server can only be used through the standard input (`stdin`) and output
(`stdout`) file descriptors.

For an example of usage of the server in a VS Code extension, see
OCaml Platform Extension implementation [here](https://github.com/ocamllabs/vscode-ocaml-platform/blob/master/src/vscode_ocaml_platform.ml).

## Features

The server supports the following LSP requests:

- [x] `textDocument/completion`
- [x] `completionItem/resolve`
- [x] `textdocument/hover`
- [ ] `textDocument/signatureHelp`
- [x] `textDocument/declaration`
- [x] `textDocument/definition`
- [x] `textDocument/typeDefinition`
- [ ] `textDocument/implementation`
- [x] `textDocument/codeLens`
- [x] `textDocument/documentHighlight`
- [x] `textDocument/documentSymbol`
- [x] `textDocument/references`
- [ ] `textDocument/documentColor`
- [ ] `textDocument/colorPresentation`
- [x] `textDocument/formatting`
- [ ] `textDocument/rangeFormatting`
- [ ] `textDocument/onTypeFormatting`
- [x] `textDocument/prepareRename`
- [x] `textDocument/foldingRange`
- [x] `textDocument/selectionRange`
- [ ] `workspace/symbol`

Note that degrees of support for each LSP request are varying.

## Integration with other tools

### Formatters: OCamlFormat & Refmt

OCaml-LSP is dependent on external tools (OCamlFormat for OCaml and `refmt` for Reason)
for formatting source files. You should have the necessary tool (OCamlFormat and/or Refmt)
installed in your opam switch or esy project to have formatting support. Note, however, that
OCaml-LSP requires presence of OCamlFormat configuration file (called `.ocamlformat`) in
the project root to be able to format source files in your project.

## Contributing to project

```bash
# clone repo with submodules
git clone --recursive git@github.com:ocaml/ocaml-lsp.git

# if you already cloned, pull submodules
git submodule update --init --recursive

# create local switch (or use global one) and install dependencies
opam switch create . ocaml-base-compiler.4.11.1 --with-test

# don't forget to set your environment to use the local switch
eval ($opam env)

# build
make all

# the ocamllsp executable can be found at _build/default/ocaml-lsp-server/src/main.exe
```

## Tests

To run tests execute:

```sh
$ make test
```

Note that tests require [Node.js](https://nodejs.org/en/) and
[Yarn][https://yarnpkg.com/lang/en/] installed.

## Relationship to Other Tools

The lsp server uses merlin under the hood, but users are not required to have
merlin installed. We vendor merlin because we currently heavily depend on some
implementation details of merlin that make infeasible to upgrade the lsp server
and merlin independently.

## History

The implementation of the lsp protocol itself was taken from
[facebook's hack](https://github.com/facebook/hhvm/blob/master/hphp/hack/src/utils/lsp/lsp.mli)

Previously, this lsp server was a part of merlin, until it was realized that the
lsp protocol covers a wider scope than merlin.

## Comparison to other LSP Servers for OCaml

Note that the comparisons below make no claims of being objective and may be
entirely out of date:

- [reason-language-server](https://github.com/jaredly/reason-language-server)
  This server supports
  [bucklescript](https://github.com/BuckleScript/bucklescript) &
  [reason](https://github.com/facebook/reason). However, this project does not
  use merlin which means that it supports fewer versions of OCaml and offers less
  "smart" functionality - especially in the face of sources that do not yet
  compile.

- [ocaml-language-server](https://github.com/ocaml-lsp/ocaml-language-server)
  This project is extremely similar in the functionality it provides because it
  also reuses merlin on the backend. The essential difference is that this
  project is written in typescript, while our server is in OCaml. We feel that
  it's best to use OCaml to maximize the contributor pool.
