.PHONY: build
build:
	dune build @install @default -j $(shell nproc)

.PHONY: watch
watch:
	dune build @all --watch -j $(shell nproc)

.PHONY: install
install:
	dune install

.PHONY: deps
deps:
	opam install dune menhir ocaml-compiler-libs cmdliner patdiff ppx_sexp_conv

.PHONY: test
test:
	dune runtest

.PHONY: promote
promote:
	dune promote

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean

