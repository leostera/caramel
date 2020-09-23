.PHONY: all
all:
	dune build @install @default -j $(shell nproc)

.PHONY: deps
deps:
	opam install dune menhir ocaml-compiler-libs cmdliner

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

