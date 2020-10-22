export

CARAMELC = $(PWD)/caramelc.exe
CARAMELC_STDLIB_PATH ?= $(PWD)/_build/default/src/stdlib

.PHONY: build
build:
	dune build @all -j8

.PHONY: watch
watch:
	dune build @all --watch -j8

.PHONY: install
install:
	dune install

.PHONY: deps
deps:
	opam install dune menhir ocaml-compiler-libs cmdliner ppx_sexp_conv sexplib ocamlformat bisect_ppx

.PHONY: test
test:
	dune runtest

.PHONY: coverage
coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src

release:
	dune install --prefix=caramel-$(shell cat VERSION) --force --sandbox=copy --release
	rm -rf _release/lib/caramel/erlang
	rm -rf _release/lib/caramel/typing
	rm -rf _release/lib/caramel/compiler
	tar czf release.tar.gz caramel-$(shell cat VERSION)

.PHONY: promote
promote:
	dune promote

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean
