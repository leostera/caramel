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

.PHONY: release
release:
	dune install --prefix=_release/caramel --force --sandbox=copy --release
	rm -rf _release/caramel/lib/erlang
	rm -rf _release/caramel/lib/caramel/typing
	rm -rf _release/caramel/lib/caramel/compiler
	tar czf release.tar.gz -C _release caramel

release-erlang:
	rm -f _build/*.tbz
	dune-release distrib --skip-build --skip-lint --skip-tests -p erlang
	mv _build/caramel*.tbz erlang.tbz

.PHONY: opam-publish
opam-publish: build
	dune-release distrib --skip-build --skip-tests -p erlang -n erlang
	dune-release opam pkg -p erlang -n erlang
	dune-release opam submit -p erlang -n erlang

.PHONY: promote
promote:
	dune promote

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean
