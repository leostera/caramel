export

CARAMEL = $(PWD)/caramel.exe
CARAMEL_STDLIB_PATH ?= $(PWD)/_build/default/stdlib

.PHONY: build
build:
	dune build @all -j8

.PHONY: watch
watch:
	dune build @all --watch -j8

.PHONY: manual
manual:
	mdbook build --dest-dir ../docs/manual ./manual

.PHONY: uninstall
uninstall:
	dune uninstall

.PHONY: install
install:
	dune install

.PHONY: setup
setup:
	opam install --deps-only ./caramel/formatter/ocamlformat-0.16.0/ocamlformat_lib.opam
	opam install ./erlang.opam
	opam install --deps-only ./caramel.opam

.PHONY: test
test:
	dune runtest

.PHONY: coverage
coverage:
	dune runtest --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src


.PHONY: prerel
prerel:
	dune install --prefix=_release/caramel --force --sandbox=copy --release
	rm -rf _release/caramel/bin/erl*
	rm -rf _release/caramel/doc/
	rm -rf _release/caramel/lib/erlang
	rm -rf _release/caramel/lib/caramel/compiler
	rm -rf _release/caramel/lib/caramel/verify
	rm -rf _release/caramel/lib/caramel/formatter
	rm -rf _release/caramel/lib/ocamlformat_lib

.PHONY: release
release: prerel
	strip _release/caramel/bin/caramel
	tar czf release.tar.gz -C _release caramel

.PHONY: release
release.win: prerel
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
