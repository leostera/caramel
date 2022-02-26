export

CARAMEL = $(PWD)/caramel.exe
CARAMEL_STDLIB_PATH ?= ./

.PHONY: build
build:
	dune build @all

.PHONY: watch
watch:
	dune build @all --watch

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
	opam install --deps-only ./caramel.opam

.PHONY: test
test:
	dune test ./tests -p caramel

.PHONY: cloc
cloc:
	cloc caramel --read-lang-def=./tests/caramel.txt
	cloc tests --read-lang-def=./tests/caramel.txt

.PHONY: coverage
coverage:
	dune test --instrument-with bisect_ppx --force
	bisect-ppx-report html --expect src


.PHONY: prerel
prerel:
	dune build @install
	dune install --prefix=_release/caramel --force --sandbox=copy --release

.PHONY: release
release: prerel
	strip _release/caramel/bin/caramel
	tar czf release.tar.gz -C _release caramel

.PHONY: release
release.win: prerel
	tar czf release.tar.gz -C _release caramel

.PHONY: promote
promote:
	dune promote

.PHONY: fmt
fmt:
	dune build @fmt --auto-promote

.PHONY: clean
clean:
	dune clean
