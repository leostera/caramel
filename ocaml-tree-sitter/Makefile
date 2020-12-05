
.PHONY: build
build:
	dune build .
	dune exec ./tests/main.exe

.PHONY: clean
clean: do-clean build

do-clean:
	dune clean

fmt:
	make -C .. fmt
