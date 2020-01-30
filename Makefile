.PHONY: default
default: build

.PHONY: build
build: fmt
	dune build

.PHONY: test
test:
	dune runtest

.PHONY: clean
clean:
	dune clean

.PHONY: fmt
fmt:
	ocamlformat src/*.ml -i --enable-outside-detected-project

.PHONY: repl
repl:
	dune utop
