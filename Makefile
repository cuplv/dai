.PHONY: default
default: build

.PHONY: build
build: fmt
	cp `find ../adapton.ocaml/ -name bits.cmx` $(OPAM_SWITCH_PREFIX)/lib/adapton
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
