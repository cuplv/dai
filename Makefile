.PHONY: default
default: build

.PHONY: build
build: fmt
	cp `find ../adapton.ocaml/ -name bits.cmx` $(OPAM_SWITCH_PREFIX)/lib/adapton
	dune build

.PHONY: test
test: build
	dune runtest src

.PHONY: clean
clean:
	dune clean
	rm -f out/cfg/* out/log/* out/daig/* ./*.dot ./*.png

.PHONY: fmt
fmt:
	ocamlformat src/*.ml* -i --enable-outside-detected-project -m 100
	ocamlformat experiments/*.ml* -i --enable-outside-detected-project -m 100

.PHONY: repl
repl:
	dune utop

.PHONY: experiments
experiments:
	dune runtest experiments
