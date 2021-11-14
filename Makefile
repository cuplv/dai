# DAI_ROOT is set to the directory of this Makefile by default for all targets
export DAI_ROOT=$(dir $(abspath $(lastword $(MAKEFILE_LIST))))

.PHONY: default
default: build

.PHONY: build
build: fmt
	dune build
	cp _build/default/src/exec.exe ./dai

.PHONY: test
test: build
	dune runtest src

.PHONY: clean
clean:
	dune clean
	rm -f out/cfg/* out/log/* out/daig/* ./*.dot ./*.png

.PHONY: fmt
fmt:
	find src \
		\( -name '*.ml' -o -name '*.mli' \) \
		-exec ocamlformat {} -i --enable-outside-detected-project -m 100 \;

.PHONY: repl
repl:
	dune utop
