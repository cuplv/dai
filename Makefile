.PHONY: default
default: build

.PHONY: build
build:
	dune build
	cp ./_build/default/experiments/exec.exe ./run_d1a_experiment

.PHONY: test
test: build
	dune runtest src

.PHONY: test_exps
test_exps: build
	dune runtest experiments

.PHONY: clean
clean:
	dune clean
	rm -f out/cfg/* out/log/* out/daig/* ./*.dot ./*.png

.PHONY: fmt
fmt:
	ocamlformat src/*.ml* -i --enable-outside-detected-project -m 100
	ocamlformat src/shape/*.ml* -i --enable-outside-detected-project -m 100	
	ocamlformat experiments/*.ml* -i --enable-outside-detected-project -m 100

.PHONY: repl
repl:
	dune utop
