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

.PHONY: experiments
experiments: build
	cat experiment_inputs/query_artifacts | xargs -Iartifact ./run_configs artifact out/log

.PHONY: csv
csv:
	echo NAME, BATCH, DEMAND, INCREMENTAL, DEMANDINCREMENTAL > out/experiments.csv
	cat experiment_inputs/query_artifacts | xargs -Ix ./print_as_csv_row x out/log >> out/experiments.csv
