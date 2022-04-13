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

.PHONY: callstring_experiments
callstring_experiments: build
	cat experiment_inputs/query_artifacts | xargs -Iartifact ./run_callstring_configs artifact out/log


.PHONY: csv
csv:
	echo NAME, BATCH, DEMAND, INCREMENTAL, DEMANDINCREMENTAL > out/experiments.csv
	cat experiment_inputs/query_artifacts | xargs -Ix ./print_as_csv_row x out/log >> out/experiments.csv

N=3000

run_synthetic_300:
	./run_synthetic 300 $(N)

run_synthetic_301:
	./run_synthetic 301 $(N)

run_synthetic_302:
	./run_synthetic 302 $(N)

run_synthetic_303:
	./run_synthetic 303 $(N)

run_synthetic_304:
	./run_synthetic 304 $(N)

run_synthetic_305:
	./run_synthetic 305 $(N)

run_synthetic_306:
	./run_synthetic 306 $(N)

run_synthetic_307:
	./run_synthetic 307 $(N)

run_synthetic_308:
	./run_synthetic 308 $(N)

run_synthetic_309:
	./run_synthetic 309 $(N)

run_synthetic_310:
	./run_synthetic 310 $(N)

run_synthetic_311:
	./run_synthetic 311 $(N)

run_synthetic_312:
	./run_synthetic 312 $(N)

run_synthetic_313:
	./run_synthetic 313 $(N)

run_synthetic_314:
	./run_synthetic 314 $(N)

run_synthetic_315:
	./run_synthetic 315 $(N)

run_synthetic_316:
	./run_synthetic 316 $(N)

run_synthetic_317:
	./run_synthetic 317 $(N)

run_synthetic_318:
	./run_synthetic 318 $(N)

run_synthetic_319:
	./run_synthetic 319 $(N)

run_synthetic_320:
	./run_synthetic 320 $(N)

run_synthetic_321:
	./run_synthetic 321 $(N)

run_synthetic_322:
	./run_synthetic 322 $(N)

run_synthetic_323:
	./run_synthetic 323 $(N)

run_synthetic_324:
	./run_synthetic 324 $(N)

run_synthetic_325:
	./run_synthetic 325 $(N)

run_synthetic_326:
	./run_synthetic 326 $(N)

run_synthetic_327:
	./run_synthetic 327 $(N)

run_synthetic_328:
	./run_synthetic 328 $(N)

run_synthetic_329:
	./run_synthetic 329 $(N)

run_synthetic_330:
	./run_synthetic 330 $(N)

run_synthetic_331:
	./run_synthetic 331 $(N)

run_synthetic_332:
	./run_synthetic 332 $(N)

run_synthetic_333:
	./run_synthetic 333 $(N)
