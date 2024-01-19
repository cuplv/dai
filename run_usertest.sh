#!/bin/sh

dune build

# --force is needed to run the tests even if the source code has not changed
DAI_ROOT="../" dune runtest usertest/ --force

dir="_build/default"
set -o xtrace # to print out commands
for fname in $dir/analyzed_*.dot; do
		dot -Tps $fname -o "${fname%.dot}.ps"
done
for fname in $dir/solved*.dot; do
		dot -Tps $fname -o "${fname%.dot}.ps"
done

