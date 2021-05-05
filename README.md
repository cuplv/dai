# Demanded Abstract Interpretation

_Incremental_ and _demand-driven_ abstract interpretation framework in OCaml

DAI requires:
 * OCaml version 4.09.0+
 * OPAM version 2.0.7+
 * Dune version 2.5.1+
 * System packages: libgmp-dev libmpfr-dev (for APRON numerical domains)
 * [Adapton](https://github.com/plum-umd/adapton.ocaml) version 0.1-dev (pinned as a local OPAM package via `make install`, per its README)

Build with `make build` and run synthetic-edit experiments (as in PLDI [paper](https://arxiv.org/abs/2104.01270)) with `./run_d1a_experiment`

