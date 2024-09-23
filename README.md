# Demanded Abstract Interpretation

_Incremental_ and _demand-driven_ abstract interpretation framework in OCaml

DAI requires:
 * OCaml version 4.09.0+
 * OPAM version 2.0.7+
 * Dune version 2.5.1+
 * System packages: libgmp-dev libmpfr-dev (for APRON numerical domains)
 * [Adapton](https://github.com/plum-umd/adapton.ocaml) version 0.1-dev (pinned
   as a local OPAM package via `make install`, per its README)
 * tree-sitter OCaml bindings for Java, generated using
   [semgrep/ocaml-tree-sitter-core](https://github.com/semgrep/ocaml-tree-sitter-core)
   and
   [semgrep/ocaml-tree-sitter-languages](https://github.com/semgrep/ocaml-tree-sitter-languages).
   These are under active development so there has been some drift as this
   project is not currently actively maintained.  We have pinned patched
   versions known to work with this project at the `dai` branch of forks at
   `https://github.com/bennostein/ocaml-tree-sitter-core`  and
   `https://github.com/bennostein/ocaml-tree-sitter-languages`; check these out
   and follow their README instructions to generate the necessary package, then
   install with `(cd ocaml-tree-sitter-languages/lang/java/ocaml-src && dune
   install --root .)`

Build with `make build` and run synthetic-edit experiments (as in PLDI
[paper](https://arxiv.org/abs/2104.01270)) with `./run_d1a_experiment`
