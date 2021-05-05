This README accompanies the artifact submission for PLDI paper #111 "Demanded Abstract Interpretation".

The artifact is provided as an Ubuntu-based Docker image that includes the dependencies to build the tool and reproduce the experiments reported in the paper.
The Dockerfile used to produce the image can be found at `~/d1a_impl/Dockerfile` or in the `artifact` branch of github.com/cuplv/d1a_impl, in case you need to rebuild the image locally or have questions about its configuration.

These instructions describe how to get the image running on your local machine and replicate the papers' experiments.

=====================
Getting Started Guide
=====================

0.) Install Docker following the directions at [https://www.docker.com/get-started] for your OS, if it is not already installed.  We have prepared this artifact and tested it using Docker Desktop on MacOS, but it should work on Windows and Linux as well.

1.) Unzip the provided Docker image.
    `gunzip -c path/to/pldi21_paper111.tar.gz > pldi21_paper111.tar`

2.) Load it into Docker.
    `docker load < pldi21_paper111.tar`

3.) Run the image. This should open a bash shell, at the home directory of user `pldi` (with sudo privileges).
    `docker run -it pldi21_paper111 bash`

4.) In `~/d1a_impl` you will find:
      * the source code for our analysis framework (`./src`)
          - OCaml source code of the core DAIG infrastructure, abstract domains, and utilities.
      * the source code of the experimental driver (`./experiments`)
          - OCaml source code of experiment-specific code: array bounds checking, random program-edit generation, and a CLI driver.
      * a prebuilt binary of the experimental driver (`./run_d1a_experiment`)
          - `./run_d1a_experiment -help` for usage information; this driver will generate random edits and queries according to the flags provided.
      * this file (`./README.txt`).
    You can rebuild the binary with `make` if you wish.  This will produce an executable binary at `./_build/default/experiments/exec.exe` and copy/rename it to `./run_d1a_experiment`.

5.) Run small versions of the experiments with `./kick_the_tires.sh`, to verify that everything runs smoothly.  This will generate and analyze random sequences of edits and queries as described in Section 7.3, but with 100 edits instead of the full 3000 as shown there.
    This will produce log files in `./out/experiments`, each named according to the analysis configuration, random seed, and number of edits used.  Each log file records the analysis cost (in milliseconds) after each edit.  It will also generate some miniature versions of the figure 10 plots, in `./out/plots`.

6.) From outside of the docker container, run `docker container ls` for a list of docker containers on your machine.  Note the "CONTAINER ID" of the container running this artifact.

7.) From outside of the docker container, run `docker cp <CONTAINER-ID>:/home/pldi/d1a_impl/out/plots .` to pull the plots out of the container and onto your local machine.  Confirm that you can open and view them.
    (The actual data will _not_ resemble the plots in the paper due to the tiny size of these kick-the-tires experiments -- this step is just to make sure that you can generate and view plots for the artifact evaluation.)

=========================
Step-by-Step Instructions
=========================

0.) First, follow the getting-started guide through step 3 if you have not already done so, and run the docker image.  Then, `cd` into `~/d1a_impl` for the rest of these instructions.

Expressivity Experiments
========================

1.) To analyze the Buckets.JS programs as described lines 1069-1078, run `dune runtest experiments`.  For each context-sensitivity policy {0,1,2}-CFA, this will output logs to `./out/log` containing one line per array access, classifying each access as SAFE (definitely in-bounds), UNSAFE (definitely out-of-bounds), or UNKNOWN (neither).
    1.a) To check the top-line numbers reported lines 1073-1076, the following commands may be useful:
         Safe 2-CFA accesses:   `grep "^SAFE" out/log/*2cfa.log | wc -l`
         Total 2-CFA accesses:  `cat out/log/*2cfa.log | wc -l`
         Safe 1-CFA accesses:   `grep "^SAFE" out/log/*1cfa.log | wc -l`
         Total 1-CFA accesses:  `cat out/log/*1cfa.log | wc -l`
         Safe 0-CFA accesses:   `grep "^SAFE" out/log/*0cfa.log | wc -l`
         Total 0-CFA accesses:  `cat out/log/*0cfa.log | wc -l`
    1.b) For a more detailed look at the analysis results or to better understand the contents of the the log files, you can also look at the CFG of each program or the DAIG before and after analysis of each program.  These are output in DOT/graphviz format in `./out/cfg` and `./out/daig` respectively; `_post` suffixes on the DAIG filenames denote the after-analysis versions.
    These `.dot` files are human-readable(-ish) or can be rendered as PNG by: `dot -Tpng input.dot > output.png`.  To do so within the container, you wil first need to `sudo apt-get install graphviz` then copy out the result as described in steps 5-7 of Getting Started, or you can copy the `.dot` out and build the PNG on your host machine.

2.) To analyze the list-append program shown in the overview and discussed in Section 7.2, run `dune runtest src/shape`.  This will output two `.dot` files to `./out/daig`: `list_append_pre.dot` with the pre-query DAIG state, and `list_append_post.dot` with the post-query DAIG state.

To check that the analysis indeed verifies the memory safety of the procedure, it should suffice to look in `list_append_post.dot` for the graph node with name beginning "exit{()}" (i.e. the exit location in the initial (unit) context).  Its contents are an abstract state consisting of two parts: a separation logic formula MEM (defined line 1086) and an environment ENV (defined line 1093).  We don't include pure constraints (defined line 1091) in the DOT output as they can get quite unwieldly.

At the exit, the MEM should read "lseg(a1,null)" (i.e. the heap contains a list segment from address a1 to null) and the ENV should read {(RETVAR, a1),(null,null)} (i.e. the distinguished return variables points to address a1 and the "null" variable points to null.  A list segment from a1 to null is simply a well-formed null-terminated list at a1, so this state confirms that the append procedure produces such a list.

For a more detailed look, these `.dot` files can be rendered as PNG images as described in step 1.b above; this will confirm that analysis converges in one demanded unrolling as mentioned line 1134.

Scalability Experiments
=======================

1.)  Steps 5-7 of the Getting Started guide above detail the process to reproduce the scalability experiments described in Section 7.3; just replace `./kick_the_tires.sh` in step 5 by `./scalability_experiments_<size>.sh` for some choice of size, described below.  This will generate and analyze random sequences of edits and queries as described in Section 7.3.

In our experiments for the paper, we ran each configuration for 3000 edits in each of 9 random seeds.  This took several processor-weeks of compute time distributed across 4 separate cloud machines, so it is quite infeasible for artifact evaluators to reproduce the full slate of experiments on laptops.

As such, we have provided three scripts to run scalability experiments:
  - `./scalability experiments_large.sh`, which runs each configuration for 1000 edits in each of 4 random seeds  This will take quite some time, on the order of several hours to a day depending on your hardware.
  - `./scalability_experiments_small.sh`, which runs each configuration for 500 edits in each of 4 random seeds. This should be very fast, on the order of minutes to a couple of hours.
  - `./scalability_experiments_full.sh`, which _doesn't actually run any experiments_ but uses the data we gathered to reproduce the plots of figure 10.  This data is provided at `./data`.

Reviewers are invited to edit the scripts and adjust these numbers as needed to produce some results with the compute/memory resources available to them; `./scalability_experiments_{small,large}.sh` are annotated with instructions on how to do so.

2.) Using `docker cp` as described in step 7 of the getting started guide, view the generated plots.

3.) If desired (after generating plots), the summary statistics reported in the table of Fig. 10 can be reproduced on this dataset:
    3.a) `sudo apt-get install datamash`
    3.b) `datamash mean 1 median 1 perc:90 1 perc:95 1 perc:99 1 < tmp/batch_all.log`
    3.c) `datamash mean 1 median 1 perc:90 1 perc:95 1 perc:99 1 < tmp/incr_all.log`
    3.d) `datamash mean 1 median 1 perc:90 1 perc:95 1 perc:99 1 < tmp/dd_all.log`
    3.e) `datamash mean 1 median 1 perc:90 1 perc:95 1 perc:99 1 < tmp/dd_incr_all.log`
    
=================================
Claims supported by this artifact
=================================

Our paper studies two research questions related to this artifact, referred to as "Expressivity" and "Scalability" and discussed/evaluated in Section 7.
We reproduce those two RQs here and will discuss each in turn.

 * Expressivity: Does the DAIG framework allow for clean and straightforward implementations of rich analysis domains that cannot be handled by existing incremental and/or demand-driven frameworks?

 * Scalability: For these rich analysis domains, what degree of performance improvement can be obtained by performing incremental and/or demand-driven analysis, as compared to batch analysis?

# Expressivity

The Buckets.JS array-bounds-checking and list memory-safety experiments support the claims about specific microbenchmarks on line 1073 and 1129.

The three abstract domain implementations described in section 7.2 can be found at `./src/itv.ml`, `./src/octagon.ml`, and `./src/shape/state.ml`.
These domain implementations are fully unaware of any incremental/demand-driven logic, and are thus comparable in their size and complexity to analogous domain implementations in batch abstract interpreters, supporting the claims of sections 7.1, 7.2.
The abstract domain interface they each implement can be found at `~/d1a_impl/src/abstract.ml` as module type `DomNoCtx`. (`Dom` is a lifting from such domains to context-sensitive variants for interprocedural analysis, as discussed in section 7.1; these context functors are at `~/d1a_impl/src/context.ml`)
Together, these implementations support our claims of "clean and straightforward"-ness (line 995) and of the effort required to implement new domains (line 1013)

Artifact evaluators can also confirm that our implementation uses the APRON library without modification (as mentioned line 1068) -- the library is imported as a dependency through `opam` (e.g. run `opam list` or look at the `dune` configuration files).

# Scalability

Because of the hardware and experiment-size differences, we expect that your results won't look the same as Figure 10.

Nonetheless, the same trends should be visible:
 - batch analysis will be the most costly
 - incremental-only and demand-only analysis will be significantly faster but still grow in cost with cumulative program edits
 - incremental+demand-driven analysis will be comparable in cost to the incremental-only/demand-only configurations at first, but should stay fast even as the program grows in size with edits.

Note that these relative scaling differences are more pronounced the larger the experiment, as the cost of analysis grows super-linearly with program size and the overheads associated with incrementality and demand are relatively less important.  Reviewers should compare their results to the relevant segment of the scatter plots of figure 10; for example, if you run `./scalability_experiments_large.sh`, your results correspond to the leftmost third of each scatter plot, but if you run `./scalability_experiments_small.sh` then your results correspond only the leftmost sixth of each plot and it may be difficult to draw conclusions about scalability due to noise in the data and small sample sizes.

As with the experimental results of the paper, these results should be seen more as proof-of-concept than conclusive measurements of absolute analysis costs.
The relative scaling behaviors of each analysis configuration should support our claims (e.g. at lines 1219-1237) that the combination of incrementality and demand has potential to significantly outperform either technique on its own, with analysis costs consistently at ~1sec or less.


=====================================
Claims not supported by this artifact
=====================================
 * The claim that these domains "cannot be handled by existing incremental and/or demand-driven frameworks" is not in scope here -- see related work for discussion about that, but such a statement is not suited to artifact evaluation.

 * The scalability results shown in the CDF and table of Figure 10 are likely not to be reproduced by artifact evaluators -- the compute resources necessary to reproduce experiments at that scale are well beyond what the artifact evaluation committee could reasonably be expected to use.


===========================
Reusability & Extensibility
===========================
There are several ways in which this implementation could be extended for different analysis domains, context-sensitivity policies, and front-end/experiment harnesses.

# Adding a new abstract domain

The most obvious extension to this tool would be to add a new abstract domain.  In order to do so, one must implement a module of type `DomNoCtx`. (src/abstract.ml:27)

To implement a new abstract domain `My_new_domain`, proceed as follows:

1. Define a public interface for your domain:
   `echo "include Abstract.DomNoCtx" > src/my_new_domain.mli`

2. Create an empty implementation for your domain, attempt to build:
   `touch src/my_new_domain.ml && make build`

   This should fail with a list of module members that you have declared in the `.mli` but have not yet implemented in the `.ml` (i.e. everything in `Abstract.DomNoCtx`).

3. Implement missing module members in `src/my_new_domain.ml` until `make build` succeeds; `src/unit_dom.ml`, which implements the trivial 1-element "unit" abstract domain, would be a good reference point/example to work from.

4. Done!  You can now lift your domain with one of the provided context functors; e.g. `Context.MakeInsensitive(My_new_domain)` is a context-insensitive domain (of type `Abstract.Dom`) that can be used elsewhere in the framework.

# Adding a new context-sensitivity policy

To implement a new context-sensitivity policy, implement a functor of type `CtxFunctor` as defined in `src/context.ml`.

The three functors therein (`MakeInsensitive`, `Make1CFA`, `Make2CFA`) can serve as examples, and see e.g. `experiments/arrays.ml` for how such functors can then be used.

# Using a different abstract domain for scalability experiments

To use a different abstract domain in the scalability experiment harness, just replace `Octagon` on lines 46, 56, 66, and 76 of `experiments/exec.ml` by the abstract domain of your choice: for example, if you implemented a new context-sensitivity policy `My_ctx_functor : Context.CtxFunctor` and a new abstract domain `My_domain : Abstract.DomNoCtx` per the previous instructions, then you would replace each `Octagon` by `My_ctx_functor (My_domain)`.

Then, run `make build`, which will build a new binary of the experiment harness at `./run_d1a_experiment`.

Finally, run `./run_d1a_experiment` (with arguments for experiment size and configuration, or with `-help` to see possible arguments)

# Running a specific analysis on a specific source file

Given a domain `My_dom` of type `Abstract.Dom` and a JavaScript source file at `/path/to/foo.js` (in the subset of JS supported by our frontend), the following snippet will build a CFG and DAIG, and fully evaluate the DAIG (i.e. compute a fixed-point on all paths from which program exit is reachable)

```
module Daig = Dai.Daig.Make (My_dom)

let%test "analyze foo.js in My_dom" =
  (* build a CFG of the program *)
  let cfg = Dai.Cfg_parser.(json_of_file >> cfg_of_json) "/path/to/foo.js" in
  (* build a DAIG from that CFG, with the domain's initial abstract state at program entry *) 
  let daig = Daig.of_cfg ~init_state:(My_dom.init ()) cfg in
  (* construct the name of the program exit abstract state *)
  let exit_loc = Daig.Name.Loc (Dai.Cfg.Loc.exit, My_dom.Ctx.init) in
  (* issue a query for said abstract state (discarding the result by prefixing with an underscore) *)
  let _abs_state_at_exit,daig = Daig.get_by_name exit_loc daig in
  (* dump a DOT representation of the resulting DAIG state at /path/to/output.dot *)
  Daig.dump_dot daig ~filename:"/path/to/output.dot";
  true
```

If that snippet were in a `.ml` file in directory `~/d1a_impl/some/relative/path", you could then run the analysis with `dune runtest some/relative/path`.

Examples of some variations on the above pattern can be found at the end of `src/shape/state.ml` (using a different precondition/program-entry abstract state), `src/daig.ml` (issuing queries at different locations), and `experiments/arrays.ml` (programmatically checking an invariant at all array accesses in programs, and using multiple different context sensitivities).
