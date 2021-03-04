This README accompanies the artifact submission for PLDI paper #111 "Demanded Abstract Interpretation".

The artifact is provided as an Ubuntu-based Docker image that includes the dependencies to build the tool and reproduce the experiments reported in the paper.

The Dockerfile used to produce the image is provided at the end of this file, in case there are questions about the configuration or a need to rebuild the image locally (e.g. if there are issues with getting-started steps 1 and 2)

These instructions describe how to get the image running on your local machine and replicate the papers' experiments.

NB: Many utilities (notably the build system `dune`) are installed through OPAM, the OCaml package manager.  If you ever see a "command not found" error, it is likely that you need to run `eval $(opam env)` to put any OPAM-managed binaries on your $PATH.  Unfortunately, this is per-bash-session and cannot be preconfigured in the Docker image.

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
    You can rebuild the binary with `make` if you wish, after running `eval $(opam env)` to add the necessary binaries to your $PATH.

5.) Run small versions of the experiments with `./kick_the_tires.sh`, to verify that everything runs smoothly.  This will generate and analyze random sequences of edits and queries as described in Section 7.3, but with 100 edits instead of the full 3000 as shown there.
    This will produce log files in `./out/experiments`, each named according to the analysis configuration, random seed, and number of edits used.  Each log file records the analysis cost (in milliseconds) after each edit.  It will also generate some miniature versions of the figure 10 plots, in `./out/plots`.

6.) From outside of the docker container, run `docker container ls` for a list of docker containers on your machine.  Note the "CONTAINER ID" of the container running this artifact.

7.) From outside of the docker container, run `docker cp <CONTAINER-ID>:/home/pldi/d1a_impl/out/plots .` to pull the plots out of the container and onto your local machine.  Confirm that you can open and view them.
    (The actual data will _not_ resemble the plots in the paper due to the tiny size of these kick-the-tires experiments -- this step is just to make sure that you can generate and view plots for the artifact evaluation.)

=========================
Step-by-Step Instructions
=========================

0.) First, follow the getting-started guide through step 3 if you have not already done so, and run the docker image.

# Expressivity Experiments

1.) To analyze the Buckets.JS programs as described lines 1069-1078, run `dune runtest experiments` from `~/d1a_impl`.

TODO(benno): Finish up here once `semantic` is working from within docker.

# Scalability Experiments

1.)  Steps 5-7 of the Getting Started guide above detail the process to reproduce the scalability experiments described in Section 7.3; just replace `./kick_the_tires.sh` in step 5 by `./scalability_experiments.sh`.  This will generate and analyze random sequences of edits and queries as described in Section 7.3.

In our experiments for the paper, we ran each configuration for 3000 edits in each of 9 random seeds.  This took several processor-weeks of compute time, so it is quite infeasible for artifact evaluators to reproduce the full slate of experiments on laptops.
As such, we have set up the `./scalability_experiments.sh` script to run each config for 1000 edits in each of 4 random seeds; this should take on the order of a few hours on a modern laptop.

Reviewers are invited to edit the script and adjust these numbers as needed to produce some results with the compute/memory resources available to them; the script is commented with some instructions on how to do so.

2.) Using `docker cp` as described in step 7 of the getting started guide, view the generated plots.

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

Because of the hardware and experiment-size differences, we expect that your results won't look precisely the same as Figure 10.

Nonetheless, the same trends should be visible:
 - batch analysis will be the most costly
 - incremental-only and demand-only analysis will be significantly faster but still grow in cost with cumulative program edits
 - incremental+demand-driven analysis will be comparable in cost to the incremental-only/demand-only configurations at first, but should stay fast even as the program grows in size with edits.

As with the experimental results of the paper, these results should be seen more as proof-of-concept than conclusive measurements of absolute analysis costs.
The relative scaling behaviors of each analysis configuration should support our claims (e.g. at lines 1219-1237) that the combination of incrementality and demand has potential to significantly outperform either technique on its own, with analysis costs consistently at ~1sec or less.


=====================================
Claims not supported by this artifact
=====================================
 * The claim that these domains "cannot be handled by existing incremental and/or demand-driven frameworks" is not in scope here -- see related work for discussion about that, but such a statement is not suited to artifact evaluation.

 * The CDF and table of Figure 10 are likely not to be reproduced by artifact evaluators -- the compute resources necessary to reproduce experiments at that scale are well beyond what the artifact evaluation committe could reasonably be expected to use.


==========
Dockerfile
==========

TODO(benno) add final Dockerfile here before submitting
