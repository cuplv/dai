This README accompanies the artifact submission for PLDI paper #111 "Demanded Abstract Interpretation".

The artifact is provided as an Ubuntu-based Docker image that includes the dependencies to build the tool and reproduce the experiments reported in the paper.

These instructions describe how to get the image running on your local machine and replicate the papers' experiments.

=====================
Getting Started Guide
=====================

0.) Install Docker following the directions at [https://www.docker.com/get-started] for your OS, if it is not already installed.  We have prepared this artifact and tested it using Docker Desktop on MacOS, but it should work on Windows and Linux as well.

1.) Unzip the provided Docker image.
    `gunzip -c path/to/pldi21_paper111.tar.gz > pldi21_paper111.tar`

2.) Load it into Docker.
    `docker load < pldi21_paper111.tar`

3.) Run the image. This should open a bash shell, at the home directory of user `pldi`.  This user has sudo privileges and password `pldi`.
    `docker run -it pldi21_paper111 bash`

4.) In `~/d1a_impl` you will find:
      * the source code for our analysis framework (`./src`)
      * the source code of the experimental driver (`./experiments`)
      * a prebuilt binary of the experimental driver (`./experiments`)
      * this file (`./README.txt`).
    You can rebuild the binary with `make` if you wish, after running `eval $(opam env)` to add the necessary binaries to your $PATH.

5.) Run small versions of the experiments with `./kick_the_tires.sh`, to verify that everything runs smoothly.  This will generate and analyze random sequences of edits and queries as described in Section 7.3, but with 100 edits instead of the full 3000 as shown there.
    This will produce log files in `./out/experiments`, each named according to the analysis configuration, random seed, and number of edits used.  Each log file records the analysis cost (in milliseconds) after each edit.  It will also generate some miniature versions of the figure 10 plots, in `./out/plots`.

6.) From outside of the docker container, run `docker container ls` for a list of docker containers on your machine.  Note the "CONTAINER ID" of the container running this artifact.

7.) From outside of the docker container, run `docker cp <CONTAINER-ID>:/home/pldi/d1a_impl/out/plots/cdf.png cdf.png` to pull the plot out of the container and onto your local machine.  Confirm that you can open and view it.
    (The actual data will _not_ resemble the plots in the paper due to the tiny size of these kick-the-tires experiments -- this step is just to make sure that you can generate and view plots for the artifact evaluation.)

=========================
Step-by-Step Instructions
=========================

