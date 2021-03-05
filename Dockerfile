FROM ubuntu:18.04

## Grab apt packages as root, including latest OPAM
RUN apt-get update && \
    apt-get install -y software-properties-common gcc libgmp-dev libmpfr-dev cmake m4 python3 python3-matplotlib && \
    add-apt-repository ppa:avsm/ppa && \
    apt-get update && \
    apt-get install -y opam ocamlbuild && \
    apt-get clean

## Create a new user 
RUN useradd -ms /bin/bash pldi && \
    apt-get install -y sudo && \
    adduser pldi sudo && \
    echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
USER pldi
WORKDIR /home/pldi

## Initialize OPAM, create a switch, and install dependencies (disable sandboxing inside of docker sandbox)
RUN opam init --disable-sandboxing --bare && \
    opam switch create d1a ocaml-variants.4.09.0+flambda && \
    opam switch d1a && \
    opam install -y ocamlfind num ppx_deriving apron core graphlib regular shell yojson && \
    opam clean -a -c -s -r --logs && \
    eval $(opam env) 

## Pull source for adapton, build, and pin as local opam package
RUN git clone https://github.com/plum-umd/adapton.ocaml.git && \
    make -C adapton.ocaml install

## Pull source for d1a, build
ARG D1A_VERSION=unknown
RUN git clone https://github.com/cuplv/d1a_impl.git -b artifact && \
    PATH=/home/pldi/.opam/d1a/bin:$PATH make -C d1a_impl build clean && \
    mkdir -p d1a_impl/out/experiments d1a_impl/out/plots d1a_impl/out/daig d1a_impl/out/cfg d1a_impl/out/log && \
    tar xf d1a_impl/semantic.tar.gz && \
    sudo mv semantic /usr/bin

RUN opam env >> /home/pldi/.bashrc

CMD ["/bin/bash"]
