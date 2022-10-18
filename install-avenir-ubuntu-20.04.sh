#! /bin/bash

AVENIR_INSTALL_DIR="$HOME/forks/avenir"

# Based on instructions found here: https://opam.ocaml.org/doc/Install.html

sudo add-apt-repository ppa:avsm/ppa
sudo apt update
sudo apt install opam
opam --version

# As of 2022-Oct-16 when I last ran this on an Ubuntu 20.04 system,
# this installed opam version 2.1.0

# The commands below are for
opam init
opam switch create . ocaml-base-compiler.4.09.0
sudo apt-get install m4
opam install merlin dune utop core ocamlformat
opam user-setup install
sudo apt-get install bubblewrap
opam install menhir

# I tried 'sudo apt-get install z3' on Ubuntu 20.04 on 2022-Oct-16,
# but it install version 4.8.7 according to the output of `z3
# --version`, which is older than the version 4.8.8 recommended by the
# Avenir README.

# Download Z3 zip file
# unzip it
# maybe need to create a link from /usr/bin/z3 to where it is installed
INSTALL_DIR="$HOME/install/avenir"
Z3_INSTALL_DIR="$HOME/z3-4.8.17-x64-glibc-2.31/bin/z3"
sudo ln ${Z3_INSTALL_DIR}/bin/z3 /usr/bin/z3

sudo apt-get install libgmp-dev
opam pin add z3 https://github.com/priyasrikumar/ocaml-z3.git

# Install desired version of petr4
cd ${INSTALL_DIR}
git clone https://github.com/cornell-netlab/petr4
cd petr4
git checkout cd556c1e2c20ccbd5b959f385cecebc43f5cfd72

cd ${AVENIR_INSTALL_DIR}/synthesis
opam --yes install p4pp=0.1.4
opam --yes pin add petr4 ${INSTALL_DIR}/petr4

dune external-lib-deps --missing @all
