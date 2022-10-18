#! /bin/bash

INSTALL_DIR=`pwd`

# Based on instructions found here: https://opam.ocaml.org/doc/Install.html

sudo add-apt-repository --yes ppa:avsm/ppa
sudo apt update
sudo apt-get install --yes opam
opam --version

# As of 2022-Oct-16 when I last ran this on an Ubuntu 20.04 system,
# this installed opam version 2.1.0

# TODO: I have not yet tested 'opam --yes init' with the --yes option
# to see whether it avoids interactive prompting.  Hopefully it does.
opam --yes init
eval $(opam env --switch=default)
opam --yes switch create . ocaml-base-compiler.4.09.0
sudo apt-get install --yes m4
opam --yes install merlin dune utop core ocamlformat
eval $(opam env)
opam --yes user-setup install
sudo apt-get install --yes bubblewrap
opam --yes install menhir

# I tried 'sudo apt-get install z3' on Ubuntu 20.04 on 2022-Oct-16,
# but it install version 4.8.7 according to the output of `z3
# --version`, which is older than the version 4.8.8 recommended by the
# Avenir README.

# Download Z3 zip file
wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.10/z3-4.8.10-x64-ubuntu-18.04.zip
# unzip it
unzip -e z3-4.8.10-x64-ubuntu-18.04.zip
# create a link from /usr/bin/z3 to where it is installed
Z3_INSTALL_DIR="${INSTALL_DIR}/z3-4.8.10-x64-ubuntu-18.04"
sudo ln ${Z3_INSTALL_DIR}/bin/z3 /usr/bin/z3

sudo apt-get install --yes libgmp-dev
opam --yes pin add z3 https://github.com/priyasrikumar/ocaml-z3.git

# Install desired version of petr4
cd ${INSTALL_DIR}
git clone https://github.com/cornell-netlab/petr4
cd petr4
git checkout cd556c1e2c20ccbd5b959f385cecebc43f5cfd72
PETR4_INSTALL_DIR=`pwd`

cd ${INSTALL_DIR}
git clone https://github.com/cornell-netlab/avenir
cd avenir
AVENIR_INSTALL_DIR=`pwd`

cd synthesis
opam --yes install p4pp=0.1.4
# I experienced errors attempting to compile petr4 in the next step
opam --yes pin add petr4 ${PETR4_INSTALL_DIR}

# I got the following error message attempting the next command:
# dune external-lib-deps: This subcommand is no longer implemented.

# This is the version of dune installed on my system:
# $ which dune
# /home/andy/install/_opam/bin/dune
# $ dune --version
# 3.4.1
dune external-lib-deps --missing @all

# There are more commands to run from the synthesis/README.md file,
# but the errors above lead me to believe I am doing something wrong,
# or the steps in the README need updating.
