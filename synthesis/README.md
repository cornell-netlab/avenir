# Getting Started

+ First, [install](https://opam.ocaml.org/doc/Install.html) OPAM version >2.0.4. If you already have OPAM >2.0.4 installed you can skip this step. If you have an earlier version of OPAM, upgrade it. 

+ In the `synthesis` directory, run

```
opam switch create . ocaml-base-compiler.4.09.0
```

to install the correct version of ocaml. Be sure to type `eval $(opam
env)` to load the appropriate environment.

+ Install `m4` if it is not already installed.

``` bash
sudo apt-get install m4
```

+ Now, install a few things to get you started

```
opam install merlin dune utop core
opam user-setup install
```

+ Install Menhir

```bash
apt install bubblewrap
opam install menhir
```

+ Install a binary of the [Z3 SMT solver](https://github.com/Z3Prover/z3) to
  `/usr/bin/z3`. Ensure you are using version 4.8.8 or greater. You may need to
  directly download a binary from the _Releases_ tab, as the `apt` repositories
  may not be up to date.
  
+ Pin our custom fork of PLASMA's Z3 serialization library that interfaces with the previously-installed Z3 binary. You may need to also install GMP-lib if it is not already installed on your system.

```bash
sudo apt install libgmp-dev
opam pin add z3 https://github.com/priyasrikumar/ocaml-z3.git
```

+ Incorporate [petr4](https://github.com/cornell-netlab/petr4) into the current switch using the following instructions. First clone the repository a directory of your choosing `<petr4 fp>`. Run the following commands
```
cd <petr4 fp>
git checkout cd556c1e2c20ccbd5b959f385cecebc43f5cfd72
```
Then change back to the `hybrid/synthesis` directory. Install the version of the p4 preprocessor `p4pp` that works with this specific commit:
``` bash
opam install p4pp=0.1.4
```
Then, pin the petr4 package to the local state.
```
opam pin add petr4 <petr4 fp>
```

+ Install any remaining dependencies (e.g. `async`) using `opam install` (e.g.
  `opam install async`) that show up when you run the following command:

```
dune external-lib-deps --missing @all
```

The list of packages should be `async cohttp-async ipaddr shell`. If `z3`,
`petr4`, or `p4pp` show up here, repeat the previous steps untill they no longer appear when you run this command. If `menhir` appears in this list even when `opam` declares that it has been correctly installed, you may proceed.

+ Run `make` to verify that `avenir` builds.

+ The makefile is currently a bit wonky, so for subsequent compilations, you may need to run `make rebuild` instead of `make build`.

# Running the code

To build the executable, run

```
make
```

which will create an executable `avenir` in your current directory. To run the executable, run

```
./avenir <command here>
```

## Verification

For an abstract program `hello/abstract.p4` and a target program
`hello/target.p4`, we will verify a set of target insertions
(`hello/solution.csv`) that implement the behavior indicated by the the
abstract insertions in `hello/inserts.csv`.

To verify equivalence, run the following commands
```
cd hello
../avenir eq-real abstract.p4 target.p4 inserts.csv solution.csv fvs noassume -I1 includes -I2 includes
```
This will print the IR encoding of `abstract.p4` and `target.p4` followed by
either `Equivalent`, or a counterexample. In this case you should see
`Equivalent`. 

## Synthesis

To synthesize the same insertions whose correctness we just verified, make sure you are still in the `hello` directory and run 

```
../avenir synth abstract.p4 target.p4 no_edits.csv no_edits.csv fvs -b 1000 -e 10 -data inserts.csv -I1 includes -I2 includes -P4 -p
```
You should again see the IR encoding of the pipeline programs, and then a line
that says `Target operations`, and the same (possibly reordered) operations as
are in `hello/solution.csv`. Try `cat`ing this file to verify this.

## Server Mode

To run avenir in server mode, type

```
./avenir server <arguments>
```

which will start a server that listens for POSTs containing JSON
objects on port `9000`.

For example, to start a server that listens for operations on `abstract.p4` and responds with equivalent operations on `target.p4`, run

```
../avenir server abstract.p4 target.p4 no_edits.csv no_edits.csv fvs -b 1000 -e 10 -data inserts.sv -I1 includes -I2 includes -P4 -p
```

# Running Experiments

We can run the experiments from the NSDI paper, the Switch Reboot case study,
the retargeting experiment and the BMV2 simple-switch demonstration.

## Prerequisite software

The following packaged are required to run these evaluations:
- python2.7
- python-tk (via `apt`)
- matplotlib (via `pip`)
- ipython (via `pip`)
- ipykernel (via `pip`)

The following worked on Ubuntu 20.04 in January 2021.

```bash
# Install python
apt install python2

# Install pip
curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
python2 get-pip.py

# Install packages
apt install python-tk
python2 -m pip install matplotlib
python2 -m pip install ipython
python2 -m pip install ipykernel
```


## ONF Switch Reboot
To run the ONF experiments, run

```bash
bash ./run-avenir-onos.sh
```

## Retargeting Experiment

To run the retargeting experiments, run

``` bash
cd synthesis/benchmarks/retargeting
```

This will generate performance graphs for each experiment: `self`,
`early_validate`, `metadata`, and `action_decompose` in files `*.pdf` and raw
data files in `*.csv`.

## BMV2 Experiment

To run the bmv2/mininet emulation experiment, there is a bunch of setup to do first.

### Setup
first you will need to install
bmv2 [from
source](https://github.com/p4lang/behavioral-model#building-the-code). The
following commands worked on Ubuntu 20.04 in January 2021.

First, install toplevel dependencies.
``` bash
apt install automake                         \
            cmake                            \
            libjudy-dev                      \
            libgmp-dev                       \
            libpcap-dev                      \
            libboost-dev                     \
            libbost-test-dev                 \
            libboost-program-options-dev     \
            libboost-system-dev              \
            libboost-filesystem-dev          \
            libboost-thread-dev              \
            libevent-dev                     \
            libtool                          \
            flex                             \
            bison                            \
            pkg-config                       \
            g++                              \
            libssl-dev                       \
            mininet
``` 
Then, install [thrift 0.11.0](https://github.com/apache/thrift/tree/0.11.0)  from source.

``` bash
git clone git@github.com:apache/thrift.git
cd thrift
git checkout 0.11.0 #? 
./bootstrap.sh
./configure
make
sudo make install
cd lib/py
sudo python setup.py install
cd ..
```

Now, install [nanomsg 1.0.0](https://github.com/nanomsg/nanomsg/tree/1.0.0) from source
``` bash
git clone git@github.com:nanomsg/nanomsg.git
cd nanomsg
git checkout 1.0.0 # ?
mkdir build && cd build
cmake ..
cmake --build .
ctest -G Debug .
sudo cmake --build . --target install
sudo ldconfig
cd ../..
```

And some additional dependencies for interacting with the CLI

``` bash
sudo apt-get install python-dev libxml2-dev libxslt-dev
```

Finally, we can download and install Bmv2:

``` bash
git clone git@github.com:p4lang/behavioral-model.git
cd behavioral-model
./autogen.sh
./configure
make
sudo make install
cd ..
```

Now return to the Avenir `synthesis` directory. And execute the following commands

``` bash
cd benchmarks/bmv2
cp <path to bmv2>/targets/simple_router .
cp <path to bmv2>/tools/runtime_CLI.py ./simple_router
```

### Running the BMV2 experiment

Now to run the experiment, execute the following commads

``` bash
cd mininet
sudo -E python2 1sw_demo.py \
    --behavioral-exe ../simple_router/simple_router \
    --json ../simple_router/simple_router.json \ 
    --thrift-port 9000 \
    --num-hosts 64 \
    --rules rules64.txt \
    --loc ~/avenir/synthesis
```

# Running Tests

Tests live in the `test` directory. To execute tests, execute `dune runtest` in
the `synthesis` directory.
