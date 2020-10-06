# Getting Started

+ First, [install](https://opam.ocaml.org/doc/Install.html) OPAM version >2.0.4. If you already have OPAM >2.0.4 installed you can skip this step. If you have an earlier version of OPAM, upgrade it. 

+ In the `synthesis` directory, run

```
opam switch create . ocaml-base-compiler.4.09.0
```

to install the correct version of ocaml. Be sure to type `eval $(opam
env)` to load the appropriate environment.

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

+ Install a binary of the [Z3 SMT
  solver](https://github.com/Z3Prover/z3) to `/usr/bin/z3`. This is the default if you run `sudo apt install z3`.
  
+ Pin our custom fork of PLASMA's Z3 serialization library that interfaces with the previously-installed binary:

```bash
opam pin add z3 https://github.com/priyasrikumar/ocaml-z3.git
```

+ Install [petr4](https://github.com/cornell-netlab/petr4) using the following instructions. First
  clone the repository a directory of your choosing `<petr4 fp>`. Run the following commands
```
cd <petr4 fp>
git checkout cd556c1e2c20ccbd5b959f385cecebc43f5cfd72
```
Then change back to the `hybrid/synthesis` directory, and run
```
opam pin add petr4 <petr4 fp>
```

+ Install any remaining dependencies (e.g. `async`) using `opam
  install` (e.g. `opam install async`) that show up when you run the
  following command:

```
dune external-lib-deps --missing @all
```



<!-- + If you get `libz3.so` error try addding the Z3 library to  your path. In Mac, this can be achieved by setting the DYLD_LIBRARY_PATH variable: -->

<!-- ``` -->
<!-- export DYLD_LIBRARY_PATH=`opam config var z3:lib` -->
<!-- ``` -->

<!-- on GNU/Linux systems the same can be done via the LD_LIBRARY_PATH environment variable: -->

<!-- ``` -->
<!-- export LD_LIBRARY_PATH=`opam config var z3:lib` -->
<!-- ``` -->

<!-- + If you get linking errors from Z3 and your Z3 installation in opam is 4.8.* try switching back to 4.7.*. -->

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

# Running Experiments from ONF

To run the ONF experiments, run

```
./avenir onf-real benchmarks/real/p4programs/onos/pipelines/fabric/impl/src/main/resources/fabric.p4 benchmarks/real/p4programs/stratum-onos-demo/p4src/main.p4 benchmarks/real/log_edits.csv benchmarks/real/phys_edits.csv benchmarks/real/onos_fvs_full.csv benchmarks/real/assume.mt -I1 benchmarks/real/p4programs/onos/pipelines/fabric/impl/src/main/resources/include -I1 benchmarks/real/p4includes/ -I2 benchmarks/real/p4programs/stratum-onos-demo/ -I2 benchmarks/real/p4includes/ -data benchmarks/onos_201_adds.csv -b 100 -e 3 --fastcx --restrict-masks -w --domain-restr --hints -s --cache-edits --min --cache-queries

```

However this might take too long, or you may want some intermediate
results. You can truncate the file using, e.g. `head -100` or `head -1000`
to create a new file and run:

```
head -200 ./benchmarks/onos_201_adds.csv > ./benchmarks/onos_201_200_adds.csv
```

