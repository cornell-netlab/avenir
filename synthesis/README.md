# Getting Started

+ First make sure you have `opam` installed. Then in the `synthesis` directory, run

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

+ Install any remaining dependencies that show up when you run the following command:

```
dune external-lib-deps --missing @all
```

+ Now you're ready to go! Open the library in `utop` by running `dune lib utop`

+ This project needs [Z3 SMT solver](https://github.com/Z3Prover/z3).
  Currently, running the below command will build the project:

```
opam pin add z3 https://github.com/priyasrikumar/ocaml-z3.git
```

+ If you get `libz3.so` error try addding the Z3 library to  your path. In Mac, this can be achieved by setting the DYLD_LIBRARY_PATH variable:

```
export DYLD_LIBRARY_PATH=`opam config var z3:lib`
```

on GNU/Linux systems the same can be done via the LD_LIBRARY_PATH environment variable:

```
export LD_LIBRARY_PATH=`opam config var z3:lib`
```

+ If you get linking errors from Z3 and your Z3 installation in opam is 4.8.* try switching back to 4.7.*.

# Running the code

To build the executable, run

```
make
```

which will create an executable `motley` in your current directory. To run the executable, run

```
./motley <command here>
```


# Running Experiments from ONF

To run the ONF experiments, run

```
./motley onf -data <onf/csv/file> -gas 1
```

Using the datafile checked into the repository you can run

```
./motley onf -data ./benchmarks/onos_201_adds.csv -gas 1
```

However this might take too long, or you may want some intermediate
results. You can truncate the file using, e.g. `head -100` or `head -1000`
to create a new file.

```
head -200 ./benchmarks/onos_201_adds.csv > ./benchmarks/onos_201_200_adds.csv
./motley onf -data ./benchmarks/onos_201_200_adds.csv -gas 1
```

If you get an `"OUT OF GAS"` failure, try increasing the value of the
`-gas` parameter.


# Writing and Running Tests

The current Testing framework uses `inline_tests`, which means you can write tests anywhere in your code. Currently all of the tests are localized in the `Test.ml` file. To write a test simply write

```
let%test _ = <boolean expression>
```
if the boolean expression returns `true`, the test passes, if it returns `false`, the test fails.

To run all of the tests written in the the library (`lib/`),  execute the following command

```
dune runtest
```
