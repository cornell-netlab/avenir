# Getting Started

+ First make sure you have `opam` installed. Then in the `synthesis` directory, run

```
opam switch create . ocaml-base-compiler.4.07.1
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

+ This project needs [Z3 SMT solver](https://github.com/Z3Prover/z3) to be installed on your machine.
You should also add the Z3 library to  your path. In Mac, this can be achieved by setting the DYLD_LIBRARY_PATH variable:

```
export DYLD_LIBRARY_PATH=`opam config var z3:lib`
```

on GNU/Linux systems the same can be done via the LD_LIBRARY_PATH environment variable:

```
export LD_LIBRARY_PATH=`opam config var z3:lib`
```

# Running the code

To build the executable, run

```
dune build bin/main.exe
```

To build-and-run the executable, run

```
dune exec bin/main.exe <path/to/logical/program> <path/to/real/program>
```


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
