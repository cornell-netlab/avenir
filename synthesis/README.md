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

+ Now youre ready to go! Open the library in `utop` by running `dune lib utop`




