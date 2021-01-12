all: avenir

format: 
	opam install ocamlformat || true
	dune build @fmt --auto-promote

_build/default/bin/main.exe:
	dune build bin/main.exe 

avenir: _build/default/bin/main.exe
	cp $< $@

clean:
	rm -fr _build

rebuild: clean avenir
