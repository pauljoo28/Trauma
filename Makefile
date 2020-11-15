.PHONY: all clean repl run build

all:
	dune exec bin/trauma.bc

build:
	dune build bin/trauma.bc
	dune build && dune install

clean:
	dune clean
