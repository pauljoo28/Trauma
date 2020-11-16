# Trauma

This is a Trauma. A programming language that is meant to prototype the concepts of differential dataflow outlined in this paper: http://michaelisard.com/pubs/differentialdataflow.pdf.

Some of the starting code was taken from pauljoo28/Imp

OCaml 4.04 or higher required

Set Up
------

We need [Dune][] and [Menhir][]:

    $ opam install dune
    $ apt-get install m4  # On Debian, for example.
    $ opam install menhir

Build by typing:

    $ make

Now you can use `dune exec bin/trauma.bc` to run the interpreter.
Or you can install a `trauma` executable:

    $ make build

Now `trauma` should be available on your path.
Simply run `trauma file.imp` to interpret that file.

[dune]: https://github.com/ocaml/dune
[menhir]: http://gallium.inria.fr/~fpottier/menhir/

Running
------

run `trauma tests/basic/*` for basic imp commands
run `trauma tests/differential_dataflow/*` for differential dataflow commands