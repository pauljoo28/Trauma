# Trauma

This is a Trauma. A programming language that is meant to prototype the concepts of differential dataflow outlined in this paper: http://michaelisard.com/pubs/differentialdataflow.pdf.

Setup is taken from pauljoo28/Imp.
------

A framework for a simple (IMP language)[https://www.cs.cornell.edu/courses/cs6110/2019sp/lectures/lec08.pdf] interpreter in OCaml.

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