MODULES=collection trace
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
OCAMLBUILD=ocamlbuild -use-ocamlfind
TEST=tests.byte

build-test:
	$(OCAMLBUILD) $(OBJECTS)

unit-test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./_build/$(TEST)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private