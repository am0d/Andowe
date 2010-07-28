include src/include.mk

EXEC = bin/andowe

include Makefile.ocaml

CAMLFLAGS = -pp camlp4o

CAMLC = ocamlc -I src $(CAMLFLAGS)
CAMLOPT = ocamlopt -I src $(CAMLFLAGS)
CAMLDEP = ocamldep -I src $(CAMLFLAGS)
