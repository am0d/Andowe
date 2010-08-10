include src/include.mk

EXEC = bin/andowe

include Makefile.ocaml

CAMLFLAGS = 

CAMLC = ocamlc -I src $(CAMLFLAGS)
CAMLOPT = ocamlopt -I src $(CAMLFLAGS)
CAMLDEP = ocamldep -I src $(CAMLFLAGS)
#CAMLYACC = ocamlyacc
CAMLYACC = menhir --explain $(TRACE) $(CAMLFLAGS)
