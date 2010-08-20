include src/include.mk

EXEC = bin/andowe

include Makefile.ocaml

CAMLFLAGS = 

CAMLC = ocamlc -I src $(CAMLFLAGS)
CAMLOPT = ocamlopt -I src $(CAMLFLAGS)
CAMLDEP = ocamldep -I src $(CAMLFLAGS)
#CAMLYACC = ocamlyacc
# To add tracing to the compiler, run make with the option
# 	TRACE=--trace
CAMLYACC = menhir --explain $(TRACE) $(CAMLFLAGS)
