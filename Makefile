include src/include.mk

EXEC = bin/andowe

include Makefile.ocaml

DEBUG = -g
CAMLFLAGS = -I +camlp4 -pp camlp4o
LIBS = camlp4lib.cma

CAMLC = ocamlc -I src $(DEBUG) $(CAMLFLAGS)
CAMLOPT = ocamlopt -I src $(DEBUG) $(CAMLFLAGS)
CAMLDEP = ocamldep -I src $(CAMLFLAGS)
#CAMLYACC = ocamlyacc
# To add tracing to the compiler, run make with the option
# 	TRACE=--trace
CAMLYACC = menhir --explain $(TRACE)
