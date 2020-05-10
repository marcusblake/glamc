# Compilers
OCAMLBUILD = ocamlbuild
CC=gcc
CFLAGS= -g -Wall
LDFLAGS= -g

FILENAME?=glamc

main: glamc.native 

glamc.native:
	$(OCAMLBUILD) -use-ocamlfind -pkgs llvm glamc.native

scanner:
	$(OCAMLBUILD) -use-ocamlfind scanner.native

parser:
	$(OCAMLBUILD) -use-ocamlfind parser.native

semant:
	$(OCAMLBUILD) -use-ocamlfind semant.native

custom:
	$(OCAMLBUILD) -use-ocamlfind $(FILENAME).native

.PHONY: clean

clean:
	ocamlbuild -clean
	rm -f *.o *.native *.a a.out
