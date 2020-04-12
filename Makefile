# Compilers
OCAMLBUILD = ocamlbuild
CC=gcc
CFLAGS= -g -Wall
LDFLAGS= -g

FILENAME?=glamc

main: glamc.native 

glamc.native:
	$(OCAMLBUILD) -use-ocamlfind glamc.native -pkgs llvm

scanner:
	$(OCAMLBUILD) -use-ocamlfind scanner.native

parser:
	$(OCAMLBUILD) -use-ocamlfind parser.native

custom:
	$(OCAMLBUILD) -use-ocamlfind $(FILENAME).native

.PHONY: clean

clean:
	ocamlbuild -clean
	rm -f *.o *.native *.a a.out
