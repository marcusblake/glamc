# Compilers
OCAMLBUILD = ocamlbuild
CC=gcc
CFLAGS= -g -Wall
LDFLAGS= -g

SCANNER?=scanner
PARSER?=parser

main: glamc.native 

glamc.native:
	$(OCAMLBUILD) -use-ocamlfind glamc.native -pkgs llvm

scanner:
	$(OCAMLBUILD) -use-ocamlfind $(SCANNER).native

parser:
	$(OCAMLBUILD) -use-ocamlfind $(PARSER).native


.PHONY: clean

clean:
	ocamlbuild -clean
	rm -f *.o *.native *.a a.out
