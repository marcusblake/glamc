# Compilers
OCAMLBUILD := ocamlbuild
OCAMLFLAGS := -use-ocamlfind -no-hygiene
CPP := g++
CFLAGS := -g -Wall
LDFLAGS := -g

SRC_DIR := ./runtime
OBJ_DIR := ./

SRC := $(wildcard $(SRC_DIR)/*.cpp)
OBJ := $(SRC:$(SRC_DIR)/%.cpp=$(SRC_DIR)/%.o)

FILENAME?=glamc

main: glamc.native $(OBJ_DIR)/libglamc.a

glamc.native: parser-driver
	$(OCAMLBUILD) $(OCAMLFLAGS) -pkgs llvm -pkgs menhirLib glamc.native

scanner:
	$(OCAMLBUILD) $(OCAMLFLAGS) scanner.native

parser: parser-driver
	$(OCAMLBUILD) $(OCAMLFLAGS) -pkgs menhirLib parse_driver.native

parser-driver:
	menhir --table parser.mly
	if [ ! -f parser.messages ]; then \
		menhir --list-errors parser.mly > parser.messages; \
	fi;
	menhir --compile-errors parser.messages parser.mly > parser_messages.ml

semant:
	$(OCAMLBUILD) $(OCAMLFLAGS) semant.native

custom:
	$(OCAMLBUILD) $(OCAMLFLAGS) $(FILENAME).native

$(OBJ_DIR)/libglamc.a: $(OBJ)
	ar -crs $@ $(OBJ)
	ranlib $@

$(SRC_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.h
	$(CPP) -c $(CFLAGS) $< -o $@


.PHONY: clean

clean:
	ocamlbuild -clean
	rm -f $(SRC_DIR)/*.o *.native $(OBJ_DIR)/*.a parser.ml parser.mli parser_messages.ml a.out llvm.out*
