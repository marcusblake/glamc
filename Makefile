# Compilers
OCAMLBUILD := ocamlbuild
CPP := g++
CFLAGS := -g -Wall
LDFLAGS := -g

SRC_DIR := ./runtime
OBJ_DIR := ./

SRC := $(wildcard $(SRC_DIR)/*.cpp)
OBJ := $(SRC:$(SRC_DIR)/%.cpp=$(SRC_DIR)/%.o)

FILENAME?=glamc

main: glamc.native $(OBJ_DIR)/libglamc.a

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

$(OBJ_DIR)/libglamc.a: $(OBJ)
	ar -crs $@ $(OBJ)
	ranlib $@

$(SRC_DIR)/%.o: $(SRC_DIR)/%.cpp $(SRC_DIR)/%.h
	$(CPP) -c $(CFLAGS) $< -o $@


.PHONY: clean

clean:
	ocamlbuild -clean
	rm -f $(SRC_DIR)/*.o *.native $(OBJ_DIR)/*.a a.out llvm.out*
