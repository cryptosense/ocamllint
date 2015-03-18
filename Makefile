include $(shell ocamlc -where)/Makefile.config

PACKAGE=ocamllint
OBJ=ppx_lint.cma ppx_lint.cmxa
INSTALL=META $(addprefix _build/, $(OBJ))

.PHONY: all install uninstall clean

all:
	ocamlbuild -use-ocamlfind $(OBJ) ppx_lint.native

install:
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	ocamlbuild -clean
