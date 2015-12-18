include $(shell ocamlc -where)/Makefile.config

PACKAGE=ocamllint
OBJ=ppx_lint.cma ppx_lint.cmxa ppx_lint.native
INSTALL=META $(addprefix _build/, $(OBJ))

.PHONY: all install uninstall clean

all:
	ocamlbuild -use-ocamlfind $(OBJ) ppx_lint.native

check:
	ocamlbuild -use-ocamlfind tests.native --

install: uninstall
	ocamlfind install $(PACKAGE) $(INSTALL)

uninstall:
	ocamlfind remove $(PACKAGE)

clean:
	ocamlbuild -clean

tests.cov:
	rm -f tests.native
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native
	mv tests.native $@

cov: tests.cov
	./tests.cov
	cd _build ; bisect-ppx-report -summary-only -text /dev/stdout ../bisect000*.out ; cd ..
