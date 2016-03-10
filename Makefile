include $(shell ocamlc -where)/Makefile.config

PACKAGE=ocamllint
OBJ=$(PACKAGE).cma $(PACKAGE).cmxa ppx_lint.cma ppx_lint.cmxa ppx_lint.native
INSTALL=META $(addprefix _build/, $(OBJ)) config.mli plugin.mli warning.mli _build/ocamllint.cmi

.PHONY: all install uninstall clean cov cov-html example

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

tests.cov: tests.ml
	rm -f tests.native
	ocamlbuild -use-ocamlfind -package bisect_ppx tests.native
	mv tests.native $@

cov: tests.cov
	rm -f ../bisect000*.out
	./tests.cov -runner sequential
	cd _build ; bisect-ppx-report -summary-only -text /dev/stdout ../bisect000*.out ; cd ..

cov-html: tests.cov
	rm -f ../bisect000*.out
	./tests.cov -runner sequential
	cd _build ; bisect-ppx-report -html cov ../bisect000*.out ; cd ..

example:
	ocamlbuild -use-ocamlfind example_plugin.cmxs
