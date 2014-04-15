SHELL=bash

PWD=$(shell pwd)
BUILD=$(PWD)/../../build
GEN=$(BUILD)/p3_gen.native

# see http://stackoverflow.com/questions/14539899/using-external-libraries-with-ocamlbuild
OCAMLI_BYTE=-cflags '-I $(BUILD)' -lflags '-I $(BUILD) p3.cma'
OCAMLI_NATIVE=-cflags '-I $(BUILD)' -lflags '-I $(BUILD) p3.cmxa'

NATIVES=ocaml_to_latex.native ocaml_to_hol.native

all: beforeall $(NATIVES) test

beforeall: $(GEN)
.PHONY:: beforeall

test: $(NATIVES)
	./ocaml_to_latex.native -f ocaml.txt
	./ocaml_to_hol.native -f ocaml.txt

%.native: %.ml
	ocamlbuild $(OCAMLI_NATIVE) -lib str $@

%.ml: %.g
	$(GEN) -g $< >$*.ml

clean: FORCE
	ocamlbuild -clean
	rm -f {ocaml_to_latex,ocaml_to_hol}.{cmi,cmx,ml,o,native}

.PRECIOUS: %.ml

FORCE:
