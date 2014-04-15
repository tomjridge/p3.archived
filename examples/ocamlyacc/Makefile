SHELL=bash

# require this lib from ocaml std distribution
COMPILERLIBS=$(shell ocamlc -where)/compiler-libs

CAMLC=ocamlc
CAMLOPT=ocamlopt

PWD=$(shell pwd)
BUILD=$(PWD)/../../build
GEN=$(BUILD)/p3_gen.native

CAMLINCLUDES=-I $(BUILD) p3.cma -I $(COMPILERLIBS) ocamlcommon.cma
CAMLOPTINCLUDES=-I $(BUILD) p3.cmxa -I $(COMPILERLIBS) ocamlcommon.cmxa

NATIVES=ocamlyacc.native ocaml.native ocaml_parser.native

all: beforeall ocaml_parser.native test

beforeall: $(GEN) $(BUILD)/p3.cmxa $(BUILD)/p3.cma

test: ocaml_parser.native
	./ocaml_parser.native -f test_ml

ocaml_parser.native: ocaml_parser.cmx
	$(CAMLOPT) -o $@ unix.cmxa -I +str $(CAMLOPTINCLUDES) $<

ocamlyacc.native: ocamlyacc.cmx
	$(CAMLOPT) -o $@ -I +str $(CAMLOPTINCLUDES) $<

ocaml.native: ocaml.cmx
	$(CAMLOPT) -o $@ -I +str $(CAMLOPTINCLUDES) $<


ocaml.g: ocamlyacc.native
	echo '{{ let fIXME = fun x -> failwith "FIXME" }}' >$@
	./ocamlyacc.native -f ocamlyacc.txt >>$@
	echo >>$@
	echo 'ERROR -> ?FIXME? {{ fIXME }} (* FIXME really should remove rules containing ERROR *)' >>$@
# in the above line, we may want to just strip out all rules for dealing with errors - this is an ocamlyacc thing; only reduces time from 10.78 to 8.78
	sed -i -e "/ERROR/d" $@


ocaml.ml: ocaml.g p3pre.ml p3mid.ml p3post.ml
	$(GEN) -basedir . -g $< >$@

%.ml: %.g
	$(GEN) -g $< >$*.ml

# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(CAMLINCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(CAMLINCLUDES) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) $(CAMLOPTINCLUDES) -c $<

depend:
	ocamldep *.ml *.mli >.depend

include .depend



clean: FORCE
	rm -f *.cm[iox] *.o
	rm -f $(NATIVES)
	rm -f ocamlyacc.ml
	rm -f ocaml.g

.PRECIOUS: %.ml

FORCE:
