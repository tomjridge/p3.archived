SHELL=bash

CAMLC=$(shell command -v ocamlc.opt || command -v ocamlc || echo missing_ocamlc)
CAMLOPT=$(shell command -v ocamlopt.opt || command -v ocamlopt || echo missing_ocamlopt)
CAMLDEP=$(shell command -v ocamldep.opt || command -v ocamldep || echo missing_ocamldep) -one-line
OCAMLDOC=$(shell command -v ocamldoc.opt || command -v ocamldoc || echo missing_ocamldoc)

# FIXME probably want to look at warning 8 a bit more closely
WARNINGS=-w -26-8
COMPFLAGS=$(WARNINGS)

P1=p1_terminal_parsers.cmo p1_core.cmo p1_everything.cmo p1_lib.cmo
E3=e3_core.cmo e3_mods.cmo e3_std.cmo e3_uni.cmo e3_dispatch.cmo earley3.cmo
P3=p3_core.cmo p3_extra.cmo p3_everything.cmo p3_lib.cmo
MINIP3=mini_p3_lib.cmo minip3.cmo
CMO=$(P1) $(E3) $(P3) # don't add MINIP3 - keep this separate

# natives which depend on p3.cmxa
NATIVES=earley3_main.native g2g.native p3_gen.native 

all: p3_examples.cmo p3.cma p3.cmxa $(NATIVES) minip3.native

p3.cma: $(CMO)
	$(CAMLC) -a -o $@ $(CMO)

p3.cmxa: $(CMO:.cmo=.cmx)
	$(CAMLOPT) -a -o $@ $(CMO:.cmo=.cmx)

minip3.native: mini_p3_lib.cmx minip3.cmx 
	$(CAMLOPT) $(COMPFLAGS) -o $@ mini_p3_lib.cmx minip3.cmx

$(NATIVES): %.native: %.ml p3.cmxa
	$(CAMLOPT) $(COMPFLAGS) -o $@ p3.cmxa $<

# FIXME not used?
e3_std.mli: e3_std.ml
	$(CAMLC) -i $<

# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) -c $<

.ml.cmx:
	$(CAMLOPT) $(COMPFLAGS) -c $<


doc: $(P3)
	$(OCAMLDOC) -html $(P3:.cmo=.ml)


depend:
	$(CAMLDEP) $(INCLUDES) *.mli *.ml >.depend

include .depend



clean:
	rm -f *.cm[iox]
	rm -f *.cma *.cmxa *.a *.o *.cmi *.native
	rm -f *.{html,css}
