SHELL=bash

OCAMLOPT=ocamlopt.opt
COMPFLAGS=-w -26-8

BUILD=../../build
GEN=$(BUILD)/p3_gen.native
G2G=$(BUILD)/g2g.native

LIBS=p3.cmxa
OCAMLI=-I $(BUILD) $(LIBS)
P3CMXA=$(BUILD)/p3.cmxa

# working tests:
NATIVES=aho_s.native aho_sml.native brackets3.native E_EEE.native			\
S_xSx.native disambiguate.native arith.native arith2.native						\
arith3.native length.native S_SSS.native precedence.native 

# FIXME hol.native h2l.native h2lpt.native # excel.native # FIXME
# g.native not working: ml.native xmlprsr.native


all: natives #h2lpt.native h2l.native
.PHONY:: all

natives: $(NATIVES)
.PHONY:: natives

# FIXME
h2lpt.g: h2l.g $(G2G)
	$(G2G) $< >$@

# 	./excel.native -f excel.txt
test: $(NATIVES)
	./arith.native -f arith.txt
	./arith2.native -f arith2.txt
	./arith3.native -f arith3.txt
	./disambiguate.native -f disambiguate.txt
	./length.native -f length.txt
	./aho_s.native -f ../inputs/x_0006.txt
	./aho_sml.native -f ../inputs/x_0006.txt
	./brackets3.native -f ../inputs/brackets3_10.txt
	./E_EEE.native -f ../inputs/1_0010.txt
	./S_xSx.native -f ../inputs/1_0501.txt
	./S_SSS.native -f ../inputs/1_0100.txt
	./precedence.native -f arith3.txt
.PHONY:: test

#	./hol.native -f hol.txt FIXME
#	./g.native -f g.g
# 	./ocaml.native -f ocaml.txt
# notworking:
# 	./ml.native -f ml.txt
# 	./xmlprsr.native -f xmlprsr.xml

%.ml: %.g $(GEN) $(P3CMXA)
	$(GEN) -g $< >$*.ml
#	$(GEN) -basedir $(BASEDIR) -g $< >$*.ml

%.native: %.ml
	$(OCAMLOPT) $(COMPFLAGS) -o $@ $(OCAMLI) $<

$(GEN):
	echo "Please build $@ first."
	false

$(G2G):
	echo "Please build $@ first."
	false

clean: FORCE
	rm -f *.cm[iox] *.ml *.o *.native

.PRECIOUS: %.ml

FORCE:
