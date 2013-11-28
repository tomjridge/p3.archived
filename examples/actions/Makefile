SHELL=bash

OCAMLOPT=ocamlopt.opt

# FIXME would like to avoid going via _build
# FIXME this all looks quite horrible - what is the right way to build ocaml programs spread across multiple dirs?
BASEDIR=../../src
BUILD=$(BASEDIR)/_build
GEN=$(BASEDIR)/p3_gen.native

LIBS=p3.cmxa #$(BUILD)/p3.cmxa #$(BUILD)/p1_lib.cmxa 
OCAMLI=-I $(BUILD) -I $(BUILD)/p1 -I $(BUILD)/earley -I $(BUILD)/p3 $(LIBS)
OCAMLI=-I $(BUILD) $(LIBS)
# OCAMLI=$(LIBS)

G2G=../../src/g2g.native

# working tests:
# excel.native 
NATIVES=aho_s.native aho_sml.native brackets3.native E_EEE.native S_xSx.native disambiguate.native arith.native arith2.native arith3.native length.native S_SSS.native precedence.native # FIXME hol.native h2l.native h2lpt.native 

# FIXME g.native

# not working: ml.native xmlprsr.native 

# FIXME
all: natives #h2lpt.native h2l.native

natives: $(NATIVES)

# FIXME
h2lpt.g: h2l.g
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


#	./hol.native -f hol.txt FIXME


#	./g.native -f g.g
# 	./ocaml.native -f ocaml.txt

notworking:
	./ml.native -f ml.txt
	./xmlprsr.native -f xmlprsr.xml

%.native: %.ml # $(LIBS)
	$(OCAMLOPT) -o $@ $(OCAMLI) $<

%.ml: %.g $(GEN)
	$(GEN) -g $< >$*.ml
#	$(GEN) -basedir $(BASEDIR) -g $< >$*.ml

$(GEN):
	echo "Please build $@ first."
	false

clean: FORCE
	rm -f {aho_s,aho_sml,brackets,brackets3,E_EEE,S_xSx,arith,arith2,arith3,bracket,disambiguate,excel,g,hol,h2l,h2lpt,length,ml,ml2,ocaml,precedence,S_SSS,S_SSS2,tmparith,xmlprsr}.{cmi,cmx,ml,o,native}
	rm -f *.native
	ocamlbuild -clean

.PRECIOUS: %.ml

FORCE:
