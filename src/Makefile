SHELL=bash

OCAMLBUILD=ocamlbuild

all: all_in_one_p3v6

# make links to all ml files so that they appear in current directory
# this is essentially so that ocamlbuild keeps files in _build, not
# subdirectories of _build, which makes it easier to reference
# e.g. p3.cmxa and associated .cmi files from other projects
all_in_one: FORCE 
	rm -f *.ml
	find p1 p3_v3 misc earley3_v3 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v3_earley3: FORCE 
	rm -f *.ml
	find p1 p3_v3 misc earley3 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v3_earley3_v2: FORCE 
	rm -f *.ml
	find p1 p3_v3 misc earley3_v2 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v3_earley3_v3: FORCE 
	rm -f *.ml
	find p1 p3_v3 misc earley3_v3 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v4: FORCE 
	rm -f *.ml
	find p1 p3_v4 misc earley3_v4 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v5: FORCE 
	rm -f *.ml
	find p1 p3_v5 misc earley3_v5 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

all_in_one_p3v6: FORCE 
	rm -f *.ml
	find p1 p3_v6 misc earley3_v6 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley3_main.native p3_examples.native p3_gen.native g2g.native p3.cma p3.cmxa 



all_in_one_e2: FORCE 
	rm -f *.ml
	find p1 p3_v2 misc earley2 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley2_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

earley1: FORCE
	$(OCAMLBUILD) earley1/earley.byte

earley2: FORCE
	$(OCAMLBUILD) earley2/earley2.byte
	$(OCAMLBUILD) -Is p1 earley2/earley2_main.native

earley3: FORCE
	$(OCAMLBUILD) earley3/earley3.byte
	$(OCAMLBUILD) -Is p1 earley3/earley3_main.native

earley3_v2: FORCE
	$(OCAMLBUILD) earley3_v2/earley3_fun.byte
	$(OCAMLBUILD) -Is p1 earley3_v2/earley3_main.native

earley3_v3: FORCE
	$(OCAMLBUILD) earley3_v3/earley3_imp.byte
	$(OCAMLBUILD) -Is p1 earley3_v3/earley3_main.native

earley3_v4: FORCE
	$(OCAMLBUILD) earley3_v4/earley3_imp.byte earley3_v4/earley3_fun.byte
#	$(OCAMLBUILD) -Is p1 earley3_v3/earley3_main.native

earley3_v5: FORCE
	$(OCAMLBUILD) earley3_v5/universe.byte
	$(OCAMLBUILD) -Is p1 earley3_v5/earley3_main.byte
	$(OCAMLBUILD) -Is p1 earley3_v5/earley3_main.native
#	$(OCAMLBUILD) earley3_v5/int_utils.byte earley3_v5/earley3_utils.byte
#	$(OCAMLBUILD) earley3_v5/earley3_int.byte
#	#	$(OCAMLBUILD) earley3_v5/earley3_fun.byte

earley3_v6: FORCE
	$(OCAMLBUILD) -Is p1 earley3_v6/e3_core.byte earley3_v6/earley3_main.native 
	echo "~~~{.ocaml}" >tmp.md
	cat earley3_v6/e3_core.ml >> tmp.md
	echo "~~~" >> tmp.md
	pandoc -s tmp.md > tmp.html

minip3_v1: FORCE
	$(OCAMLBUILD) -Is minip3_v1 minip3_v1/minip3.native

minip3_e2: FORCE
	$(OCAMLBUILD) -Is earley2 minip3_e2/minip3_e2.native

minip3_v2: FORCE
	$(OCAMLBUILD) -Is earley1 minip3_v2/minip3.native

p1: FORCE
	$(OCAMLBUILD) p1/p1_lib.byte

p1.cma: FORCE
	find p1 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) $@

p3_e1: FORCE
	$(OCAMLBUILD) -Is p1,earley1 p3_e1/p3_lib.byte

p3_v1: FORCE
	$(OCAMLBUILD) -Is p1,earley2 p3_v1/p3_lib.byte p3_v1/p3_examples.byte

p3_v2: FORCE
	$(OCAMLBUILD) -Is p1,earley2 p3_v2/p3_lib.byte p3_v2/p3_examples.byte p3_v2/p3_gen.native

p3_v3: FORCE
	$(OCAMLBUILD) -Is p1,earley3 p3_v3/p3_lib.byte p3_v3/p3_examples.byte p3_v3/p3_gen.native

p3_v4: FORCE
	$(OCAMLBUILD) -Is p1,earley3_v4 p3_v4/p3_lib.byte p3_v4/p3_examples.byte p3_v4/p3_gen.native p3.cmxa

p3_v5: FORCE
	$(OCAMLBUILD) -Is p1,earley3_v5 p3_v5/p3_lib.byte p3_v5/p3_examples.byte p3_v5/p3_gen.native 



# export all ml files in a single directory
export: FORCE
	-mkdir export
	find p1 earley2 p3_v2 -maxdepth 1 -name "*.ml" -exec cp \{\} export \;

doc: all_in_one_p3v6 FORCE
	$(OCAMLBUILD) p3.docdir/index.html

earley2.ml.md: FORCE
	echo "~~~{.ocaml}" >$@
	cat earley2/earley2.ml >> $@
	echo "~~~" >> $@

earley2.html: earley2.ml.md
	pandoc -s $< > $@


earley3.ml.md: FORCE
	echo "~~~{.ocaml}" >$@
	cat earley3/earley3.ml >> $@
	echo "~~~" >> $@

earley3.html: earley3.ml.md
	pandoc -s $< > $@



clean: 
	rm -f earley2.ml.md earley2.html
	rm -f p3.cmxa *.ml 
	rm -rf export
	$(OCAMLBUILD) -clean

FORCE:


# # for html documentation
# p1=p1_core.ml p1_everything.ml p1_lib.ml p1_terminal_parsers.ml
# e=earley.ml
# p3=p3_core.ml p3_everything.ml p3_extra.ml p3_lib.ml
# mls=$(p1) $(e) $(p3)
# 
# # for html doc, we need interface files and compiled object files
# byte: all_in_one FORCE
# 	MLS=`sed -e s/.ml/.byte/g <<< "$(mls)"`; echo $(OCAMLBUILD) $$MLS; $(OCAMLBUILD) $$MLS
# 
# html: byte FORCE
# 	ocamldoc -html -d _build -I _build $(mls)

# all: p1/p1_lib.cmx earley/earley.cmx earley/earley_main.byte p3/p3_gen.native p3/p3_lib.cmx p3.cmxa misc/g2g.native
# 
# extra: minip3/minip3.native minip3_e2/minip3_e2.native earley2/earley2.byte


# # we include FORCE because we want ocamlbuild to sort out the dependencies
# %.byte: %.ml FORCE
# 	ocamlbuild $(IS) $@
# 
# %.native: %.ml FORCE
# 	ocamlbuild $(IS) $@
# 
# %.cmx: FORCE
# 	ocamlbuild $(IS) $@
# 
# %.cma: FORCE
# 	ocamlbuild $(IS) $@
# 
# %.cmxa: FORCE
# 	ocamlbuild $(IS) $@

