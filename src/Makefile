SHELL=bash

OCAMLBUILD=ocamlbuild

all: all_in_one

# make links to all ml files so that they appear in current directory
# this is essentially so that ocamlbuild keeps files in _build, not
# subdirectories of _build, which makes it easier to reference
# e.g. p3.cmxa and associated .cmi files from other projects
all_in_one: FORCE 
	rm -f *.ml
	find p1 earley p3_v2 misc earley2 -maxdepth 1 -name "*.ml" -exec ln -s \{\} . \;
	$(OCAMLBUILD) earley2_main.native p3_examples.native p3.cma p3.cmxa p3_gen.native g2g.native

earley: FORCE
	$(OCAMLBUILD) -I earley earley/earley.byte

earley2: FORCE
	$(OCAMLBUILD) -I earley2 earley2/earley2.byte
	$(OCAMLBUILD) -Is p1,earley2 earley2/earley2_main.native

minip3: FORCE
	$(OCAMLBUILD) -Is earley,minip3 minip3/minip3.native

minip3_e2: FORCE
	$(OCAMLBUILD) -Is earley2,minip3_e2 minip3_e2/minip3_e2.native

p1: FORCE
	$(OCAMLBUILD) -I p1 p1/p1_lib.byte

p3: FORCE
	$(OCAMLBUILD) -Is p1,earley2 p3/p3_lib.byte p3/p3_examples.byte

p3_e1: FORCE
	$(OCAMLBUILD) -Is p1,earley p3_e1/p3_lib.byte

p3_v2: FORCE
	$(OCAMLBUILD) -Is p1,earley2 p3_v2/p3_lib.byte p3_v2/p3_examples.byte p3_v2/p3_gen.native

# export all ml files in a single directory
export: FORCE
	-mkdir export
	find p1 earley p3 -maxdepth 1 -name "*.ml" -exec cp \{\} export \;

doc: all_in_one FORCE
	$(OCAMLBUILD) p3.docdir/index.html

clean: 
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

