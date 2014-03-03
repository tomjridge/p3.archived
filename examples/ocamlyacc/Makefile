SHELL=bash

COMPILERLIBS=$(shell ocamlc -where)/compiler-libs

PWD=$(shell pwd)
BASEDIR=$(PWD)/../../src
BUILD=$(BASEDIR)/_build
# see http://stackoverflow.com/questions/14539899/using-external-libraries-with-ocamlbuild
OCAMLI_BYTE=-cflags '-I $(BUILD) -I $(COMPILERLIBS)' -lflags '-I $(BUILD) p3.cma -I $(COMPILERLIBS) ocamlcommon.cma'
OCAMLI_NATIVE=-cflags '-I $(BUILD) -I $(COMPILERLIBS)' -lflags '-I $(BUILD) p3.cmxa -I $(COMPILERLIBS) ocamlcommon.cmxa'

GEN=$(BASEDIR)/p3_gen.native

NATIVES=ocamlyacc.native ocaml.native

all: $(NATIVES) test

test: $(NATIVES)
	./ocaml.native -f ../modular_opsem/mod_opsem.ml

ocaml.g: ocamlyacc.native
	echo '{{ let fIXME = fun x -> failwith "FIXME" }}' >$@
	./ocamlyacc.native -f ocamlyacc.txt >>$@
	echo >>$@
	echo 'ERROR -> ?FIXME? {{ fIXME }} (* FIXME really should remove rules containing ERROR *)' >>$@
# in the above line, we may want to just strip out all rules for dealing with errors - this is an ocamlyacc thing; only reduces time from 10.78 to 8.78
	sed -i -e "/ERROR/d" $@

%.native: %.ml
	ocamlbuild $(OCAMLI_NATIVE) -lib str $@

ocaml.ml: ocaml.g p3pre.ml p3mid.ml p3post.ml
	$(GEN) -basedir . -g $< >$@

%.ml: %.g
	$(GEN) -g $< >$*.ml

clean: FORCE
	ocamlbuild -clean
	rm -f ocamlyacc.{cmi,cmx,ml,o,native}
	rm -f ocaml.{g,ml,native}

.PRECIOUS: %.ml

FORCE:
