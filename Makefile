all:
	cd build && $(MAKE)
	cd examples && $(MAKE)

clean:
	cd build && $(MAKE) clean
#	cd examples && $(MAKE) clean


# ocamlfind, opam etc

install: ocamlfind_install

ocamlfind_install:
	cd build && $(MAKE)
	ocamlfind install p3 META build/*.cmi  build/p3.cma build/p3.cmxa build/p3.a

ocamlfind_remove:
	ocamlfind remove p3

# note opam also install binary build/p3_gen.native as p3_gen
# opam_install: ocamlfind_install


# opam_remove: # nothing to do - opam removes p3_gen binary automatically
