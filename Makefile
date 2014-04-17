all:
	cd build && make
	cd examples && make

clean:
	cd build && make clean
	cd examples && make clean


# ocamlfind, opam etc

ocamlfind_install:
	cd build && make
	ocamlfind install p3 META build/*.cmi  build/p3.cma build/p3.cmxa build/p3.a

ocamlfind_remove:
	ocamlfind remove p3

# note opam also install binary build/p3_gen.native as p3_gen
# opam_install: ocamlfind_install


# opam_remove: # nothing to do - opam removes p3_gen binary automatically
