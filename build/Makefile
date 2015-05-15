SHELL:=bash

include ../Makefile.common

CAMLCINCLUDES:=-package str $(E3_PKG)
CAMLOPTINCLUDES:=-package str $(E3_PKG:.cma=.cmxa)
COMPFLAGS:=-w -8 -w -23

MLS:=$(filter-out interactive.ml, $(shell ocamlfind ocamldep -sort *.ml *.mli))
SRC_DIR:=../src

MLS_TO_EXEC:=p3_examples.ml # executables
IGNORE_FOR_LIB:=$(MLS_TO_EXEC) interactive.ml # don't want these in the lib
MLS_FOR_LIB:=$(filter-out $(IGNORE_FOR_LIB), $(MLS)) # p1_examples.ml is an executable
LIB:=p3


all: before_all
	$(MAKE) cmo cmx
	$(MAKE) $(LIB).cma $(LIB).cmxa $(MLS_TO_EXEC:.ml=.native)

# ../src_ext/e3/build/e3.cma ../src_ext/e3/build/e3.cmxa:
# 	cd ../src_ext/e3/build && make


include Makefile.include


clean: before_clean
	rm -rf ocamldoc orebuild.exe pending.ml.todo

