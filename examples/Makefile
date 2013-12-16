SHELL=bash

PWD=$(shell pwd)
BASEDIR=$(PWD)/../src
BUILD=$(BASEDIR)/_build
# see http://stackoverflow.com/questions/14539899/using-external-libraries-with-ocamlbuild
OCAMLI_BYTE=-cflags '-I $(BUILD)' -lflags '-I $(BUILD) p3.cma'
OCAMLI_NATIVE=-cflags '-I $(BUILD)' -lflags '-I $(BUILD) p3.cmxa'

all: actions grammars lexer modular_opsem ocaml_parser

actions: FORCE
	cd actions && make

grammars: FORCE
	$(BASEDIR)/earley3_main.native -g grammars/E_EEE.g -f inputs/1_0010.txt

lexer: FORCE
	ocamlbuild -I lexer arith.ml
	ocamlbuild -I lexer $(OCAMLI_BYTE) lexer_main.byte

modular_opsem: FORCE
	ocamlbuild -I modular_opsem $(OCAMLI_NATIVE) mod_opsem.native

ocaml_parser: FORCE
	cd ocaml_parser && make

clean: FORCE
	ocamlbuild -clean
	cd actions && make clean
	cd ocaml_parser && make clean

FORCE:
