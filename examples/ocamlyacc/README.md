This directory implements an OCaml parser using the standard ocaml
grammar.

  * `ocaml.parser.mly` - the original parser from OCaml
  * `ocamlyacc.txt` - a slightly tweaked version of the above, to make
    it easier to parse (use double braces for actions that themselves
    include a brace)

We need to process this grammar file, so we introduce a grammar for
ocamlyacc files:

  * `ocamlyacc.g` - parser for .mly files
  
With this, we can use P3 to build `ocamlyacc.native`. At this point,
we can process `ocamlyacc.txt` to produce a P3 version of the ocaml
grammar

  * `ocaml.g` - P3 version of the OCaml grammar
  
Processing this via P3 gives `ocaml.native`, a P3 parser for OCaml
(that doesn't actually print anything).

Finally, the `make test` command uses the `ocaml.native` executable to
process some OCaml code.

--

  * `ocaml_parser.ml` - avoids inefficiencies with strings
