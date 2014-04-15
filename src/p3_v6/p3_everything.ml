(**
{1 P3_everything: include all definitions from other relevant modules}

The idea is to allow open P3_lib.P3_everything rather than opening individual modules.

*)


include P1_lib.P1_core.Prelude  
(* include P1_lib.P1_core.Types prefer p3 types e.g. for term *)
include P1_lib.P1_core.Substring
include P1_lib.P1_core.Common
(*  include RawParsers - lots of names that clash with eg BasicParsers and P3BasicParsers *)
include P1_lib.P1_core.Combinator
(*  include BasicParsers *)
include P1_lib.P1_core.Context
include P1_lib.P1_core.GrammarToParser
include P1_lib.P1_core.ParseGrammar
include P1_lib.P1_core.CommandLine
(* include EarleyTypes *)
(* include EarleyCore  *)
(* include Earley.Earley_interface *)
include P3_core
include P3_extra.P3_memo
(*  include P3BasicParsers *) (* we don't include the basic parsers, in case they clash with p1 basic parsers *)

