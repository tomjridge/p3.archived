type 'a ty_substring = 'a P3_core.ty_substring

type term = P3_core.term
type nonterm = P3_core.nonterm
type symbol = P3_core.symbol

type 'string input = 'string P3_core.input

type ('string,'a) output = ('string,'a) P3_core.output

type ('string,'a) parser3 = ('string,'a) P3_core.parser3

let ( ***> ) = P3_core.( ***> )

let ( |||| ) = P3_core.( |||| )

let ( >>>> ) = P3_core.( >>>> )

let a = P3_extra.P3_basic_parsers.a

let mkntparser = P3_core.mkntparser

type 'a outm = 'a P3_core.outm

let grammar_of_parser = P3_core.grammar_of_parser

let p3_run_parser_string = P3_core.p3_run_parser_string

type 'a local_context = 'a P3_core.local_context

let memo_p3 = P3_extra.P3_memo.memo_p3

let string_of_symbol = P3_core.string_of_symbol

let sym_of_parser = P3_core.sym_of_parser
