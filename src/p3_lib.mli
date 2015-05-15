type 'a ty_substring = 'a P3_core.ty_substring


type term
type nonterm

type symbol = [ `NT of nonterm | `TM of term ]

type 'string input
type ('string,'a) output

type ('string,'a) parser3 = ('string input -> ('string,'a) output)

val ( ***> ): ('string,'a) parser3 -> ('string,'b) parser3 -> ('string,'a*'b) parser3

val ( |||| ) : ('string,'a) parser3 -> ('string,'a) parser3 -> ('string,'a) parser3

val ( >>>> ) : ('string,'a) parser3 -> ('a -> 'b) -> ('string,'b) parser3

val a: string -> (string,string ty_substring) parser3

val mkntparser: string -> ('a,'b) parser3 -> ('a,'b) parser3

type 'a outm

val grammar_of_parser: ('a,'b) parser3 -> 'a outm  (* FIXME remove from interface *)

val p3_run_parser_string: (string,'b)parser3 -> string -> 'b list

type 'a local_context = 'a P3_core.local_context

val memo_p3: (int local_context * int ty_substring,('string,'a)output) Hashtbl.t -> ('string,'a)parser3 -> ('string,'a)parser3

val string_of_symbol: symbol -> string

val sym_of_parser: ('a,'b)parser3 -> symbol
