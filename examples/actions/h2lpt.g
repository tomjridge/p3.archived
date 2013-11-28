{{

(* we need to define some raw parsers, but we want to use the existing
   Combinator and BasicParsers; Combinator is already opened by
   Everything *)
open P1_lib.P1_terminal_parsers.RawParsers

  let parse_epsws = (parse_while (fun s -> s = " " || s = "\n" || s="\t"))

let myparse_ruleheader = ((((a "/*") **>@ (until_a "*/") **>@ (a "*/"))) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z])))

let myparse_comm = fun i -> (((a "(*") **>@ until_a "*)" **>@ (a "*)")) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z]))) i

let rec myparse_wscomm = fun i -> ((parse_epsws)
   |||@ ((parse_epsws **>@ myparse_comm **>@ myparse_wscomm) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z] )))) i

let myparse_ident' = (
  let pred = fun c -> 
    ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
    || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    || (c = "_") || (c = "'")
  in
  noteps(parse_while pred))

(* FIXME we don't want to allow ' as an ident *)

let myparse_ident = fun i -> 
  let rs = myparse_ident' i in
  List.filter (fun (x,s) -> not (mem (content x) ["let";"in";"if";"then";"else";"case";"of";"'";"NOTIN";"with";"IN'";"INTER"])) rs  

(* here we alter RawParsers.term_to_parser to include our new terminal
   parsers; NB term_to_parser is already bound to
   RawParsers.term_to_parser at this point *)

let term_to_parser s = (match s with 
  | "?ruleheader?" -> (myparse_ruleheader)
  | "?w?" -> (myparse_wscomm)
  | "?epsws?" -> (myparse_wscomm)
  | "?ws?" -> (myparse_wscomm)
  | "?ident?" -> (myparse_ident)
  | _ -> (P1_lib.P1_terminal_parsers.RawParsers.term_to_parser s))

let c = content

let id = fun x -> x

}}
START -> ?epsws? NETSEM ?epsws? ?EOF? {{fun x -> let r = ( fun (x0,(x1,(x2,x3))) -> NODE("START",[LF("?epsws?",x0);x1;LF("?epsws?",x2);LF("?EOF?",x3)]) ) x in print_endline (string_of_pt r); print_newline (); r}}

XSTART -> ?epsws? EXP ?epsws? ?EOF? {{ fun (x0,(x1,(x2,x3))) -> NODE("XSTART",[LF("?epsws?",x0);x1;LF("?epsws?",x2);LF("?EOF?",x3)]) }}

NETSEM -> "(" FORALLS ?epsws? RULENAME ?epsws? RULEHEADER ?epsws? HOST ?epsws? LBL ?epsws? HOST ?epsws? "<==" ?epsws? COND ?epsws? ")" {{ fun (x0,(x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,(x10,(x11,(x12,(x13,(x14,(x15,(x16,x17))))))))))))))))) -> NODE("NETSEM",[LF("\"(\"",x0);x1;LF("?epsws?",x2);x3;LF("?epsws?",x4);x5;LF("?epsws?",x6);x7;LF("?epsws?",x8);x9;LF("?epsws?",x10);x11;LF("?epsws?",x12);LF("\"<==\"",x13);LF("?epsws?",x14);x15;LF("?epsws?",x16);LF("\")\"",x17)]) }}

FORALLS -> "!" ?epsws? IDENTS ?epsws? "." {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("FORALLS",[LF("\"!\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\".\"",x4)]) }}

RULENAME -> ?ident? {{ fun x0 -> NODE("RULENAME",[LF("?ident?",x0)]) }}

RULEHEADER -> "/*" ?epsws? RULEPROTO ?epsws? "," ?epsws? RULECAT ?epsws? "*/" {{ fun (x0,(x1,(x2,(x3,(x4,(x5,(x6,(x7,x8)))))))) -> NODE("RULEHEADER",[LF("\"/*\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\",\"",x4);LF("?epsws?",x5);x6;LF("?epsws?",x7);LF("\"*/\"",x8)]) }}

RULEPROTO -> ?ident? {{ fun x0 -> NODE("RULEPROTO",[LF("?ident?",x0)]) }}

RULECAT -> IDENT {{ fun x0 -> NODE("RULECAT",[x0]) }}
| IDENT ?epsws? IDENT {{ fun (x0,(x1,x2)) -> NODE("RULECAT",[x0;LF("?epsws?",x1);x2]) }}
| IDENT ?epsws? IDENT ?epsws? IDENT {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("RULECAT",[x0;LF("?epsws?",x1);x2;LF("?epsws?",x3);x4]) }}

HOST -> EXP {{ fun x0 -> NODE("HOST",[x0]) }}

LBL -> "--" ?w? EXP ?w? "--=>" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("LBL",[LF("\"--\"",x0);LF("?w?",x1);x2;LF("?w?",x3);LF("\"--=>\"",x4)]) }}

COND -> EXP {{ fun x0 -> NODE("COND",[x0]) }}

IDENT -> ?ident? {{ fun x0 -> NODE("IDENT",[LF("?ident?",x0)]) }}

PREFIX -> "~" ?epsws? EXP {{ fun (x0,(x1,x2)) -> NODE("PREFIX",[LF("\"~\"",x0);LF("?epsws?",x1);x2]) }}

IDENTS -> ?ident? {{ fun x0 -> NODE("IDENTS",[LF("?ident?",x0)]) }}
| ?ident? ?epsws? IDENTS {{ fun (x0,(x1,x2)) -> NODE("IDENTS",[LF("?ident?",x0);LF("?epsws?",x1);x2]) }}

TYPE -> ?ident? {{ fun x0 -> NODE("TYPE",[LF("?ident?",x0)]) }}
| "'" ?ident? {{ fun (x0,x1) -> NODE("TYPE",[LF("\"'\"",x0);LF("?ident?",x1)]) }}
| TYPE ?epsws? "->" ?epsws? TYPE {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("TYPE",[x0;LF("?epsws?",x1);LF("\"->\"",x2);LF("?epsws?",x3);x4]) }}
| TYPE ?ws? ?ident? {{ fun (x0,(x1,x2)) -> NODE("TYPE",[x0;LF("?ws?",x1);LF("?ident?",x2)]) }}

BRACKET -> "(" ")" {{ fun (x0,x1) -> NODE("BRACKET",[LF("\"(\"",x0);LF("\")\"",x1)]) }}
| "(" ?epsws? EXP ?epsws? ")" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("BRACKET",[LF("\"(\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\")\"",x4)]) }}
| "(" ?epsws? COMMAS ?epsws? ")" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("BRACKET",[LF("\"(\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\")\"",x4)]) }}
| "(" EXP "," ?epsws? EXP "," ?epsws? EXP ")" {{ fun (x0,(x1,(x2,(x3,(x4,(x5,(x6,(x7,x8)))))))) -> NODE("BRACKET",[LF("\"(\"",x0);x1;LF("\",\"",x2);LF("?epsws?",x3);x4;LF("\",\"",x5);LF("?epsws?",x6);x7;LF("\")\"",x8)]) }}

COMMAS -> EXP {{ fun x0 -> NODE("COMMAS",[x0]) }}
| EXP ?epsws? "," ?epsws? COMMAS {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("COMMAS",[x0;LF("?epsws?",x1);LF("\",\"",x2);LF("?epsws?",x3);x4]) }}

EXPLIST -> "" {{ fun x0 -> NODE("EXPLIST",[LF("\"\"",x0)]) }}
| EXP {{ fun x0 -> NODE("EXPLIST",[x0]) }}
| EXP ?w? ";" ?w? EXPLIST {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXPLIST",[x0;LF("?w?",x1);LF("\";\"",x2);LF("?w?",x3);x4]) }}

INFIX -> "=" {{ fun x0 -> NODE("INFIX",[LF("\"=\"",x0)]) }}
| "++" {{ fun x0 -> NODE("INFIX",[LF("\"++\"",x0)]) }}
| "::" {{ fun x0 -> NODE("INFIX",[LF("\"::\"",x0)]) }}
| "/\" {{ fun x0 -> NODE("INFIX",[LF("\"/\\\"",x0)]) }}
| "\/" {{ fun x0 -> NODE("INFIX",[LF("\"\\/\"",x0)]) }}
| "==>" {{ fun x0 -> NODE("INFIX",[LF("\"==>\"",x0)]) }}
| "+" {{ fun x0 -> NODE("INFIX",[LF("\"+\"",x0)]) }}
| "-" {{ fun x0 -> NODE("INFIX",[LF("\"-\"",x0)]) }}
| "/ " {{ fun x0 -> NODE("INFIX",[LF("\"/ \"",x0)]) }}
| "*" {{ fun x0 -> NODE("INFIX",[LF("\"*\"",x0)]) }}
| "<<" {{ fun x0 -> NODE("INFIX",[LF("\"<<\"",x0)]) }}
| "<" {{ fun x0 -> NODE("INFIX",[LF("\"<\"",x0)]) }}
| ">" {{ fun x0 -> NODE("INFIX",[LF("\">\"",x0)]) }}
| "<=" {{ fun x0 -> NODE("INFIX",[LF("\"<=\"",x0)]) }}
| ">=" {{ fun x0 -> NODE("INFIX",[LF("\">=\"",x0)]) }}
| "<>" {{ fun x0 -> NODE("INFIX",[LF("\"<>\"",x0)]) }}
| "|++" {{ fun x0 -> NODE("INFIX",[LF("\"|++\"",x0)]) }}
| "|+" {{ fun x0 -> NODE("INFIX",[LF("\"|+\"",x0)]) }}
| "andThen" {{ fun x0 -> NODE("INFIX",[LF("\"andThen\"",x0)]) }}
| "onlywhen" {{ fun x0 -> NODE("INFIX",[LF("\"onlywhen\"",x0)]) }}

FNARGS -> ATEXP ?epsws? ATEXP {{ fun (x0,(x1,x2)) -> NODE("FNARGS",[x0;LF("?epsws?",x1);x2]) }}
| ATEXP ?epsws? FNARGS {{ fun (x0,(x1,x2)) -> NODE("FNARGS",[x0;LF("?epsws?",x1);x2]) }}

CASES -> CASE {{ fun x0 -> NODE("CASES",[x0]) }}
| CASE ?ws? "||" ?ws? CASES {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("CASES",[x0;LF("?ws?",x1);LF("\"||\"",x2);LF("?ws?",x3);x4]) }}

CASE -> EXP ?ws? "->" ?ws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("CASE",[x0;LF("?ws?",x1);LF("\"->\"",x2);LF("?ws?",x3);x4]) }}
| "_" ?ws? "->" ?ws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("CASE",[LF("\"_\"",x0);LF("?ws?",x1);LF("\"->\"",x2);LF("?ws?",x3);x4]) }}

RECORD -> RECORDA {{ fun x0 -> NODE("RECORD",[x0]) }}
| ATEXP ?w? "with" ?w? RECORDA {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("RECORD",[x0;LF("?w?",x1);LF("\"with\"",x2);LF("?w?",x3);x4]) }}

ATEXP -> BRACKET {{ fun x0 -> NODE("ATEXP",[x0]) }}
| ?ident? {{ fun x0 -> NODE("ATEXP",[LF("?ident?",x0)]) }}
| ?Ident? {{ fun x0 -> NODE("ATEXP",[LF("?Ident?",x0)]) }}
| '"' ?notdquote? '"' {{ fun (x0,(x1,x2)) -> NODE("ATEXP",[LF("'\"'",x0);LF("?notdquote?",x1);LF("'\"'",x2)]) }}
| "[" ?epsws? EXPLIST ?epsws? "]" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("ATEXP",[LF("\"[\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\"]\"",x4)]) }}
| "{" ?epsws? EXPLIST ?epsws? "}" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("ATEXP",[LF("\"{\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\"}\"",x4)]) }}
| "{" ?epsws? EXP ?epsws? "|" ?epsws? EXP ?epsws? "}" {{ fun (x0,(x1,(x2,(x3,(x4,(x5,(x6,(x7,x8)))))))) -> NODE("ATEXP",[LF("\"{\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\"|\"",x4);LF("?epsws?",x5);x6;LF("?epsws?",x7);LF("\"}\"",x8)]) }}
| '"' "[]" '"' {{ fun (x0,(x1,x2)) -> NODE("ATEXP",[LF("'\"'",x0);LF("\"[]\"",x1);LF("'\"'",x2)]) }}
| '"' "::" '"' {{ fun (x0,(x1,x2)) -> NODE("ATEXP",[LF("'\"'",x0);LF("\"::\"",x1);LF("'\"'",x2)]) }}
| RECORD {{ fun x0 -> NODE("ATEXP",[x0]) }}
| ?num? {{ fun x0 -> NODE("ATEXP",[LF("?num?",x0)]) }}
| ATEXP "." ?ident? {{ fun (x0,(x1,x2)) -> NODE("ATEXP",[x0;LF("\".\"",x1);LF("?ident?",x2)]) }}
| ?ruleheader? {{ fun x0 -> NODE("ATEXP",[LF("?ruleheader?",x0)]) }}
| "$\/" {{ fun x0 -> NODE("ATEXP",[LF("\"$\\/\"",x0)]) }}

RECORDA -> "<|" ?epsws? FIELDS ?epsws? "|>" {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("RECORDA",[LF("\"<|\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\"|>\"",x4)]) }}

FIELDS -> "" {{ fun x0 -> NODE("FIELDS",[LF("\"\"",x0)]) }}
| FIELD {{ fun x0 -> NODE("FIELDS",[x0]) }}
| FIELD ?epsws? ";" ?epsws? FIELDS {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("FIELDS",[x0;LF("?epsws?",x1);LF("\";\"",x2);LF("?epsws?",x3);x4]) }}

FIELD -> ?ident? ?epsws? ":=" ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("FIELD",[LF("?ident?",x0);LF("?epsws?",x1);LF("\":=\"",x2);LF("?epsws?",x3);x4]) }}
| ?ident? ?epsws? "updated_by" ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("FIELD",[LF("?ident?",x0);LF("?epsws?",x1);LF("\"updated_by\"",x2);LF("?epsws?",x3);x4]) }}

EXP -> ATEXP {{ fun x0 -> NODE("EXP",[x0]) }}
| EXP ?epsws? INFIX ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);x2;LF("?epsws?",x3);x4]) }}
| FNARGS {{ fun x0 -> NODE("EXP",[x0]) }}
| "case" ?ws? EXP ?ws? "of" ?ws? CASES {{ fun (x0,(x1,(x2,(x3,(x4,(x5,x6)))))) -> NODE("EXP",[LF("\"case\"",x0);LF("?ws?",x1);x2;LF("?ws?",x3);LF("\"of\"",x4);LF("?ws?",x5);x6]) }}
| "if" ?ws? EXP ?ws? "then" ?ws? EXP ?ws? "else" ?ws? EXP {{ fun (x0,(x1,(x2,(x3,(x4,(x5,(x6,(x7,(x8,(x9,x10)))))))))) -> NODE("EXP",[LF("\"if\"",x0);LF("?ws?",x1);x2;LF("?ws?",x3);LF("\"then\"",x4);LF("?ws?",x5);x6;LF("?ws?",x7);LF("\"else\"",x8);LF("?ws?",x9);x10]) }}
| "let" ?ws? EXP ?ws? "in" ?ws? EXP {{ fun (x0,(x1,(x2,(x3,(x4,(x5,x6)))))) -> NODE("EXP",[LF("\"let\"",x0);LF("?ws?",x1);x2;LF("?ws?",x3);LF("\"in\"",x4);LF("?ws?",x5);x6]) }}
| "\" ?epsws? EXP ?epsws? "." ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,(x4,(x5,x6)))))) -> NODE("EXP",[LF("\"\\\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\".\"",x4);LF("?epsws?",x5);x6]) }}
| EXP ?epsws? ":" ?epsws? TYPE {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\":\"",x2);LF("?epsws?",x3);x4]) }}
| "!" ?epsws? IDENTS ?epsws? "." ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,(x4,(x5,x6)))))) -> NODE("EXP",[LF("\"!\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\".\"",x4);LF("?epsws?",x5);x6]) }}
| "?" ?epsws? IDENTS ?epsws? "." ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,(x4,(x5,x6)))))) -> NODE("EXP",[LF("\"?\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);LF("\".\"",x4);LF("?epsws?",x5);x6]) }}
| "get_sock" ?epsws? EXP {{ fun (x0,(x1,x2)) -> NODE("EXP",[LF("\"get_sock\"",x0);LF("?epsws?",x1);x2]) }}
| "get_cb" ?epsws? EXP {{ fun (x0,(x1,x2)) -> NODE("EXP",[LF("\"get_cb\"",x0);LF("?epsws?",x1);x2]) }}
| "chooseM" ?epsws? EXP ?epsws? EXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[LF("\"chooseM\"",x0);LF("?epsws?",x1);x2;LF("?epsws?",x3);x4]) }}
| PREFIX {{ fun x0 -> NODE("EXP",[x0]) }}
| ATEXP ?epsws? "'" ?epsws? ATEXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\"'\"",x2);LF("?epsws?",x3);x4]) }}
| ATEXP ?epsws? "NOTIN" ?epsws? ATEXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\"NOTIN\"",x2);LF("?epsws?",x3);x4]) }}
| ATEXP ?epsws? "IN'" ?epsws? ATEXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\"IN'\"",x2);LF("?epsws?",x3);x4]) }}
| ATEXP ?epsws? "\\" ?epsws? ATEXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\"\\\\\"",x2);LF("?epsws?",x3);x4]) }}
| ATEXP ?epsws? "INTER" ?epsws? ATEXP {{ fun (x0,(x1,(x2,(x3,x4)))) -> NODE("EXP",[x0;LF("?epsws?",x1);LF("\"INTER\"",x2);LF("?epsws?",x3);x4]) }}

