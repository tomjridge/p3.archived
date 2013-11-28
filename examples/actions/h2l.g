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


START -> ?epsws? NETSEM ?epsws? ?EOF?               {{ fun ((w1,(i,(w2,_)))) -> let _ = print_string ("Parsed an exp\n"^i^"\n\n") in i }} 

XSTART -> ?epsws? EXP ?epsws? ?EOF?               {{ fun ((w1,(i,(w2,_)))) -> let _ = print_string ("Parsed an exp\n"^i^"\n\n") in i }} 


   
    (* netsem host transitions *)
    NETSEM -> "(" FORALLS ?epsws? RULENAME ?epsws? RULEHEADER ?epsws? HOST ?epsws? LBL ?epsws? HOST ?epsws? "<==" ?epsws? COND ?epsws? ")"
      {{ fun (_,(fas,(w1,(rn,(w2,(rh,(w3,(h1,(w4,(lbl,(w5,(h2,(w6,(revimp,(w7,(c1,(w8,_))))))))))))))))) ->
(*         "("^fas^(c w1)^rn^(c w2)^rh^(c w3)^h1^(c w4)^lbl^(c w5)^h2^(c w6)^"<=="^(c w7)^c1^(c w8)^")" *)

rn^": "^""^fas^(c w1)^c1^(c w7)^"==>"^(c w6)^"host_redn "^rn^(c w2)^""^rh^""^(c w3)^"((* h1 *)"^h1^")"^(c w4)^"((* lbl *)"^lbl^")"^(c w5)^"((* h2 *)"^h2^")"^(c w8)^""


}}
    
    FORALLS ->
        "!" ?epsws? IDENTS ?epsws? "."                   {{ fun (_,(w1,(x,(w2,_)))) -> "forall "^(c w1)^(String.concat "" x)^(c w2)^"." }} 
    
    RULENAME -> ?ident? {{ c }}
    
    RULEHEADER -> "/*" ?epsws? RULEPROTO ?epsws? "," ?epsws? RULECAT ?epsws? "*/"
      {{ fun (_,(w1,(rp,(w2,(_,(w4,(rc,(w3,_)))))))) -> "((* rp *)"^(c w1)^rp^(c w2)^") "^(c w3)^"((* rc *)"^rc^")"^(c w4) }}
    
    RULEPROTO -> ?ident? {{ c }}
    
    RULECAT -> IDENT {{ id }}
      | IDENT ?epsws? IDENT {{ fun (e1,(w1,e2)) -> e1^(c w1)^e2 }}
      | IDENT ?epsws? IDENT ?epsws? IDENT {{ fun (e1,(w1,(e2,(w2,e3)))) -> e1^(c w1)^e2^(c w2)^e3 }}

    HOST -> EXP {{ id }}
    
    LBL ->   
      "--" ?w? EXP ?w? "--=>" {{ fun (_,(w1,(e1,(w2,_)))) -> e1 (* ""^(c w1)^e1^(c w2)^"" *) }}
    
    COND -> EXP {{ id }}

IDENT -> ?ident? {{ c }}


EXP -> 
    ATEXP                                                        {{ fun i -> i }}
  | EXP ?epsws? INFIX ?epsws? EXP                                {{ fun (e1,(w1,(i,(w2,e2)))) -> e1^(c w1)^i^(c w2)^e2 }} 
  | FNARGS                                                       {{ fun ss -> (String.concat "" ss) }}
  | "case" ?ws? EXP ?ws? "of" ?ws? CASES                         {{ fun (_,(w1,(e,(w2,(_,(w3,cs)))))) -> ("match"^(c w1)^e^(c w2)^"with"^(c w3)^(String.concat "|" cs)^" end") }}
  | "if" ?ws? EXP ?ws? "then" ?ws? EXP ?ws? "else" ?ws? EXP      {{ fun (_,(w1,(e1,(w2,(_,(w3,(e2,(w4,(_,(w5,(e3))))))))))) -> ("if"^(c w1)^e1^(c w2)^"then"^(c w3)^e2^(c w4)^"else"^(c w5)^e3^"") }}
  | "let" ?ws? EXP ?ws? "in" ?ws? EXP                            {{ fun (_,(w1,(e1,(w2,(_,(w3,e2)))))) -> "let"^(c w1)^e1^(c w2)^"in"^(c w3)^e2 }}
  | "\" ?epsws? EXP ?epsws? "." ?epsws? EXP                      {{ fun (_,(w1,(e1,(w2,(_,(w3,e2)))))) -> "fun "^(c w1)^e1^(c w2)^"->"^(c w3)^e2 }}
  | EXP ?epsws? ":" ?epsws? TYPE                                 {{ fun (e,(w1,(_,(w2,t)))) -> e^(c w1)^":"^(c w2)^t }} 
  | "!" ?epsws? IDENTS ?epsws? "." ?epsws? EXP                  {{ fun (_,(w1,(x,(w2,(_,(w3,e2)))))) -> "forall "^(c w1)^(String.concat "" x)^(c w2)^"."^(c w3)^e2 }} 
  | "?" ?epsws? IDENTS ?epsws? "." ?epsws? EXP                  {{ fun (_,(w1,(x,(w2,(_,(w3,e2)))))) -> "exists "^(c w1)^(String.concat "" x)^(c w2)^"."^(c w3)^e2 }} 
  | "get_sock" ?epsws? EXP {{ fun (_,(w1,e)) -> "get_sock"^(c w1)^e }} (* get_sock \ sock *)
  | "get_cb" ?epsws? EXP {{ fun (_,(w1,e)) -> "get_cb"^(c w1)^e }} (* get_sock \ sock *)
  | "chooseM" ?epsws? EXP ?epsws? EXP {{ fun (_,(w1,(e1,(w2,e2)))) -> "chooseM"^(c w1)^e1^(c w2)^e2 }}
  | PREFIX {{ fun i -> i }}
  | ATEXP ?epsws? "'" ?epsws? ATEXP  {{ fun (e1,(w1,(_,(w2,e2)))) -> "(FAPPLY "^e1^(c w1)^(c w2)^e2^")" }}
  | ATEXP ?epsws? "NOTIN" ?epsws? ATEXP  {{ fun (e1,(w1,(_,(w2,e2)))) -> "NOTIN "^e1^(c w1)^(c w2)^e2 }}
  | ATEXP ?epsws? "IN'" ?epsws? ATEXP  {{ fun (e1,(w1,(_,(w2,e2)))) -> "IN' "^e1^(c w1)^(c w2)^e2 }}
  | ATEXP ?epsws? "\\" ?epsws? ATEXP {{ fun (e1,(w1,(_,(w2,e2)))) -> "DOMSUB "^e1^(c w1)^(c w2)^e2 }}
  | ATEXP ?epsws? "INTER" ?epsws? ATEXP {{ fun (e1,(w1,(_,(w2,e2)))) -> e1^(c w1)^"inter"^(c w2)^e2 }}

PREFIX -> "~" ?epsws? EXP  {{ fun (_,(w1,e)) -> "not "^(c w1)^e }}

IDENTS -> ?ident? {{ fun s -> [c s] }}
  | ?ident? ?epsws? IDENTS {{ fun (s,(w,ss)) -> (c s)::(c w)::ss }}

TYPE -> ?ident?                                                  {{ fun s -> content s }}
  | "'" ?ident?                                                  {{ fun (_,s2) -> "'" ^ (content s2) }}
  | TYPE ?epsws? "->" ?epsws? TYPE                               {{ fun (t1,(w1,(_,(w2,t2)))) -> t1^(c w1)^"->"^(c w2)^t2 }}
  | TYPE ?ws? ?ident?                                            {{ fun (t,(w1,i)) -> t^(c w1)^(content i) }}

ATEXP -> 
  BRACKET                                                      {{ fun s -> s }}

  | ?ident?                                                      {{ fun s -> content s }}
  | ?Ident?                                                      {{ c }}
  | '"' ?notdquote? '"'                                          {{ fun (_,(s,_)) -> ("\"" ^ (content s) ^ "\"") }} 
  | "[" ?epsws? EXPLIST ?epsws? "]"                              {{ fun (_,(w1,(ss,(w2,_)))) -> "[" ^(c w1)^ (String.concat "" ss) ^ (c w2) ^ "]" }}
  | "{" ?epsws? EXPLIST ?epsws? "}"                              {{ fun (_,(w1,(ss,(w2,_)))) -> "{" ^(c w1)^ (String.concat "" ss) ^ (c w2) ^ "}" }}
  | "{" ?epsws? EXP ?epsws? "|" ?epsws? EXP ?epsws? "}"  
    {{ fun (_,(w1,(e1,(w2,(_,(w3,(e2,(w4,_)))))))) -> "{"^(c w1)^e1^(c w2)^"|"^(c w3)^e2^(c w4)^"}" }}
  | '"' "[]" '"'                                                 {{ fun _ -> "\"[]\"" }}
  | '"' "::" '"'                                                 {{ fun _ -> "\"::\"" (* not really an infix *) }}
  | RECORD                                                       {{ fun i -> i }}
  | ?num? {{ c }}
  | ATEXP "." ?ident? {{ fun (e,(_,f)) -> e^"."^(c f) }} (* field selection *)
  | ?ruleheader? {{ c }}
  | "$\/" {{ fun _ -> "prefix_or" }}

BRACKET -> 
    "(" ")"                                                      {{ fun _ -> "()" }}
  | "(" ?epsws? EXP ?epsws? ")"                                  {{ fun (_,(w1,(e,(w2,_)))) -> "("^(c w1)^e^(c w2)^")" }}
  | "(" ?epsws? COMMAS ?epsws? ")"                                  
    {{ fun (_,(w1,(e1,(w2,_)))) -> ("("^(c w1)^e1^(c w2)^")") }}
  | "(" EXP "," ?epsws? EXP "," ?epsws? EXP ")"                  {{ fun (_,(e1,(_,(w2,(e2,(_,(w3,(e3,_)))))))) -> ("("^e1^","^(c w2)^e2^","^(c w3)^e3^")") }}

COMMAS -> 
  EXP           {{ id }}
  | EXP ?epsws? "," ?epsws? COMMAS   {{ fun (e1,(w1,(_,(w2,e2)))) -> e1^(c w1)^","^(c w2)^e2 }}

(*
BAL -> "(" KET                                                   {{ fun (s1,s2) -> dest_Some (concatenate_two s1 s2) }}
KET -> ?notbracket? ")"                                          {{ fun (s1,s2) -> dest_Some (concatenate_two s1 s2) }}
  | ?notbracket? BAL KET                                         {{ fun (s1,(s2,s3)) -> dest_Some (concatenate_list [s1;s2;s3]) }}
*)

EXPLIST ->
  ""                                                             {{ fun _ -> [] }}
  | EXP                                                          {{ fun e -> [e] }}
  | EXP ?w? ";" ?w? EXPLIST                                              {{ fun (e,(w1,(_,(w2,es)))) -> (e::(c w1)::";"::(c w2)::es) }} (* FIXME ws *)

INFIX -> "="                                                     {{ fun s -> content s }}
  | "++"                                                         {{ fun s -> content s }} (* NB expressions like x++y++z are ambiguous *)
  | "::"                                                         {{ fun s -> content s (* FIXME not an infix ?*) }} 
  | "/\"                                                         {{ fun _ -> "&&" }} 
  | "\/"                                                         {{ fun _ -> "||" }} 
  | "==>"                                                         {{ fun _ -> "-->" }} 
  | "+"                                                         {{ c }} 
  | "-"                                                         {{ c }} 
  | "/ " {{ c }}
  | "*" {{ c }}
  | "<<" {{ c }}
  | "<"                                                         {{ c }} 
  | ">"                                                         {{ c }} 
  | "<="                                                         {{ c }} 
  | ">="                                                         {{ c }} 
  | "<>" {{ fun _ -> "/=" }}
  | "|++" {{ c }}
  | "|+" {{ c }}
  | "andThen" {{ c }}
  | "onlywhen" {{ c }}
(* note that with this configuation, /\ gets parsed as / (div) followed by \ (fun) *)

FNARGS -> ATEXP ?epsws? ATEXP                                    {{ fun (e1,(w1,e2)) -> [e1;(c w1);e2] }}
  | ATEXP ?epsws? FNARGS                                         {{ fun (s,(w1,ss)) -> ( s)::(c w1)::ss }}

CASES -> CASE                                                    {{ fun s -> [s] }}
  | CASE ?ws? "||" ?ws? CASES                                    {{ fun (c1,(w1,(_,(w2,cs)))) -> match cs with cs1::cs2 -> (c1^(c w1))::((c w2)^cs1)::cs2 }} (* FIXME need to get ws right *)


CASE -> EXP ?ws? "->" ?ws? EXP                                   {{ fun (e1,(w1,(_,(w2,e2)))) -> ""^e1^(c w1)^"->"^(c w2)^e2^"" }}
  | "_" ?ws? "->" ?ws? EXP                                       {{ fun (_,(w1,(_,(w2,e)))) -> "_"^(c w1)^"->"^(c w2)^e^"" }}


RECORD ->
  RECORDA {{ fun x -> match x with `Record(s) -> "<|"^s^"|>" }}
  | ATEXP ?w? "with" ?w? RECORDA {{ fun (e1,(w1,(_,(w2,e2)))) -> match e2 with `Record(s) -> "<| "^e1^(c w1)^"with"^(c w2)^s^"|>" }}

RECORDA -> 
    "<|" ?epsws? FIELDS ?epsws? "|>"                             {{ fun (_,(w1,(fs,(w2,_)))) -> `Record((c w1)^(String.concat "" fs)^(c w2)) }}

FIELDS -> 
    ""                                                           {{ fun _ -> [] }}
  | FIELD                                                        {{ fun f -> [f] }}
  | FIELD ?epsws? ";" ?epsws? FIELDS                             {{ fun (f,(w1,(_,(w2,fs)))) -> f::(c w1)::";"::(c w2)::fs }} (* FIXME ws *)

FIELD -> 
    ?ident? ?epsws? ":=" ?epsws? EXP                             {{ fun (f,(w1,(_,(w2,e)))) -> ((content f)^(c w1)^" ="^(c w2)^e) }}
  | ?ident? ?epsws? "updated_by" ?epsws? EXP                     {{ fun (f,(w1,(_,(w2,e)))) -> ((c f)^(c w1)^"updated_by"^(c w2)^e) }}


(* 

  | "::"                                                         {{ fun s -> content s ( * not an infix * ) }}
*)
