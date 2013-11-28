{{

(* FIXME map Inl to INL, Inr to INR; List.mem to MEM; max to MAX *)

(* FIXME trailing ; at end of fields isn't accepted *)

(* map List.fold_left to FOLDL *)

(* don't parse List.hd as a record; List.filter; List.map *)

(* FIXME allow datatype defns to start with |: type e = | ... *)

(* FIXME move towards ocaml_to_latex clauses - these match the ocaml manual; the below is rather ad-hoc *)

(* a simple grammar, for OCaml fragments *)

(* This is a naive grammar for ocaml (partly based on the official
   grammar from the ocaml manual). The actions currently translate the
   ocaml to hol, suitable for use in the hol4 theorem prover. *)

(* FIXME when printing to hol, remove any trailing ; in field list *)

(* FIXME need to be able to see the parse trees when we get ambiguous
   results; maybe adjust the other parsers to take a grammar with actions
   and ignore the actions to output a parse tree; need to incorporate the
   header! *)

(* FIXME one improvement would be to move closer to the ocaml grammar
   described at
   http://caml.inria.fr/pub/docs/manual-ocaml-312/expr.html *)

(* FIXME doesn't handle nested comments; also need to track whitespace
   in all parses eg for field *)

let id x = x
let c s = content s
let w s = c s

(* we need to define some raw parsers, but we want to use the existing
   Combinator and BasicParsers; Combinator is already opened by
   Everything *)

module Tmp = struct 

open P1_lib.P1_terminal_parsers.RawParsers

let myparse_comm = fun i -> (((a "(*") **>@ until_a "*)" **>@ (a "*)")) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z]))) i

let rec myparse_wscomm = fun i -> ((parse_epsws)
   |||@ ((parse_epsws **>@ myparse_comm **>@ myparse_wscomm) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z] )))) i

(* test

let _ = myparse_wscomm ((full ""));;

*)

let myparse_ident' = 
  let pred = fun c -> 
    ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
    || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    || (c = "_") || (c = "'")
  in
  (parse_az **>@ (parse_while pred)) >>@ (fun (s1,s2) -> dest_Some (concatenate_list [s1;s2]))

let myparse_ident = fun i -> 
  let rs = myparse_ident' i in
  List.filter (fun (x,s) -> not (mem (content x) ["let";"in";"if";"then";"else";"rec"])) rs  

let myparse_Ident = 
  let pred = fun c -> 
    ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
    || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    || (c = "_") || (c = "'")
  in
  (parse_AZ **>@ (parse_while pred)) >>@ (fun (s1,s2) -> dest_Some (concatenate_list [s1;s2]))

end

include Tmp

(* test

let _ = myparse_ident ( (full "abc"))
let _ = myparse_ident ( (full "let abc"))
let _ = myparse_ident ( (full "Abc"))
let _ = myparse_ident ( (full "letabc")) 

let _ = myparse_ident ( (full "todo6"))

let _ = myparse_Ident ( (full "Set_nt_item.empty"))
let _ = myparse_Ident ( (full "set_nt_item.empty"))

*)

(* here we alter RawParsers.term_to_parser to include our new terminal
   parsers; NB term_to_parser is already bound to
   RawParsers.term_to_parser at this point *)

let wrap = P3_lib.P3_basic_parsers.wrap

let term_to_parser s = (match s with 
  | "?wscomm?" -> (wrap  s myparse_wscomm)
  | "?w?" -> (wrap s myparse_wscomm)
  | "?ident?" -> (wrap s myparse_ident)
  | "?Ident?" -> (wrap s myparse_Ident)
  | _ -> (P3_lib.P3_basic_parsers.term_to_parser s))


(* following translates eg between List.map and MAP *)
let map_module_name s = (match s with
  | "List.map" -> "MAP"
  | "List.hd" -> "HD"
  | "List.tl" -> "TL"
  | "List.partition" -> "MYPARTITION"
  | "List.filter" -> "FILTER"
  | "List.concat" -> "FLAT"
  | "List.rev" -> "REVERSE"
  | "List.for_all" -> "EVERY"
  | "Set_nt_item.empty" -> "{}"
  | "Set_nt_item.mem" -> "MEM"
  | "Set_nt_item.is_empty" -> "(\\ s. s = {})"
  | "Set_nt_item.union" -> "(\\ x. \\ y. x UNION y)"
  | "Set_nt_item.diff" -> "(\\ x. \\ y. x DIFF y)"
  | "Set_nt_item.add" -> "(\\ x. \\ y. x INSERT y)"
  | "Set_nt_item.map" -> "IMAGE"
  | "Set_nt_item.split" -> "FIXME FIXME HOL specific"
  | "Set_nt_item.choose" -> "CHOICE"
  | "Set_nt_item.list_union" -> "LIST_UNION"
  | "Set_nt_item.elements" -> "SET_TO_LIST"
  | "Set_nt_item.from_list" -> "(\\ xs. set xs)"
  | "Set_nt_item.remove" -> "(\\ x s. s DIFF {x})"
  | "Set_nt_item_int.list_union" -> "LIST_UNION"
  | "Set_nt_item_int.empty" -> "{}"
  | "Map_earley_key.empty" -> "(\\ x. {})"
  | "Map_earley_key.find2" -> "FIND2"
  | "Map_earley_key.add" -> "(\\ x. \\ y. \\ m. (x =+ y) m)"
  | "Map_int.add" -> "(\\ x. \\ y. \\ m. (x =+ y) m)"
  | "Map_int.empty" -> "(\\ x. {})"
  | "Map_int.find2" -> "FIND2"
  | "Map_int_int.add" -> "(\\ x. \\ y. \\ m. (x =+ y) m)"
  | "Map_int_int.empty" -> "(\\ x. \\ y. {})"
  | "Map_int_int.find2" -> "FIND2"
  | "Map_syms_sym.add" -> "(\\ x. \\ y. \\ m. (x =+ y) m)"
  | "Map_syms_sym.find2" -> "FIND2"
  | "String.length" -> "STRLEN"
  | _ -> s)

let map_constr s = (match s with
  | "Some" -> "SOME"
  | "None" -> "NONE"
  | _ -> s)

let map_type s = (match s with
  | "int" -> "num"
  | "Set_nt_item.t" -> "(nt_item set)"
  | "Set_nt_item_int.t" -> "((nt_item#num) set)"
  | "Map_int.ty_map" -> "num -> (nt_item set)"
  | "Map_int_int.ty_map" -> "(num # num) -> (symbol list # symbol) -> (nt_item set)"
  | "Map_earley_key.ty_map" -> "(num # nonterm) -> (nt_item set)"
  | _ -> s)

let map_ident s = (match s with 
  | "true" -> "T"
  | "false" -> "F"
  | "snd" -> "SND"
  | "not" -> "~"
  | "union" -> "(\\ x. \\ y. x++y)"
  | "subtract" -> "MYLISTDIFF"
  | "itlist" -> "ITLIST"
  | _ -> s)

(* let debug_endline s = () *)

(* first string is the name of the type *)
type tydef = Tyabbrev of string * string | Tydatatype of string * string

let string_of_tydef d = (match d with
  | Tyabbrev (s1,s2) -> ("val "^s1^"_def = type_abbrev(\""^s1^"\",``:"^s2^"``);") (* FIXME map types? *)
  | Tydatatype (s1,s2) -> ("val "^s1^"_def = Hol_datatype `\n"^s2^"\n`;") (* FIXME map types? *)
)

type defn = Letrec of string * string | Let of string * string

let string_of_defn d = (match d with
  | Letrec (s1,s2) -> ("val "^s1^"_def = Define `\n"^s2^"\n`;") 
  | Let (s1,s2) -> ("val "^s1^"_def = Define `\n"^s2^"\n`;") )

}}

                                                               
START -> ?w? DEFN ?w? ?EOF?                                    {{ fun (_,(i,(_,_))) -> let _ = debug_endline ("Parsed an exp\n") in let _ = print_endline (string_of_defn i) in () }}                                                
  | ?w? TYPEDEFINITION ?w? ?EOF?                               {{ fun (_,(i,(_,_))) -> let _ = debug_endline ("Parsed a type definition\n") in let _ = print_endline i in () }}                                   
                                                               
DEFN -> "let rec " ?w? FNARGS ?w? "=" ?w? EXP                   {{ fun (_,(_,(fnargs,(_,(_,(_,e)))))) -> Letrec(List.hd fnargs,((*"let rec "^*)(String.concat "" fnargs)^" = "^e)) }}                                      
| "let" ?w? FNARGS ?w? "=" ?w? EXP                             {{ fun (_,(_,(fnargs,(_,(_,(_,e)))))) -> Let(List.hd fnargs,((*"let "^*)(String.concat "" fnargs)^" = "^e)) }}                                          
| "let" ?w? ?ident? ?w? "=" ?w? EXP                             {{ fun (_,(_,(i,(_,(_,(_,e)))))) -> Let(c i,(c i)^" = "^e) }}                                          

                                                               
EXP ->                                                         
    ATEXP                                                      {{ fun i -> i }}                                                                                                                 
  | CONSTANT                                                   {{ fun x -> x }}                                                                                                                 
  | CONSTR ?w? EXP                                             {{ fun (s1,(w1,s2)) -> s1^(c w1)^s2 }}                                                                                           
  | EXP ?w? INFIX ?w? EXP                                      {{ fun (e1,(w1,(i,(w2,e2)))) -> e1^(c w1)^i^(c w2)^e2 }}                                                                         
  | SFNARGS                                                     {{ fun ss -> ss  }}                                                                                           
  | "match" ?w? EXP ?w? "with" ?w? CASES                       {{ fun (_,(w1,(e,(w2,(_,(w3,cs)))))) -> ("case"^(c w1)^e^(c w2)^"of"^(c w3)^((*String.concat "||"*) cs)) }}                      
  | "if" ?w? EXP ?w? "then" ?w? EXP ?w? "else" ?w? EXP         {{ fun (_,(w1,(e1,(w2,(_,(w3,(e2,(w4,(_,(w5,(e3))))))))))) -> ("if"^(c w1)^e1^(c w2)^"then"^(c w3)^e2^(c w4)^"else"^(c w5)^e3) }}
  | "let" ?w? EXP ?w? "in" ?w? EXP                             {{ fun (_,(w1,(e1,(w2,(_,(w3,e2)))))) -> "let"^(c w1)^e1^(c w2)^"in"^(c w3)^e2 }}                                                
  | "fun" ?w? EXP ?w? "->" ?w? EXP                             {{ fun (_,(_,(e1,(_,(_,(_,e2)))))) -> "\\ "^e1^" . "^e2 }}                                                                       
  | EXP ":" TYPEXPR                                            {{ fun (e,(_,t)) -> e^":"^t }}                                                       
     

SFNARGS -> FNARGS {{ fun ss -> String.concat "" ss }}

CONSTANT -> CONSTR                                             {{ fun x -> x }}                                                                                                                 
                                                               
                                                                                                                             
ATEXP ->                                                       
    CONSTANT                                                   {{ id }}   
  | BRACKET                                                    {{ fun s -> s }}                                                                                                                 
  | ?ident?                                                    {{ fun s -> map_ident (content s) }}                               
  | ?num? {{ c }}                                                                          
  | "-" ?num? {{ fun (s1,s2) -> (c s1)^(c s2) }}                                                                          
                                                               
  | "_"                                                        {{ fun s -> content s (* for patterns *) }}                                                                                      
                                                               
  | '"' ?notdquote? '"'                                        {{ fun (_,(s,_)) -> ("\"" ^ (content s) ^ "\"") }}                                                                               
  | "[" ?w? EXPLIST ?w? "]"                                    {{ fun (_,(_,(ss,(_,_)))) -> "[" ^ (String.concat ";" ss) ^ "]" }}                                                               
  | '"' "[]" '"'                                               {{ fun _ -> "\"[]\"" }}                                                                                                          
  | '"' "::" '"'                                               {{ fun _ -> "\"::\"" (* not really an infix *) }}                                                                                
  | RECORD                                                     {{ fun i -> i }}                                                                                                                 
  | ATEXP "." FIELDNAME                                        {{ fun (s1,(_,s2)) -> s1^"."^s2 (* FIXME rec select *)  }}                                                                       
                                                               
  | MODULEPATH "." VALUENAME                                   {{ fun (s1,(_,s2)) -> map_module_name (s1^"."^s2) (* FIXME struct select *)  }}                                                                    

ATEXPR -> ATEXP {{ id }} (* ocaml_to_latex uses ATEXPR *)
                                                               
                                                               
CONSTR ->                                                      
    ?Ident?                                                    {{ fun s -> map_constr (c s) }}                                                                                                                          
                                                               
MODULEPATH -> ?Ident?                                          {{ c }}                                                                                                                          
  | MODULEPATH "." ?Ident?                                     {{ fun (s1,(_,s2)) -> (s1^(c s2)) }}                                                                                               
                                                               
                                                               
FIELDNAME -> ?ident?                                           {{ fun s -> c s }}                                                                                                               
                                                               
VALUENAME -> ?ident?                                           {{ fun s -> c s }}                                                                                                               
                                                               
IDENT ->                                                       
    ?ident?                                                    {{ fun s -> content s }}                                                                                                         
  | ?Ident?                                                    {{ fun s -> content s }}                                                                                                         
                                                               
BRACKET ->                                                     
    "(" ")"                                                    {{ fun _ -> "()" }}                                                                                                              
  | "(" ?w? EXPCOMMALIST ?w? ")"                               {{ fun (_,(w1,(e,(w2,_)))) -> "("^(c w1)^e^(c w2)^")" }}                                                                         

(*
  | "(" ?w? EXP ?w? "," ?w? EXP ?w? ")"                        {{ fun (_,(w1,(e1,(w2,(_,(w3,(e2,(w4,_)))))))) -> ("("^(c w1)^e1^(c w2)^","^(c w3)^e2^(c w4)^")") }}                                                                         
  | "(" EXP "," ?w? EXP "," ?w? EXP ")"                        {{ fun (_,(e1,(_,(_,(e2,(_,(_,(e3,_)))))))) -> ("("^e1^","^e2^","^e3^")") }}                                                     
*)

EXPCOMMALIST -> 
    EXP                                                        {{ fun e -> e }}                                                                                                               
  | EXP ?w? "," ?w? EXPCOMMALIST                               {{ fun (e,(w1,(_,(w2,es)))) -> e^(c w1)^","^(c w2)^es }}                                                                                                    
                                                               
(*                                                             
BAL -> "(" KET                                                 {{ fun (s1,s2) -> dest_Some (concatenate_two s1 s2) }}                                                                           
KET -> ?notbracket? ")"                                        {{ fun (s1,s2) -> dest_Some (concatenate_two s1 s2) }}                                                                           
  | ?notbracket? BAL KET                                       {{ fun (s1,(s2,s3)) -> dest_Some (concatenate_list [s1;s2;s3]) }}                                                                
*)                                                             
                                                               
EXPLIST ->                                                     
  ""                                                           {{ fun _ -> [] }}                                                                                                                
  | EXP                                                        {{ fun e -> [e] }}                                                                                                               
  | EXP ";" EXPLIST                                            {{ fun (e,(_,es)) -> e::es }}                                                                                                    
                                                               
INFIX -> "="                                                   {{ fun s -> (content s) }}                                                                                                       
  | "++"                                                       {{ fun s -> content s }}                                                                                                         
  | "::"                                                       {{ fun s -> content s (* FIXME not an infix ?*) }}     

  | "+"                                                        {{ fun s -> content s (* FIXME not an infix ?*) }}     
  | "@"                                                        {{ fun s -> "++" (* FIXME not an infix ?*) }}          
  | "&&"                                                        {{ fun s -> "/\\" }}                                     | "||"                                                        {{ fun s -> "\\/" }}                                                                               
  | ">>=" {{ c }}     
  | "<>" {{ c }}
  | "^" {{ fun _ -> "++" }}
  | "<=" {{ c }}
  | "<" {{ c }}
  | "-" {{ c }}

 
FNARGS -> ATEXPR ?w? ATEXPR                      {{ fun (e1,(w1,e2)) -> [e1;(w w1);e2] }}                                                                                         
  | ATEXPR ?w? FNARGS                            {{ fun (e1,(w1,e2)) -> [e1;(w w1)]@e2 }}                                                                                         
                                                               
(* FIXME following needs optional bar at front *)              
                                                               
CASES -> CASESB                                                {{ fun s -> s}}                                                                                                                  
  | "|" ?w? CASESB                                             {{ fun (_,(w1,x)) -> " "^(c w1)^x (* initial | *) }}                                                                             
                                                               
CASESB ->                                                      
  CASE                                                         {{ fun s -> s }}                                                                                                                 
  | CASE ?w? "|" ?w? CASES                                     {{ fun (c1,(w1,(_,(w2,cs)))) -> c1^(c w1)^"||"^(c w2)^cs }}                                                                      
(* above clause erroneously allows cases to start with a bar *)
                                                               
CASE -> EXP ?w? "->" ?w? EXP                                   {{ fun (e1,(w1,(_,(w2,e2)))) -> ""^e1^(c w1)^"->"^(c w2)^e2^"" }}                                                                
(* already treating _ as an exp  | "_" ?w? "->" ?w? EXP                                       {{ fun (_,(_,(_,(_,e)))) -> "_->"^e^"" }} *)                                                                                        
                                                               
                                                               
RECORD ->                                                      
    "{" ?w? FIELDS ?w? "}"                                     {{ fun (_,(w1,(fs,(w2,_)))) -> "<|"^(c w1)^(fs)^(c w2)^"|>" }}                                                              
  | "{" ?w? ATEXP ?w? "with" ?w? FIELDS ?w? "}"                {{ fun (_,(w1,(i,(w2,(_,(w3,(fs,(w4,_)))))))) -> "("^i^(c w2)^"with"^(c w3)^"<|"^(c w1)^fs^(c w4)^"|>)" }}                             
                                                               
FIELDS ->                                                      
    ""                                                         {{ fun _ -> "" }}                                                                                                                
  | FIELD                                                      {{ fun f -> f }}                                                                                                               
  | FIELD ?w? ";" ?w? FIELDS                                   {{ fun (f,(w1,(_,(w2,fs)))) -> f^(c w1)^";"^(c w2)^fs }}                                                                                            
                                                               
FIELD ->                                                       
    ?ident? ?w? "=" ?w? EXP                                    {{ fun (f,(_,(_,(_,e)))) -> ((content f)^" := "^e) }}                                                                            

(* following for type defns FIXME needs tidying up  *)


TYPEXPR -> "'" ?ident?                                         {{ fun (_,s2) -> "'" ^ (content s2) }}                                                                                           
  | "(" ?w? TYPEXPR ?w? ")"                                    {{ fun (_,(w1,(t,(w2,_)))) -> "("^(c w1)^t^(c w2)^")" }}
  | TYPEXPR ?w? "->" ?w? TYPEXPR                               {{ fun (t1,(w1,(_,(w2,t2)))) -> t1^(c w1)^"->"^(c w2)^t2 }}                                                                                              
  | TYPEXPR ?w? "*" ?w? TYPEXPR                                {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(c w1)^"#"^(c w2)^s2 }} (* FIXME ambiguity *)
  | TYPECONSTR                                                 {{ id }}
  | TYPEXPR ?w? TYPECONSTR                                     {{ fun (s1,(w1,s2)) -> map_type (s1^(c w1)^s2) (* FIXME try and map types at a higher level as well *)  }}
  | "(" ?w? TYPEXPRA ?w? ")" ?w? TYPECONSTR {{ fun (_,(w1,(s1,(w2,(_,(w3,s2)))))) -> "("^(w w1)^s1^(w w2)^")"^(w w3)^s2 }}

TYPEXPRA -> TYPEXPR {{ id }}
  | TYPEXPR ?w? "," ?w? TYPEXPRA {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^","^(w w2)^s2 }}
                                  


(*
  | "(" ?w? TYPE ?w? ")"                                       {{ fun (_,(w1,(s1,(w2,_)))) -> "("^(c w1)^s1^(c w2)^")" }}
  | TYPE ?w? ?ident?                                           {{ fun (t,(_,i)) -> t^" "^(map_type (content i)) }}                                                                                         
  | MODULEPATH "." TYPEXPR                                        {{ fun (s1,(_,s2)) -> map_type (s1^"."^s2) }}
  | TYPEXPR ?w? TYPECONSTR                                     {{ fun (s1,(w1,s2)) -> s1^(c w1)^s2 }}
*)

TYPECONSTR -> TYPECONSTRNAME {{ id  }} 
  | MODULEPATH "." TYPECONSTRNAME {{ fun (s1,(_,s2)) -> map_type(s1^"."^s2) }}


TYPEDEFINITION -> "type" ?w? TYPEDEF      {{ fun (s1,(w1,s2)) -> string_of_tydef s2 (* "(*type"^(c w1)^"*)"^s2 *) }}                      

TYPEDEF -> TYPECONSTRNAME ?w? TYPEINFORMATION 
      {{ fun (s1,(w1,s2)) -> (match s2 with | Tyabbrev (_,s2) -> Tyabbrev(s1,s2) | Tydatatype (_,s2) -> Tydatatype (s1,s1^" = "^s2)) (*s1^(c w1)^s2*) }}                                  
  | TYPEPARAMS ?w? TYPECONSTRNAME ?w? TYPEINFORMATION 
      {{ fun (s0,(w0,(s1,(w1,s2)))) -> (match s2 with | Tyabbrev (_,s2) -> Tyabbrev(s1,s2) | Tydatatype (_,s2) -> Tydatatype (s1,s1^" = "^s2)) (*s1^(c w1)^s2*) }} 
      (* FIXME what about params? may cause problems because hol lists params in order they occur in defn :( *)                                 
   
TYPEPARAMS -> TYPEPARAM {{ id }}
  | "(" ?w? TYPEPARAMSA ?w? ")"           {{ fun (_,(s2,(s3,(s4,_)))) -> "("^(c s2)^s3^(c s4)^")" }}

TYPEPARAMSA -> TYPEPARAM                  {{ id }}
  | TYPEPARAM ?w? "," ?w? TYPEPARAMSA     {{ fun (s1,(w2,(_,(w4,s5)))) -> s1^(w w2)^","^(w w4)^s5 }}

TYPEPARAM -> "'" ?ident? {{ fun (s1,s2) -> (c s1)^(c s2) }}
                                       
TYPECONSTRNAME -> ?ident?                 {{ fun s -> map_type(c s) }}                                          
                                          
TYPEINFORMATION -> TYPEEQUATION           {{ fun s -> Tyabbrev("",s) }}                                                    
  | TYPEREPRESENTATION                    {{ fun s -> Tydatatype("",s) }}                                                    

TYPEEQUATION -> "=" ?w? TYPEXPR           {{ fun (_,(w1,s1)) -> (*"="^(c w1)^*)s1 }}

TYPEREPRESENTATION -> "=" ?w? CONSTRDECLS {{ fun (_,(w1,s1)) -> (*"="^(c w1)^*)s1 }}                                                    
  | "=" ?w? "{" ?w? FIELDDECLS ?w? "}"    {{ fun (_,(w3,(_,(w1,(s2,(w2,_)))))) -> (*"="^(c w3)^*)"<|"^(c w1)^s2^(c w2)^"|>" }}
                                          
CONSTRDECLS -> CONSTRDECL                 {{ id }}                                                    
  | CONSTRDECL ?w? "|" ?w? CONSTRDECLS    {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(c w1)^"|"^(c w2)^s2 }}                      
                                          
CONSTRDECL -> CONSTRNAME                  {{ id }}                                                    
  | CONSTRNAME ?w? "of" ?w? TYPEXPR       {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(c w1)^"of"^(c w2)^s2 }} 
                                          
CONSTRNAME -> ?Ident?                     {{ c }}                                                     

(* following can end in optional ; *)
FIELDDECLS -> FIELDDECLSA                 {{ id }}
  | FIELDDECLSA ?w? ";"                   {{ fun (s1,(w1,s2)) -> (s1^(c w1)^" "(*^(c s2)*)) }}

FIELDDECLSA -> FIELDDECL                   {{ id }}
  | FIELDDECL ?w? ";" ?w? FIELDDECLSA      {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(c w1)^";"^(c w2)^s2 }} 


FIELDDECL -> FIELDNAME ?w? ":" ?w? POLYTYPEXPR {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(c w1)^":"^(c w2)^s2 }}

POLYTYPEXPR -> TYPEXPR                    {{ (* FIXME *) id }}
