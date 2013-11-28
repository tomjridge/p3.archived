{{

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

(* FIXME might be much easier to work with strings in actions rather than substrings; but this doesn't mesh with our terminal parsers *)

(* FIXME would like to have a generated type of parse trees; where the leaves are the contents of the terminal parsers, and the nodes are the nts *)

(* FIXME Inr { .. } is interpreted as a constr (good) and as a fnargs; can OCaml constructors take more than one arg? these both seem reasonable interpretations; fortunately both will typeset the same because both ultimately resolve to a constr; we might want to approach disambiguation as in examples/actions *)

(* let _ = Prelude.debugging := true *)

let id x = x
let c s = content s
let rec to_whitespace s = (
 if s = "" then "" else
 let c = s.[0] in
 if c=' ' then "\\ "^(to_whitespace (String.sub s 1 (String.length s - 1))) else
 "\\\\\n"^(to_whitespace (String.sub s 1 (String.length s - 1))))
                                   
let w s = to_whitespace(content s)
let cat xs = dest_Some (concatenate_list xs)
let ccat xs = c(cat xs)


let c5 s = fun (e1,(w1,(_,(w2,e2)))) -> e1^(w w1)^s^(w w2)^e2

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
  List.filter (fun (x,s) -> not (mem (content x) ["let";"in";"if";"then";"else";"rec";"match";"with"])) rs  

let myparse_Ident = 
  let pred = fun c -> 
    ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
    || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    || (c = "_") || (c = "'")
  in
  (parse_AZ **>@ (parse_while pred)) >>@ (fun (s1,s2) -> dest_Some (concatenate_list [s1;s2]))

let myparse_constr = fun i -> 
  let rs = myparse_Ident i in
  List.filter (fun (x,s) -> not (mem (content x) ["List";"String";"Hashtbl"])) rs

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
  | "?constr?" -> (wrap s myparse_constr)
  | _ -> (P3_lib.P3_basic_parsers.term_to_parser s))


(* following translates eg between List.map and MAP *)
let map_module_name s = "\\textsf{"^s^"}"

let map_module_path s = "\\textsf{"^s^"}"

let map_constr s = "\\textsf{"^s^"}"

let map_type s = s

let map_ident s = s

let debug_endline s = ()

(* first string is the name of the type *)
type tydef = Tyabbrev of string * string | Tydatatype of string * string

let string_of_tydef d = (match d with
  | Tyabbrev (s1,s2) -> ("type "^s1^" = "^s2) 
  | Tydatatype (s1,s2) -> ("type "^s1^" = "^s2) 
)

type defn = Letrec of string * string | Let of string * string

let string_of_defn d = (match d with
  | Letrec (s1,s2) -> ("val "^s1^"_def = Define `\n"^s2^"\n`;") 
  | Let (s1,s2) -> ("val "^s1^"_def = Define `\n"^s2^"\n`;") )

let map_tyvar s = 
  if s = "'a" then "\\alpha" else 
  if s = "'b" then "\\beta" else 
  if s = "'c" then "\\gamma" else 
  if s = "'d" then "\\delta" else 
  if s = "'e" then "\\epsilon" else 
  if s = "'f" then "\\zeta" else 
  s

let rec map_TYPECONSTRNAME s = (
  "\\textsf{"
    ^(Str.global_replace (Str.regexp "[_]") "\\_" s)
    ^"}")

let map_valuename s = 
  "\\textit{"
  ^(Str.global_replace (Str.regexp "[_]") "\\_" s)
  ^"}"

type substring = string P3_lib.P3_core.ty_substring

type whitespace = substring

(* by separating out the actions, we can more easily substitute alternative actions eg to output to html rather than latex; to make this feasible, you need to have the relevant rule, and the action, visible on screen at the same time, ie you need to split the emacs window *)

type ('DEFN,'TYPEDEFINITIONS,'TYPEDEFINITION,'VALUENAME,'INFIXOP,'FIELDNAME,'VALUEPATH,'MODULEPATH,'CONSTR) ops = { 
  _DEFN_1:substring*(whitespace*string)->'DEFN; 
  _DEFN_2:substring*(whitespace*(substring*(whitespace*string)))->'DEFN; 
  _TYPEDEFINITIONS_1:'TYPEDEFINITION->'TYPEDEFINITIONS;
  _TYPEDEFINITIONS_2:'TYPEDEFINITION*(whitespace*'TYPEDEFINITIONS) -> 'TYPEDEFINITIONS;
  _VALUENAME_1:substring->'VALUENAME;
  _INFIXOP_01:substring->'INFIXOP;
  _INFIXOP_02:substring->'INFIXOP;
  _INFIXOP_03:substring->'INFIXOP;
  _INFIXOP_04:substring->'INFIXOP;
  _INFIXOP_05:substring->'INFIXOP;
  _INFIXOP_06:substring->'INFIXOP;
  _INFIXOP_07:substring->'INFIXOP;
  _INFIXOP_08:substring->'INFIXOP;
  _INFIXOP_09:substring->'INFIXOP;
  _INFIXOP_10:substring->'INFIXOP;
  _INFIXOP_11:substring->'INFIXOP;
  _FIELDNAME_1:substring->'FIELDNAME;
  _VALUEPATH_1:'VALUENAME->'VALUEPATH;
  _VALUEPATH_2:'MODULEPATH*(substring*'VALUENAME)->'VALUEPATH;
  _CONSTR_1:substring->'CONSTR;
  _MODULEPATH_1:substring->'MODULEPATH;
  _MODULEPATH_2:'MODULEPATH*(substring*substring)->'MODULEPATH;
}

let latex_ops = {
  _DEFN_1=(fun (_,(w1,e1)) -> "\\textsf{let}"^(w w1)^e1);
  _DEFN_2=(fun (_,(w0,(_,(w1,e1)))) -> "\\textsf{let}"^(w w0)^"\\textsf{rec}"^(w w1)^e1);

  _TYPEDEFINITIONS_1=id;
  _TYPEDEFINITIONS_2=(fun (s1,(w1,s2)) -> s1^(w w1)^s2);






  _VALUENAME_1=(fun s -> map_valuename(c s));

  _INFIXOP_01=c;
  _INFIXOP_02=c;
  _INFIXOP_03=c; (* FIXME not an infix? *)
  _INFIXOP_04=c;
  _INFIXOP_05=(fun _ -> "{\\le}");
  _INFIXOP_06=c;
  _INFIXOP_07=(fun s -> "++" (* FIXME not an infix ?*));
  _INFIXOP_08=(fun s -> "{\\&\\&}" );
  _INFIXOP_09=(fun s -> "{\\otimes}");
  _INFIXOP_10=(fun s -> "{\\oplus}");
  _INFIXOP_11=(fun s -> "{\\gg}");

  _FIELDNAME_1=(fun s -> map_valuename(c s));


  _VALUEPATH_1=id;
  _VALUEPATH_2=(fun (s1,(_,s2)) -> s1^"."^s2);


  _CONSTR_1=(fun s -> map_constr (c s));


  _MODULEPATH_1=(fun s -> map_module_path (c s));
  _MODULEPATH_2=(fun (s1,(_,s2)) -> (s1^"."^(map_module_path (c s2))));
}

(* would be nice if we didn't have to give types for the record, and could just use ops_DEFN_1, but then we couldn't easily swap all the defns e.g. to output hol rather than latex; basically it seems we have to give record type upfront *)

(* separating off the ops, and parameterizing, means difficult to modify related clauses *)

let ops = latex_ops

}}

                                                               
START -> ?w? DEFN ?w? ?EOF?                                    {{ fun (_,(i,(_,_))) -> let _ = debug_endline ("Parsed an exp\n") in let _ = print_string (i) in () }}                                                
  | ?w? TYPEDEFINITIONS ?w? ?EOF?                               {{ fun (_,(i,(_,_))) -> let _ = debug_endline ("Parsed a type definition\n") in let _ = print_string i in () }}                                   
  | ?w? TYPEXPR ?w? ?EOF?                               {{ fun (_,(i,(_,_))) -> let _ = debug_endline ("Parsed a type expression\n") in let _ = print_string i in () }}                                   
  | ?w? "e:" EXPR ?w? ?EOF?                               {{ fun (_,(_,(i,(_,_)))) -> let _ = debug_endline ("Parsed an expression\n") in let _ = print_string i in () }}                                   

DEFN -> "let" ?w? LETBINDING               {{ ops._DEFN_1 }}                      
  | "let" ?w? "rec" ?w? LETBINDING     {{ ops._DEFN_2 }}
                                      
TYPEDEFINITIONS -> TYPEDEFINITION      {{ ops._TYPEDEFINITIONS_1 }}                                                                
  | TYPEDEFINITION ?w? TYPEDEFINITIONS {{ ops._TYPEDEFINITIONS_2 }}                                  




(* 6.3 Names *)                       

VALUENAME -> ?ident?                   {{ ops._VALUENAME_1 }}                                       
                                      
INFIXOP -> "="                         {{ ops._INFIXOP_01 }}
  | "++"                               {{ ops._INFIXOP_02 }}
  | "::"                               {{ ops._INFIXOP_03 }}
  | "<"                                {{ ops._INFIXOP_04 }}
  | "<="                               {{ ops._INFIXOP_05 }}
  | "+"                                {{ ops._INFIXOP_06 }}
  | "@"                                {{ ops._INFIXOP_07 }}
  | "&&"                               {{ ops._INFIXOP_08 }}
  | "***>"                             {{ ops._INFIXOP_09 }}  
  | "||||"                             {{ ops._INFIXOP_10 }}  
  | ">>>>"                             {{ ops._INFIXOP_11 }}
                                      
FIELDNAME -> ?ident?                   {{ ops._FIELDNAME_1 }}                                                      
                                     

VALUEPATH -> VALUENAME       {{ ops._VALUEPATH_1 }}                          
  | MODULEPATH "." VALUENAME {{ ops._VALUEPATH_2 }}

(* this was ?Ident? but we don't want List.map interpreted as CONSTR.FIELDNAME *)
CONSTR ->                 
    ?constr?               {{ ops._CONSTR_1 }}                           
                          
MODULEPATH -> ?Ident?      {{ ops._MODULEPATH_1 }}                      
  | MODULEPATH "." ?Ident? {{ ops._MODULEPATH_2 }}
                                                               
                                                               


(* 6.4 Type expressions; following for type defns FIXME needs tidying up  *)

TYPEXPR -> "'" ?ident?            {{ fun (s1,s2) -> map_tyvar (ccat [s1;s2]) }}                                                                    
  | "(" ?w? TYPEXPR ?w? ")"       {{ fun (_,(w1,(t,(w2,_)))) -> "("^(w w1)^t^(w w2)^")" }}                                                  
  | TYPEXPR ?w? "->" ?w? TYPEXPR  {{ fun (t1,(w1,(_,(w2,t2)))) -> t1^(w w1)^"{\\rightarrow}"^(w w2)^t2 }}                                               
  | TYPEXPR ?w? "*" ?w? TYPEXPR   {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^"{\\times}"^(w w2)^s2 }} (* FIXME ambiguity *)                          
  | TYPECONSTR                    {{ id }}                                                                                                  
  | TYPEXPR ?w? TYPECONSTR        {{ fun (s1,(w1,s2)) -> map_type (s1^(w w1)^s2) (* FIXME try and map types at a higher level as well *)  }}
  | "(" ?w? TYPEXPRA ?w? ")" ?w? TYPECONSTR {{ fun (_,(w1,(s1,(w2,(_,(w3,s2)))))) -> "("^(w w1)^s1^(w w2)^")"^(w w3)^s2 }}

TYPEXPRA -> TYPEXPR {{ id }}
  | TYPEXPR ?w? "," ?w? TYPEXPRA {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^","^(w w2)^s2 }}
                                  
TYPECONSTR -> TYPECONSTRNAME      {{ id  }}                                                                                                 
  | MODULEPATH "." TYPECONSTRNAME {{ fun (s1,(_,s2)) -> map_type(s1^"."^s2) }}                                                              
                                  
POLYTYPEXPR -> TYPEXPR            {{ (* FIXME *) id }}                                                                                      



(* 6.5 Constants *)

CONSTANT -> CONSTR      {{ id }}                                          
  | "[" ?w? "]"         {{ fun (_,(w1,_)) -> "["^(w w1)^"]" }}            
  | ?num?               {{ c }}                                           
  | "-" ?num?           {{ fun (s1,s2) -> (c s1)^(c s2) }}                
  | '"' ?notdquote? '"' {{ fun (_,(s,_)) -> ("{\\texttt{\"" ^ (content s) ^ "\"} }") }}
  | "'" ?notsquote? "'" {{ fun (_,(s,_)) -> ("'"^(c s)^"'") }}            
  | "()"                {{ c }}                                           

(* 6.6 Patterns *)

PATTERN -> EXPR {{ id (* FIXME *) }} 
  

(* 6.7 Expressions ; grammar is too ambiguous so we identify atomic expressions which can be arguments to functions *)
                                                       
EXPR -> ATEXPR                                            {{ id }}                                                                                                                         
  | EXPR ":" TYPEXPR                                      {{ fun (e,(_,t)) -> e^":"^t }}                                                                                                   
  | EXPR ?w? "," ?w? EXPRA                                {{ c5 "," }}                                                                                                                     
  | CONSTR ?w? EXPR                                       {{ fun (s1,(w1,s2)) -> s1^(w w1)^s2 }}                                                                                           
  | EXPR ?w? "::" ?w? EXPR                                {{ c5 "::" }}                                                                                                                    
  | FNARGS                                                {{ fun ss -> ss  }}                                                                                                              
  | EXPR ?w? INFIXOP ?w? EXPR                             {{ fun (e1,(w1,(i,(w2,e2)))) -> e1^(w w1)^i^(w w2)^e2 }}                                                                         
  | "if" ?w? EXPR ?w? "then" ?w? EXPR ?w? "else" ?w? EXPR {{ fun (_,(w1,(e1,(w2,(_,(w3,(e2,(w4,(_,(w5,(e3))))))))))) -> ("\\textsf{if}"^(w w1)^e1^(w w2)^"\\textsf{then}"^(w w3)^e2^(w w4)^"\\textsf{else}"^(w w5)^e3) }}
  | "match" ?w? EXPR ?w? "with" ?w? PATTERNMATCHING       {{ fun (_,(w1,(e,(w2,(_,(w3,cs)))))) -> ("\\textsf{match}"^(w w1)^e^(w w2)^"\\textsf{with}"^(w w3)^((*String.concat "||"*) cs)) }}            
  | "let" ?w? LETBINDING ?w? "in" ?w? EXPR                {{ fun (_,(w1,(e1,(w2,(_,(w3,e2)))))) -> "\\textsf{let}"^(w w1)^e1^(w w2)^"\\textsf{in}"^(w w3)^e2 }}                                                
  | "let" ?w? "rec" ?w? LETBINDING ?w? "in" ?w? EXPR      {{ fun (_,(w0,(_,(w1,(e1,(w2,(_,(w3,e2)))))))) -> "\\textsf{let}"^(w w0)^"rec"^(w w1)^e1^(w w2)^"\\textsf{in}"^(w w3)^e2 }}                                                
  | "fun" ?w? MULTIPLEMATCHING                            {{ fun (_,(w1,e1)) -> "\\textsf{fun}"^(w w1)^e1 }}                                                                                         
(* FIXME List.map parses as ATEXPR "." FIELDNAME, where ATEXPR is CONSTANT CONSTR *)

ATEXPR ->                    
    VALUEPATH                 {{ id }}                                                             
  | CONSTANT                  {{ id }}                                                             
  | "(" ?w? EXPR ?w? ")"      {{ fun (_,(w1,(e1,(w2,_)))) -> "("^(w w1)^e1^(w w2)^")" }}           
  | "[" ?w? EXPRLIST ?w? "]"  {{ fun (_,(w1,(ss,(w2,_)))) -> "["^(w w1)^ss^(w w2)^"]" }}           
  | RECORD                    {{ fun i -> i }}                                                     
  | ATEXPR "." FIELDNAME      {{ fun (s1,(_,s2)) -> s1^"."^s2 (* FIXME rec select *)  }}           
  | ATEXPR "." "[" EXPR "]"   {{ fun (s1,(_,(_,(s2,_)))) -> s1^".["^s2^"]"  }}                     
  | "_"                       {{ fun _ -> "\\_" }} (* FIXME this just because PATTERNs are EXPRs *)
  | "<fun>"                   {{ fun _ -> "<\\textit{fun}>" (* "{<{\\textit{fun}>}" *) }} (* for return values from ocaml toplevel *)
  | "(" ?w? INFIXOP ?w? ")"  {{ fun (_,(w1,(s1,(w2,_)))) -> "("^(w w1)^s1^(w w2)^")" }}
                             
EXPRA -> EXPR                 {{ id }}                                                             
  | EXPR ?w? "," ?w? EXPRA    {{ c5 "," }}                                                         

EXPRLIST ->                  
    EXPR                      {{ id }}                                                             
  | EXPR ?w? ";" ?w? EXPRLIST {{ c5 ";" }}                                                         
                             

PATTERNMATCHING -> CASESB     {{ fun s -> s}}                                                      
  | "|" ?w? CASESB            {{ fun (_,(w1,x)) -> "|"^(w w1)^x (* initial | *) }}                 
                             
CASESB ->                    
  CASE                        {{ fun s -> s }}                                                     
  | CASE ?w? "|" ?w? CASESB   {{ fun (c1,(w1,(_,(w2,cs)))) -> c1^(w w1)^"|"^(w w2)^cs }}           
(* above clause erroneously allows cases to start with a bar *)
                                                               
CASE -> PATTERN ?w? "->" ?w? EXPR                {{ fun (e1,(w1,(_,(w2,e2)))) -> ""^e1^(w w1)^"{\\rightarrow}"^(w w2)^e2^"" }}                                                  
                                                
MULTIPLEMATCHING -> PATTERNMATCHING              {{ id }} (* FIXME *)                                                                                                           

LETBINDING -> PATTERN ?w? "=" ?w? EXPR           {{ c5 "=" }}                                                                                                                   


                                                
FNARGS -> ATEXPR ?w? ATEXPR                      {{ fun (e1,(w1,e2)) -> e1^(w w1)^e2 }}                                                                                         
  | ATEXPR ?w? FNARGS                            {{ fun (e1,(w1,e2)) -> e1^(w w1)^e2 }}                                                                                         
                                                
RECORD ->                                       
    "{" ?w? FIELDS ?w? "}"                       {{ fun (_,(w1,(fs,(w2,_)))) -> "\\langle{}"^(w w1)^(fs)^(w w2)^"\\rangle{}" }}                                                 
  | "{" ?w? ATEXPR ?w? "with" ?w? FIELDS ?w? "}" {{ fun (_,(w1,(i,(w2,(_,(w3,(fs,(w4,_)))))))) -> "\\langle{}"^(w w1)^i^(w w2)^"\\textsf{with}"^(w w3)^fs^(w w4)^"\\rangle{}" }}
                                                
FIELDS ->                                       
    FIELD                                        {{ id }}                                                                                                                       
  | FIELD ?w? ";" ?w? FIELDS                     {{ c5 ";" }}                                                                                                                   
                                                
FIELD ->                                        
    ?ident? ?w? "=" ?w? EXPR                     {{ fun (f,(w1,(_,(w2,e)))) -> (map_valuename (c f))^(w w1)^"="^(w w2)^e }}                                                                     





(* 6.8 Type and exception definitions *)

TYPEDEFINITION -> "type" ?w? TYPEDEF      {{ fun (_,(w1,s2)) -> "\\textsf{type}"^(w w1)^s2 }}                      

TYPEDEF -> TYPECONSTRNAME ?w? TYPEINFORMATION 
      {{ fun (s1,(w1,s2)) -> s1^(w w1)^s2 }}                                  
  | TYPEPARAMS ?w? TYPECONSTRNAME ?w? TYPEINFORMATION 
      {{ fun (s0,(w0,(s1,(w1,s2)))) -> s0^(w w0)^s1^(w w1)^s2 }} 
      (* FIXME what about params? may cause problems because hol lists params in order they occur in defn :( *)                                 
   
TYPEPARAMS -> TYPEPARAM {{ id }}
  | "(" ?w? TYPEPARAMSA ?w? ")"           {{ fun (_,(s2,(s3,(s4,_)))) -> "("^(c s2)^s3^(c s4)^")" }}

TYPEPARAMSA -> TYPEPARAM                  {{ id }}
  | TYPEPARAM ?w? "," ?w? TYPEPARAMSA     {{ fun (s1,(w2,(_,(w4,s5)))) -> s1^(w w2)^","^(w w4)^s5 }}

TYPEPARAM -> "'" ?ident?                  {{ fun (s1,s2) -> 
  map_tyvar(ccat [s1;s2])
}}
                                       
TYPECONSTRNAME -> ?ident?                 {{ fun s -> map_TYPECONSTRNAME (c s) }}
                                          
TYPEINFORMATION -> TYPEEQUATION           {{ id }}                                                    
  | TYPEREPRESENTATION                    {{ id }}                                                    

TYPEEQUATION -> "=" ?w? TYPEXPR           {{ fun (_,(w1,s2)) -> "="^(w w1)^s2 }}

TYPEREPRESENTATION -> "=" ?w? CONSTRDECLS {{ fun (_,(w1,s2)) -> "="^(w w1)^s2 }}                                                    
  | "=" ?w? "{" ?w? FIELDDECLS ?w? "}"    {{ fun (_,(w3,(_,(w1,(s2,(w2,_)))))) -> "="^(w w3)^"\\langle{}"^(w w1)^s2^(w w2)^"\\rangle{}" }}
                                          
CONSTRDECLS -> CONSTRDECL                 {{ id }}                                                    
  | CONSTRDECL ?w? "|" ?w? CONSTRDECLS    {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^"|"^(w w2)^s2 }}                      
                                          
CONSTRDECL -> CONSTRNAME                  {{ id }}                                                    
  | CONSTRNAME ?w? "of" ?w? TYPEXPR       {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^"\\textsf{of}"^(w w2)^s2 }} 
                                          
CONSTRNAME -> ?Ident?                     {{ fun s -> "\\textsf{"^(c s)^"}" }}                                                     

(* following can end in optional ; *)
FIELDDECLS -> FIELDDECLSA                 {{ id }}
  | FIELDDECLSA ?w? ";"                   {{ fun (s1,(w1,s2)) -> (s1^(w w1)^" "^(c s2)) }}

FIELDDECLSA -> FIELDDECL                   {{ id }}
  | FIELDDECL ?w? ";" ?w? FIELDDECLSA      {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^";"^(w w2)^s2 }} 

(* FIXME the pattern is we map nonterms to strings; this causes lots of messing about with c and ^ *)

FIELDDECL -> FIELDNAME ?w? ":" ?w? POLYTYPEXPR {{ fun (s1,(w1,(_,(w2,s2)))) -> s1^(w w1)^":"^(w w2)^s2 }}

(*
debugging

let _ = MyHashtbl.global_reset ()
let _ = p3_run_parser parse_EXPR "let f x = y in z";;
let _ = p3_run_parser parse_PATTERN "f x";;
let _ = p3_run_parser parse_START "let f x = y in z";;

*)

