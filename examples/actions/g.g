{{

(* parsing grammar files themselves; 0 or more actions *)

let id x = x

open RawParsers

let myparse_comm = fun i -> (((a "(*") **>@ until_a "*)" **>@ (a "*)")) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z]))) i

let rec myparse_wscomm = fun i -> ((parse_epsws)
   |||@ ((parse_epsws **>@ myparse_comm **>@ myparse_wscomm) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z] )))) i

let term_to_parser s = (match s with 
  | "?w?" -> myparse_wscomm
  | "?tmp?" -> (until_a ("}"^"}"))
  | _ -> (term_to_parser s))

(* FIXME in the following we took the code from ParseGrammar, and expanded the listofs, but listofs seem a bit buggy (they allow item sep to represent a singleton list, but probably this should not be allowed) *)

(* the grammar produces multiple parses if actions - why? does the usual code do this? the usual code, and this code, produces one output only, but the print action is triggered twice because there are two identical results resuts returned from the subparse on HG presumably; this is because the rules can parse to RULE ?w?; this is now fixed; we should probably fix this in listof *)


module A = struct 

  let string_of_symbol sym = (match sym with | NT nt -> nt | TM tm -> tm)

  let string_of_rule (nt,(syms,acts)) = (nt^" -> "^(String.concat " " (List.map string_of_symbol syms)))

  let string_of_rules rs = (String.concat "\n" (List.map string_of_rule rs))

  let string_of_grammar (h,rs) = (
    let rs = List.concat rs in
    "{{\n"^h^"\n}"^"}\n\n"
    ^(string_of_rules rs)
  )

end


}}

GRAMMARWITHACTIONS -> HG ?w? ?EOF? {{ 
  fun (hg,(_,_)) -> (
    let _ = print_endline "parsed a grammar file" in 
    let _ = print_endline (A.string_of_grammar hg) in
    hg) 
}}    

HG -> RULES                       {{ fun rs -> ("",rs) }}                                                          
  | HEADER ?w? RULES              {{ fun (h,(_,rs)) -> (h,rs) }}                                                   

HEADER -> CODE                    {{ id }}                                                                         
                                  
RULES -> ""                       {{ fun _ -> [] }}                                                                
  | RULESPLUS                     {{ id }}                                                                         

RULESPLUS -> RULE                 {{ fun x -> [x] }}                                                               
  | RULE ?w? RULESPLUS            {{ fun (r,(_,rs)) -> r::rs }}                                                    

RULE -> SYM ?w? "->" ?w? RHS      {{ (fun (NT nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (nt,syms)) symss)) }}
                                  
RHS -> ""                         {{ fun _ -> [] }}                                                                
  | RHSPLUS                       {{ id }}                                                                         

RHSPLUS -> SYMSACT                {{ fun x -> [x] (* FIXME THIS isn't right - RHS should be nonempty *) }}         
  | SYMSACT ?w? "|" ?w? RHSPLUS   {{ fun (s,(_,(_,(_,rhs)))) -> s::rhs }}                                          

SYMSACT -> SYMS ?w? ACTS           {{ (fun (syms,(_,act)) -> (syms,act)) }}                                         

SYMS -> SYM                       {{ fun x -> [x] }}                                                               
 | SYM ?w? SYMS                   {{ fun (x,(_,xs)) -> x::xs }}                                                    

SYM -> '"' ?notdquote? '"'        {{ (fun (_,(s,_)) -> tm_of_lit (content s)) }}                                   
  | "'" ?notsquote? "'"           {{ (fun (_,(s,_)) -> tm_of_lit (content s)) }}                                   
  | ?AZS?                         {{ (fun s -> NT (content s)) }}                                                  
  | "?" ?azAZs? "?"               {{ (fun (_,(s,_)) -> TM("?" ^ (content s) ^ "?")) }}                             
                                  
ACTS -> CODES                      {{ id }}                                                                         
                                  
CODES -> ""                       {{ fun _ -> [] (* FIXME THIS ISN:T A correct expansion of listof *) }}           
  | CODESPLUS                     {{ id }}                                                                         

CODESPLUS -> CODE                 {{ fun x -> [x] }}                                                               
  | CODE ?w? CODESPLUS            {{ fun (x,(_,xs)) -> x::xs }}                                                    

CODE -> "{" "{" ?tmp? "}" "}"     {{ (fun (_,(_,(act,_gt))) -> (content act)) }}                                     
