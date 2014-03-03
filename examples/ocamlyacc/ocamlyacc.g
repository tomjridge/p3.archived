{{

let c s = content s

let id = fun x -> x

let fIXME = fun _ -> "FIXME"

module Tmp = struct

open P1_lib.P1_terminal_parsers.RawParsers

let pretype = fun i -> (until_a ">") i

let header = fun i -> (until_a "%}") i

let untilrbrace = fun i -> (until_a "}") i
let untilrbracerbrace = fun i -> (until_a ("}"^"}")) i

let myparse_comm = fun i -> (((a "/*") **>@ until_a "*/" **>@ (a "*/")) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z]))) i

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

end

include Tmp

(* here we alter RawParsers.term_to_parser to include our new terminal
   parsers; NB term_to_parser is already bound to
   RawParsers.term_to_parser at this point *)

let wrap = P3_lib.P3_basic_parsers.wrap

let term_to_parser s = (match s with 
  | "?w?" -> (wrap s myparse_wscomm)
  | "?untilrbrace?" -> (wrap s untilrbrace)
  | "?untilrbracerbrace?" -> (wrap s untilrbracerbrace)
  | "?header?" -> (wrap s header)
  | "?ident?" -> (wrap s myparse_ident)
  | "?Ident?" -> (wrap s myparse_Ident)
  | "?pretype?" -> (wrap s pretype)
  | _ -> (P3_lib.P3_basic_parsers.term_to_parser s))

let string_of_sym s = (match s with
  | `NT x -> (String.uppercase x)
  | `TM x -> ("?"^x^"?"))

let string_of_syms xs = (match xs with
  | [] -> (failwith "string_of_syms: unexpected")
  | _ -> (String.concat " " (List.map string_of_sym xs)))

let string_of_ruleline rl = (match rl with
  | `RULELINE(x,y,z) -> (
    let syms = (match x with None -> "?eps?" | Some x -> string_of_syms x) in
    let prec = (match y with None -> "" | Some y -> y) in
    syms^"" (* FIXME prec *)^" {"^"{ fIXME }"^"}"(*z*)))

let string_of_rule r = (match r with
  | `RULE(x,rls) -> ((string_of_sym (`NT x))^" -> "^(String.concat "\n  | " (List.map string_of_ruleline rls))))

}}

START -> MLY {{ id }}

MLY -> ?w?
  "%{" HEADER "%}" ?w? 
  DECLARATIONS ?w? 
  "%%" ?w? RULES ?w? 
  "%%" ?all? 
  ?EOF?
  {{ fun (_,(_,(h,(_,(_,(d,(_,(_,(_,(rs,(_,(_,(t,_))))))))))))) ->
(*print_endline "Parsed";*)
print_endline (String.concat "\n\n" (List.map string_of_rule rs))

 }}

HEADER -> ?header?  {{ fun x -> `HEADER (c x) }}


DECLARATIONS -> DECLARATION     {{ fun x -> [x] }}
  | DECLARATION ?w? DECLARATIONS {{ fun (x,(_,y)) -> x::y }}

DECLARATION -> 
    DECTOKEN {{ id }}
  | DECPREC  {{ id }}
  | DECTYPE  {{ id }}
  | DECSTART {{ id }}
 
DECTOKEN -> "%token" ?w? NAMES       {{ fun (_,(_,ns)) -> `DECTOKEN(None,ns) }}
  | "%token" ?w? PRETYPE ?w? NAMES   {{ fun (_,(_,(pt,(_,ns)))) -> `DECTOKEN(Some(pt),ns) }}

NAMES -> NAME  {{ fun x -> [x] }}
  | NAME ?w? NAMES  {{ fun (x,(_,y)) -> x::y }}

NAME -> ?Ident?  {{ fun x -> `NAME(c x) }}

PRETYPE -> "<" ?pretype? ">"  {{ fun (_,(t,_)) -> `PRETYPE(t) }}

DECPREC -> "%left" ?w? SYMBOLS {{ fun (_,(_,syms)) -> `LEFT(syms) }}
  | "%right" ?w? SYMBOLS       {{ fun (_,(_,syms)) -> `RIGHT(syms) }}
  | "%nonassoc" ?w? SYMBOLS    {{ fun (_,(_,syms)) -> `NONASSOC(syms) }}

SYMBOLS -> SYMBOL {{ fun x -> [x] }}
  | SYMBOL ?w? SYMBOLS {{ fun (x,(_,y)) -> x::y }}

SYMBOL -> NT {{ id }} | TM {{ id }}

NT -> ?ident?  {{ fun x -> `NT (c x) }}

TM -> ?Ident?  {{ fun x -> `TM (c x) }}

DECTYPE -> "%type" ?w? PRETYPE ?w? NTS {{ fun (_,(_,(x,(_,nts)))) -> `DECTYPE(x,nts) }}

NTS -> NT      {{ fun x -> [x] }}
  | NT ?w? NTS {{ fun (x,(_,y)) -> x::y }}

DECSTART -> "%start" ?w? SYMBOLS {{ fun (_,(_,syms)) -> `DECSTART(syms) }}




RULES -> RULE       {{ fun x -> [x] }}
  | RULE ?w? RULES  {{ fun (x,(_,y)) -> x::y }}

RULE -> RESULT ":" ?w? RULELINES ?w? ";"  {{ fun (x,(_,(_,(rls,(_,_))))) -> `RULE(x,rls) }}

RESULT -> ?ident?  {{ c }}

RULELINES -> RULELINESA {{ id }}
  | "|" ?w? RULELINESA {{ fun (_,(_,x)) -> x }}

RULELINESA -> RULELINE               {{ fun x -> [x] }}
  | RULELINE ?w? "|" ?w? RULELINESA  {{ fun (x,(_,(_,(_,xs)))) -> x::xs }}


RULELINE -> 
    "{" ?untilrbrace? "}"              {{ fun (_,(x,_)) -> `RULELINE(None,None,c x) }} (* no symbols - epsilon production *)
  | SYMBOLS ?w? "{" ?untilrbrace? "}"  {{ fun (syms,(_,(_,(x,_)))) -> `RULELINE((Some syms), None, c x) }}
  | SYMBOLS ?w? "%prec" ?w? ?ident? ?w? "{" ?untilrbrace? "}"  {{ fun (syms,(_,(_,(_,(idt,(_,(_,(x,_)))))))) -> `RULELINE((Some syms),Some (c idt), c x) }} (* FIXME what is grammar of %prec rule recls? *)
  | SYMBOLS ?w? "{{" ?untilrbracerbrace? "}}"  {{ fun (syms,(_,(_,(x,_)))) -> `RULELINE((Some syms),None,c x) }} (* FIXME this to cope with rbrace in action *)


TRAILER -> "trailer"  {{ c }}


