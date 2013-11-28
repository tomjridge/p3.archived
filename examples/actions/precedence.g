{{ 

(* Example of parsing with priorities/ operator precedence parsing,
following dypgen (see dypgen doc, Sect. 3 "Resolving Ambiguities"

It is worth noting that our parsing machinery is sufficiently general
that it supports this form of disambiguation directly, with no
special-purpose ad-hocery, and no performance problems.

*) 

(* we work with priorities pi<pt<pp (following dypgen example) *)

let (pi,pt,pp) = (1,2,3)

type expr = Num of int 
  | PM of string * expr * expr (* PM stands for: plus minus *)
  | TD of string * expr * expr (* TD stands for: times divide *)
  | BRACKET of expr

let rec string_of_expr e = (match e with
  | Num i -> (string_of_int i)
  | PM(s,e1,e2) -> ("("^(string_of_expr e1)^s^(string_of_expr e2)^")")
  | TD(s,e1,e2) -> ("("^(string_of_expr e1)^s^(string_of_expr e2)^")")
  | BRACKET(e1) -> ("("^(string_of_expr e1)^")"))

let rec eval e = (match e with 
  | Num i -> i
  | PM (s,e1,e2) -> (if s="+" then (eval e1)+(eval e2) else (eval e1)-(eval e2))
  | TD (s,e1,e2) -> (if s="*" then (eval e1)*(eval e2) else (eval e1)/(eval e2))
  | BRACKET (e1) -> (eval e1))

(* the option types in the following are very ugly, of course, and could be much improved *)

let option_lift f x = (match x with None -> None | Some x -> Some (f x))

}}

START -> E ?ws? ?EOF? {{ fun (i,_) -> (match i with 
  | Some (p,i) -> (
    let s1 = string_of_expr i in
    let s2 = string_of_int (eval i) in
    print_string (s1^" = "^s2^"\n"))
  | _ -> () )
}}

PM -> "+" {{ fun s -> content s }} | "-" {{ fun s -> content s }}
  
TD -> "*" {{ fun s -> content s }} | "/" {{ fun s -> content s }}

(* we return expressions with a priority! *)
E -> E PM E     {{ fun (x,(pm,y)) -> (match x,y with 
  | (Some(px,x),Some(py,y)) -> (if (px<=pp) && (py<pp) then Some(pp,PM(pm,x,y)) else None)
  | _ -> None)
}}

  | E TD E      {{ fun (x,(td,y)) -> (match x,y with
  | (Some(px,x),Some(py,y)) -> (if (px<=pt) && (py<pt) then Some(pt,TD(td,x,y)) else None)
  | _ -> None)
}}

  | "(" E ")"    {{ fun (x,(y,z)) -> option_lift (fun (_,y) -> (pi,BRACKET(y))) y }}

  | ?num?        {{ fun s -> Some(pi,Num(int_of_string (content s))) }}
  
