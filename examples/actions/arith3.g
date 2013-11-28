{{ 

(* * binds tighter than + *) 

(* 

  x*y+z = (x*y)+z not x*(y+z) 

  x+y*z = x+(y*z) not (x+y)*z

  two more...

  FIXME change to left assoc
  x+y+z = x+(y+z) not (x+y)+z
  x*y*z = x*(y*z) not (x*y)*z

*)

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
  | Some i -> (
    let s1 = string_of_expr i in
    let s2 = string_of_int (eval i) in
    print_string (s1^" = "^s2^"\n"))
  | _ -> () )
}}

PM -> "+" {{ fun s -> content s }} | "-" {{ fun s -> content s }}
  
TD -> "*" {{ fun s -> content s }} | "/" {{ fun s -> content s }}

E -> E PM E     {{ fun (x,(pm,y)) -> (match x,y with 
  | (Some _,Some(PM _)) -> None (* not x+(y+z) ! *)
  | (Some x,Some y) -> Some(PM(pm,x,y)) 
  | _ -> None)
}}

  | E TD E      {{ fun (x,(td,y)) -> (match x,y with
  | (Some _,Some (TD _)) -> None (* not x*(y*z) ! *)
  | (Some (PM _),Some _) -> None (* not (x+y)*z ! *)
  | (Some x,Some(PM _)) -> None (* not x*(y+z) ! *)
  | (Some x,Some y) -> Some(TD(td,x,y)) 
  | _ -> None)
}}

  | "(" E ")"    {{ fun (x,(y,z)) -> option_lift (fun y -> BRACKET(y)) y }}

  | ?num?        {{ fun s -> Some(Num(int_of_string (content s))) }}
  
