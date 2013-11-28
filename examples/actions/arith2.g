{{ 

(* * binds tighter than + *) 

(* 

  x*y+z = (x*y)+z not x*(y+z) 

  x+y*z = x+(y*z) not (x+y)*z

  two more...

  x+y+z = x+(y+z) not (x+y)+z
  x*y*z = x*(y*z) not (x*y)*z

*)

type expr = Num of int | Plus of expr * expr | Times of expr * expr

let rec string_of_expr e = (match e with
  | Num i -> (string_of_int i)
  | Plus(e1,e2) -> ("("^(string_of_expr e1)^"+"^(string_of_expr e2)^")")
  | Times(e1,e2) -> ("("^(string_of_expr e1)^"*"^(string_of_expr e2)^")"))

let rec eval e = (match e with 
  | Num i -> i
  | Plus (e1,e2) -> (eval e1)+(eval e2)
  | Times (e1,e2) -> (eval e1)*(eval e2))

(* the option types in the following are very ugly, of course, and could be much improved *)

}}

START -> E ?ws? ?EOF? {{ fun (i,_) -> (match i with 
  | Some i -> (
    let s1 = string_of_expr i in
    let s2 = string_of_int (eval i) in
    print_string (s1^" = "^s2^"\n"))
  | _ -> () )
}}

E -> E "+" E     {{ fun (x,(_,y)) -> (match x,y with 
  | (Some(Plus _),Some _) -> None (* not (x+y)+z ! *)
  | (Some x,Some y) -> Some(Plus(x,y)) 
  | _ -> None)
}}

  | E "*" E      {{ fun (x,(_,y)) -> (match x,y with
  | (Some (Times _),Some _) -> None (* not (x*y)*z ! *)
  | (Some (Plus _),Some _) -> None (* not (x+y)*z ! *)
  | (Some x,Some(Plus _)) -> None (* not x*(y+z) ! *)
  | (Some x,Some y) -> Some(Times(x,y)) 
  | _ -> None)

}}
  | ?num?        {{ fun s -> Some(Num(int_of_string (content s))) }}
  
