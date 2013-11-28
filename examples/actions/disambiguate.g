{{ 

(* Experiment in disambiguation. *)

(* basic print function FIXME move to core *)
let rec string_of_pt pt = (match pt with
  | `LF(x,_) -> ("LF("^x^",_)")
  | `NODE(x,pts) -> (
      "NODE("^
       x^","^
       (String.concat "," (List.map string_of_pt pts))^
       ")"))


}}

START -> 
    E ?ws? ?EOF? {{ fun (i,_) -> 
  let _ = print_string "Parsed an expression: " in
  if i=None then ((print_string "None"); i) 
  else (print_string ((string_of_pt (dest_Some i))^"\n"); i) 
}}

E -> 
    ""          {{ fun x -> Some(`LF("EPS",x)) }}
  | "a"         {{ fun x -> Some(`LF("a",x)) }}
  | E E E       {{ fun (x,(y,z)) -> 
  match (x,y,z) with 
    | (Some(`LF("a",_)),Some(`LF("EPS",_)),Some(_)) -> Some(`NODE("E",[dest_Some x;dest_Some y;dest_Some z]))
    | _ -> None       
}}

(*

we want to accept only results that are right-nested

we get 2 results, one of which is a None, and one is the actual pt we want

*)
