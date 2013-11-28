{{ 

(* demonstration of actions with an ambiguous grammar *) 

}}

START -> E ?ws? ?EOF?
                 {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

E -> E "+" E     {{ fun (x,(_,y)) -> x+y }}
  | E "*" E      {{ fun (x,(_,y)) -> x*y }}
  | "1"          {{ fun _ -> 1 }}
  | "2"          {{ fun _ -> 2 }}
