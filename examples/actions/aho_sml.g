{{

}}

START -> S ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

S -> S S "x" {{ fun (x,(y,z)) -> x+y+1 }}
  | ""       {{ fun _ -> 0 }}
