{{

}}

START -> E ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

E -> E E E   {{ fun (x,(y,z)) -> x+y+z }}
  | "1"      {{ fun _ -> 1 }}
  | ""       {{ fun _ -> 0 }}

