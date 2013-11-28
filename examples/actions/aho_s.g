{{

}}

START -> S ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

S -> "x" S S   {{ fun (x,(y,z)) -> 1+y+z }}
  | ""       {{ fun _ -> 0 }}
