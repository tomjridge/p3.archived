{{

}}

START -> S ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

S -> "1" S "1" {{ fun (_,(x,_)) -> 2+x }}
  | "1" {{ fun _ -> 1 }}
