START -> E {{ fun (i) -> print_string ((string_of_int i)^"\n"); i }}

E -> E E E   {{ fun (x,(y,z)) -> x+y+z }}
  | "a"      {{ fun _ -> 0 }}
  | ""       {{ fun _ -> 1 }}

COMMENT -> "A very stupid way to count the length of the input."
