{{

}}

START -> E ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

E -> E E {{ fun (x,y) -> x+y }}
  | "(" E ")" {{ fun (_,(x,_)) -> 2+x }}
  | "" {{ fun _ -> 0 }}

(* Example: well-formed parentheses http://en.wikipedia.org/wiki/Context-free_grammar#Well-formed_parentheses *)


