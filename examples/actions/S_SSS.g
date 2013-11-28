{{

(* grammar used to test gll, described here: https://github.com/djspiewak/gll-combinators 

also in: GLL Parsing, Elizabeth Scott and Adrian Johnstone, 2010

*)

}}

START -> S ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

S -> S S S   {{ fun (x,(y,z)) -> x+y+z }}
  | S S      {{ fun (x,y) -> x+y }}
  | "1"      {{ fun _ -> 1 }}

