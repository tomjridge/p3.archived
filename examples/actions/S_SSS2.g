{{

(* 

Gamma_2^*, from: GLL Parsing, Elizabeth Scott and Adrian Johnstone, 2010

*)

}}

START -> S ?ws? ?EOF? {{ fun (i,_) -> print_string ((string_of_int i)^"\n"); i }}

S -> S S A   {{ fun (x,(y,z)) -> x+y+z }}
  | "1"      {{ fun _ -> 1 }}

A -> S {{ fun x -> x }}
  | "" {{ fun _ -> 0 }}
