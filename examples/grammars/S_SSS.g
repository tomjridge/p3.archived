START -> S ?ws? ?EOF?

S -> S S S
  | S S   
  | "1"   

(* grammar used to test gll, described here: https://github.com/djspiewak/gll-combinators 

also in: GLL Parsing, Elizabeth Scott and Adrian Johnstone, 2010

*)
