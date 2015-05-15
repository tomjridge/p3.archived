(* 

(* some tests for p3 *)
open P3_core
open P3_extra.P3_basic_parsers
open P3_extra.P3_memo

let counter=ref 0

let parse_1 = (a "1") >>>> (fun _ -> counter:=1+(!counter); 1)
let parse_1 = (fun x -> (match x with 
  | Inl _ -> print_endline "parse_1 inl"
  | Inm _ -> print_endline "parse_1 inm"
  | Inr _ -> print_endline "parse_1 inr")
; parse_1 x)
let parse_eps = (a "") >>>> (fun _ -> 0)


let tbl_E = Hashtbl.create 100 
let rec parse_E = (fun i -> memo_p3 tbl_E (mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps))
  i)

let p = parse_E
let txt = "111"

let (o,tmo) = oracle_of_parser p txt 3
let _ = tmo "a \"1\"" (0,0)
let _ = o (`NT "E",`NT "E") (0,3)

(* 

  [ ] calling oracle_of_parser doesn't seem to invoke parse_1 at all! 

  this is because... the function which actually operates on the input is extracted at grammar extraction time


*)


let s = setup_of_parser p txt 3
let r = Earley3.Earley_interface.earley_full s

let g = grammar_of_parser p
(* parse_1 called 6 times; !counter is 0 *)
let [3] = p3_run_parser_string p txt

(* 

input "111", call to grammar_of_parser:

parse_1 inl
parse_1 inl
parse_1 inl
parse_1 inl
parse_1 inl
parse_1 inm


call to p3_run_parser_string:

parse_1 inl|grammar extraction
parse_1 inl|
parse_1 inl|
parse_1 inl|
parse_1 inl|
parse_1 inm|grammar extraction 
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|
parse_1 inr|

!counter==3

Since !counter==3, the action was applied only 3 times, which is correct.

The issue is that parse_1 inr seems to be called 10 times, whereas we
expect only 4.

This must presumably be happening in the earley parser.

How to isolate changes in earley code? pull out earley_grammar_of_grammar and setup_of_parser



*)



(* 

for input length 4, counter is 4, but parse_1 is called 21 times; 6 for initial grammar extraction and a further 15 times; possibly calling it 5 times is OK; and 10 are unexpected 

for input length 3, counter is 3, parse_1 called 16 times ie 6 for initial grammar extraction and 10 elsewhere, of which 4 are expectecd and 6 are unexpected

the pattern seems to be that we get 2*n unexpected calls to the terminal parser 

--

for grammar extraction, perhaps this isn't so bad: we get one call at the toplevel, and then 3 more from the sub-Es?

actually, we should really track how many times each component is called

*)

*)
