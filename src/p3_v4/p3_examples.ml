(* 

    #directory "../_build";;
    #load "p3.cma";;    

*)

(**
{1 p3_examples.ml}
{2 P3 examples}
*)


open P3_lib

open P1_lib.P1_core.Prelude
open P1_lib.P1_core.Types
open P3_lib.P3_core
open P3_lib.P3_memo
open P3_lib.P3_basic_parsers

(* examples *)
let parse_1 = (a "1") >>>> (fun _ -> 1)
let parse_eps = (a "") >>>> (fun _ -> 0)


(* example with no memoization *)
(* right associative ***> *)
let rec parse_E = (fun i -> (mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps))
  i)

let p = parse_E
let _ = grammar_of_parser p
let txt = "111111"
let [6] = p3_run_parser_string p txt




(* example with explicit memoization *)
let tbl_E = MyHashtbl.create 100 
let rec parse_E = (fun i -> memo_p3 tbl_E (mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps))
  i)

let p = parse_E
(* if running in try.ocamlpro.com, this blows the stack :( 

   try.ocamlpro.com has 15538 stack size; "normal" ocaml is about 262067  

   http://stackoverflow.com/questions/7826992/browser-javascript-stack-size-limit

   http://rosettacode.org/wiki/Find_limit_of_recursion#OCaml
*)
let txt = "111111111111111111111111111111"
let [30] = p3_run_parser_string p txt


(* parse trees *)
let rec parse_E = (fun i -> mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> `Node(x,y,z)))
  |||| ((a "1") >>>> (fun _ -> `LF("1")))
  |||| ((a "") >>>> (fun _ -> `LF(""))))
  i)

let p = parse_E
let txt = "11"
let _ = p3_run_parser_string p txt



(* example with implicit memoization via memo_rec2 *)
let parse_E = memo_rec2 (fun self -> (mkntparser "E" (
  ((self ***> self ***> self) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps)))

let p = parse_E
let txt = "111111111111111111111111111111"
let [30] = p3_run_parser_string p txt






(* defining other combinators *)
(* no memo *)
let rec star p = (
  let f1 x = "STAR("^x^")" in
  let nt = f1 (string_of_symbol (sym_of_parser p)) in
  fun i -> (mkntparser nt (
    (parse_eps >>>> (fun _ -> [])) 
    |||| ((p ***> (star p)) >>>> (fun (x,xs) -> x::xs)) )) i)

(* implicit memo *)
let star p = (
  let f1 x = "STAR("^x^")" in
  let nt = f1 (string_of_symbol (sym_of_parser p)) in
  memo_rec2 (fun self -> (mkntparser nt (
    (parse_eps >>>> (fun _ -> [])) 
    |||| ((p ***> self) >>>> (fun (x,xs) -> x::xs)) ))))

let rec parse_E = (star parse_1)

let p = parse_E
let txt = "111111111111111111111111111111"
let _ = p3_run_parser_string p txt

(* the following gives the same (perhaps unexpected) result; we only allow good trees ! *)
let rec parse_E = (star (parse_eps |||| parse_1))



(* FIXME following should be purely meta - don't need to define new nts *)
(* it is even possible to do something like the following *)
let rec itern n p = (
  let f1 x y = "ITER("^x^","^y^")" in
  let nt = f1 (string_of_int n) (string_of_symbol (sym_of_parser p)) in (* FIXME sym or symbol *)
  (mkntparser nt (
    match n with 
    | 0 -> (parse_eps >>>> (fun _ -> []))
    | _ -> (p ***> (itern (n-1) p)) >>>> (fun (x,xs) -> x::xs))))

let _ = sym_of_parser (itern 5 parse_1)
let _ = grammar_of_parser (itern 5 parse_1)

let p = itern 5 parse_1
let txt = "11111"
let _ = p3_run_parser_string p txt


