(**
{1 p3_examples.ml}
{2 P3 examples}
*)


open P1_lib.P1_core.Prelude
open P1_lib.P1_core.Types
open P3_lib.P3_everything
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
let _ = assert([6] = p3_run_parser_string p txt)




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
let _ = assert ([30] = p3_run_parser_string p txt)


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
let _ = assert([30] = p3_run_parser_string p txt)






(* defining other combinators; note that these definitions work
   regardless of the input type (lexed tokens, strings, etc) *)
let parse_maybe p = 
  (parse_eps >>>> (fun _ -> None))
  |||| (p >>>> (fun x -> Some x)) 

let p = (parse_maybe parse_1)
let _ = assert ([Some 1] = p3_run_parser_string p "1")
let _ = assert ([None] = p3_run_parser_string p "")
let _ = assert ([] = p3_run_parser_string p "11")


(* iterate a parser n times; following is purely meta - don't need to
   define new nts *)
let rec itern n p = (
  if n = 0 then parse_eps >>>> (fun _ -> [])
  else (p ***> (itern (n-1) p)) >>>> (fun (x,xs) -> x::xs))

let _ = sym_of_parser (itern 5 parse_1)
let _ = grammar_of_parser (itern 5 parse_1)

let p = itern 5 parse_1
let txt = "11111"
let _ = assert ([[1; 1; 1; 1; 1]] = p3_run_parser_string p txt)




(* no memo; star aka many *)
let rec star p = (
  let f1 x = "STAR("^x^")" in
  let nt = f1 (string_of_symbol (sym_of_parser p)) in
  fun i -> (mkntparser nt (
    (parse_eps >>>> (fun _ -> [])) 
    |||| ((p ***> (star p)) >>>> (fun (x,xs) -> x::xs)) )) i)


let rec parse_E = (star parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = p3_run_parser_string p "11111")
let _ = assert ([[]] = p3_run_parser_string p "")

(* the following gives the same (perhaps unexpected) result as the
   above; we only allow good trees! Note the star(parse_eps |||| ...) *)
let rec parse_E = (star (parse_eps |||| parse_1))
let _ = assert([[1; 1; 1; 1; 1]] = p3_run_parser_string parse_E "11111")


(* 1 or more *)
let many1 p = (p ***> (star p)) >>>> (fun (x,xs) -> x::xs)
let rec parse_E = (many1 parse_1)

let p = parse_E
let _ = assert([[1; 1; 1; 1; 1]] = p3_run_parser_string p "11111")
let _ = assert ([] = p3_run_parser_string p "")


(* sepby1, from "Monadic Parser Combinators", Hutton & Meijer, 1996 *)
let sepby1 p sep = (
  let sep_p = (sep ***> p) >>>> (fun (_,x) -> x) in
  (p >>>> (fun x -> [x]))
   |||| ((p ***> (many1 sep_p)) >>>> (fun (x,xs) -> x::xs)))

let p = sepby1 parse_1 (a ";")
let _ = assert([[1;1;1;1]] = p3_run_parser_string p "1;1;1;1")


(* bracket, from "Monadic Parser Combinators" *)
let bracket op p cl = (op ***> p ***> cl) >>>> (fun (_,(x,_)) -> x)

let p1 = sepby1 parse_1 (a ";")
let p = bracket (a "[") p1 (a "]")
let _ = assert([[1;1;1;1]] = p3_run_parser_string p "[1;1;1;1]")

(* etc etc *)




(* NAMES DO NOT HAVE TO BE PART OF THE INTERFACE!!! *)

(* example with gen_string *)

let gen_string () = (
  let open P3_lib.P3_core in
  let open P3_lib.P3_core.Box in
  let i = box_get_key (box_even ()) in
  let s = "_"^(string_of_int i) in
  s)


let rec parse_E = 
  let s = gen_string() in (* and the type of the returned value from gen_string could be made abstract *)
  (fun i -> (mkntparser s (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps))
  i)


let p = parse_E
let _ = grammar_of_parser p
let txt = "111111"
let _ = assert([6] = p3_run_parser_string p txt)



(* Example where the interface mkntparser_no_name takes a ref None,
   the type of which could also be hidden in the interface (using a
   gensym like function get_nt). The use of a reference seems forced
   by the staging of the various computations when e.g. defining
   parse_E *)
(*
let gen_nt () = ref None

let mkntparser_no_name so p = (
  let s = (match !so with 
    | None -> (
      let open P3_lib.P3_core in
      let open P3_lib.P3_core.Box in
      let s = gen_string () in
      let _ = so:=Some(s) in
      s)
    | Some s -> s)
  in
  mkntparser s p)

(* testing mkntparser_no_name *)
let rec parse_E = 
  let so = gen_nt() in 
  (fun i -> (mkntparser_no_name so (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| parse_1
  |||| parse_eps))
  i)


*)

