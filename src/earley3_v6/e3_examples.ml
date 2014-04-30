(* 

Examples using the programmatic interface to Earley3

*)

module E3 = Earley3

(* we define two terminals; for Earley_int_interface, terminals are
   odd ints *)
let eps = 1
let a1 = 3

(* note that our notion of terminal parsers (a function) is extremely
   general (the most general!); we must return the ints k representing
   the prefixes (i,k) that could be parsed of the input (i,j) *)
let parse_eps = (fun (s,i,j) -> if i<=j then [i] else [])

let parse_a1 = (fun (s,i,j) -> 
  if i < j && i < String.length s && String.get s i = '1' then 
    [i+1]
  else
    [])

(* function giving the terminal parsers *)
let p_of_tm = (fun tm -> 
  if tm=eps then parse_eps
  else if tm=a1 then parse_a1
  else failwith "p_of_tm: 25")

(* define a nonterminal; for Earley_int_interface, nonterminals are
   even ints *)
let e = 2

(* example grammar: E -> E E E | "1" | eps *)
let g = [
  (e,[e;e;e]);
  (e,[a1]);
  (e,[eps])]

let setup s = (
  object
    method g7=g;
    method length7=String.length s;
    method p_of_tm7=p_of_tm;
    method string7=s;
    method sym7=e
  end)

let e3_parse s = (
  let setup = setup s in
  E3.Earley_int_interface.earley_full setup)

let r = e3_parse "11111"

(* if we want to parse the sequence "E E" between positions 0 and 5,
   where do we cut? *)
let _ = (
  let rs = (r#oracle (e,e) (0,5)) in
  let rs = List.sort Pervasives.compare rs in (* may not be sorted? *)
  assert([0;1;2;3;4;5] = rs))

(* NB if you don't want an oracle, but want the results in some other
   form, just cut and paste the earley_full code from e3_std, and
   adjust the post_process function *)

(* we also have information about the terminal parsers *)
let _ = (assert(true = (r#tmoracle a1 (1,2))))


(* How fast is the earley parser? In a top-level, the following
   returns in about 1s. Compiled this whole file takes about 0.3 s. *)
let r = e3_parse "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111"
