(*

This is an example of using a lexer before parsing: the type of the
input can be more-or-less arbitrary (here we use an array of tokens).

With scannerless parsing we may write:

  (a "(")

to indicate that we parse a single open bracket from the input. When
using a lexer, tokens may be of arbitary type, so we now write e.g.:

  (a `OPEN)

*)

(* 

    #directory "../../src/p3_v2//../p1";;
    #mod_use "p1_terminal_parsers.ml";;
    #mod_use "p1_core.ml";;
    #mod_use "p1_everything.ml";;
    #mod_use "p1_lib.ml";;  

    #directory "../../src/p3_v2//../earley2";;
    #mod_use "earley2.ml";;

    #directory "../../src/p3_v2//";;
    #mod_use "p3_core.ml";;
    #mod_use "p3_extra.ml";;
    #mod_use "p3_everything.ml";;
    #mod_use "p3_lib.ml";;

    #mod_use "arith.ml";;

*)

(* we include everything in a dummy module so that we can ignore
   "contains type variables that cannot be generalized" errors *)

module Main = (struct
  
  open P3_lib.P3_everything
  
  let list_of_lexing s0 = (
    let rec f1 xs s0 = (
      let tok = Arith.lex s0 in
      if tok = `EOF then xs else f1 (tok::xs) s0)
    in
    List.rev (f1 [] s0))
  
  type ty_tok = [ `CLOSE | `EOF | `INT of int | `MINUS | `OPEN | `PLUS | `TIMES ]

  let is_INT tok = (match tok with | `INT i -> true | _ -> false)
  
  let parse_INT' = (fun (SS(s,i,j)) -> 
    if i < j && (is_INT (Array.get s i)) then
      [(SS(s,i,i+1),SS(s,i+1,j))]
    else
      [])
  
  let a' tok = (fun (SS(s,i,j)) -> 
    if i < j && Array.get s i = tok then
      [(SS(s,i,i+1),SS(s,i+1,j))]
    else
      [])
    
  type mysubstring = ty_tok array P3_lib.P3_core.ty_substring
  
  let (_:mysubstring -> (mysubstring * mysubstring) list) = parse_INT'
  
  let parse_INT = mktmparser "INT" (fun i -> List.map fst (parse_INT' i))
  
  let a tok = (
    (* notice that `INT is not explicitly matched in the following! *)
    let name = (match tok with
      | `CLOSE -> "CLOSE"
      | `OPEN -> "OPEN"
      | `MINUS -> "MINUS"
      | `PLUS -> "PLUS"
      | `TIMES -> "TIMES"
      | _ -> (failwith "a"))
    in
    mktmparser ("_"^name) (fun i -> List.map fst (a' tok i)))
    
  let tbl_E = MyHashtbl.create 100 

  let rec parse_E = (fun i -> memo_p3 tbl_E (mkntparser "E" (
    parse_PLUS
    |||| parse_MINUS
    |||| parse_TIMES
    |||| (parse_INT >>>> (fun (SS(s,i,j)) -> `LF(Array.get s i)))
    |||| (((a `OPEN) ***> parse_E ***> (a `CLOSE))
             >>>> (fun (_,(x,_)) -> x))))
    i)
  and
      parse_PLUS = (fun i -> (mkntparser "PLUS" (
        ((parse_E ***> (a `PLUS) ***> parse_E)
         >>>> (fun (e1,(_,e2)) -> `NODE("+",e1,e2)))))
        i)
  and
      parse_MINUS = (fun i -> (mkntparser "MINUS" (
        ((parse_E ***> (a `MINUS) ***> parse_E)
         >>>> (fun (e1,(_,e2)) -> `NODE("-",e1,e2)))))
        i)
  and
      parse_TIMES = (fun i -> (mkntparser "TIMES" (
        ((parse_E ***> (a `TIMES) ***> parse_E)
         >>>> (fun (e1,(_,e2)) -> `NODE("*",e1,e2)))))
        i)
  
  (* let _ = p3_run_parser parse_E [||] 0 *)

  let s0 = (Lexing.from_string "1 + 2*(3+4)")
    
  let toks = Array.of_list (list_of_lexing s0)
  
  let rs = p3_run_parser parse_E toks (Array.length toks)
   
  (*
  [`NODE
     ("+", `LF (`INT 1),
      `NODE ("*", `LF (`INT 2), `NODE ("+", `LF (`INT 3), `LF (`INT 4))));
   `NODE
     ("*", `NODE ("+", `LF (`INT 1), `LF (`INT 2)),
      `NODE ("+", `LF (`INT 3), `LF (`INT 4)))]
  *)

  let _ = print_endline ("Number of parsed results: "^(string_of_int (List.length rs)))

end : sig end)
