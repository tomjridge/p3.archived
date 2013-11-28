(**
{1 earley_main.ml}
{2 Earley main}

Run core earley parser directly; top-level command line parsing etc

*)

open P1_lib
open P1_core

open Prelude
open Types
open BasicParsers
open Combinator
open CommandLine
open ParseGrammar

(* command line args *)
type ty_cl_args = { input:string; grammar:string; output:bool }
let cl0 = { input="/tmp/input.txt"; grammar="/tmp/grammar.g"; output=true }

(* FIXME output flag is ignored *)

(* precedence to earlier args *)
let rec parse_CL = fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-output",["true"]) -> {cl with output=true }
    | ("-output",["false"]) -> {cl with output=false }
    | ("-f",[a]) -> {cl with input=a }
    | ("-g",[a]) -> {cl with grammar=a }
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

let main () =
  let g = get_grammar args.grammar in
  let txt = read_file_as_string args.input in
  let start_nt = fst (List.hd g) in
  (* let start_sym = `NT(start_nt) in *)
  let setup = `Setup(
    `G7(g),
    `Sym7(start_nt),
    `P_of_tm7(P1_terminal_parsers.RawParsers.term_to_parser),
    `String7(txt,String.length txt)) 
  in
  let l2 = Earley2.Earley_interface.earley_full setup in
  let (l2_done5) = (match l2 with
    | `Loop2(`Done(x),_) -> (x)) 
  in
  (* FIXME remove the following if timing *)
  let i = List.length (Earley2.Earley_interface.Set_sym_item.elements l2_done5) in
  (* let j = List.length (Earley.Earley_interface.Set_nt_item_int.elements l2_prod5) in *)
  let _ = print_endline ("Done items: "^(string_of_int i)) in
  (* let _ = print_endline ("Productions: "^(string_of_int j)) in *)
  ()

let _ = main ()

