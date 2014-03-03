(**
{1 earley_main.ml}
{2 Earley main}

Run core earley parser directly; top-level command line parsing etc

*)

(*

#directory "../_build"
#load "p1.cma";;
#mod_use "universe.ml";;
#mod_use "earley3_int.ml";;
#mod_use "uni_earley.ml";;
#mod_use "earley3.ml";;

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
type ty_cl_args = { input:string; grammar:string; output:bool; uni:string }
let cl0 = { input="/tmp/input.txt"; grammar="/tmp/grammar.g"; output=true; uni="" }

(* FIXME output flag is ignored *)

(* precedence to earlier args *)
let rec parse_CL = fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-output",["true"]) -> {cl with output=true }
    | ("-output",["false"]) -> {cl with output=false }
    | ("-f",[a]) -> {cl with input=a }
    | ("-g",[a]) -> {cl with grammar=a }
    | ("-u",[a]) -> {cl with uni=a }
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

let main () = (
  let g = get_grammar args.grammar in
  let txt = read_file_as_string args.input in
  let start_nt = fst (List.hd g) in
  (* let start_sym = `NT(start_nt) in *)
  let (setup:string Earley3.Earley_public_types.ty_setup) = (object
    method g7=g;
    method sym7=start_nt;
    method p_of_tm7=(fun tm -> fun (s,i,j) -> 
        let rs = P1_terminal_parsers.RawParsers.term_to_parser tm (s,i,j) in
        List.map (fun ((s,i,j),_) -> j) rs); (* FIXME change type of raw parser? and p1 should wrap the type that produces a list of int *)
    method string7=txt;
    method length7=(String.length txt);
    method uni7=(match args.uni with
    | "U4" -> (Some `U4)
    | "U4_imp" -> (Some `U4_imp)
    | "U6" -> (Some `U6)
    | "U7" -> (Some `U7)
    | "U8" -> (Some `U8)
    | "" -> None
    );
  end) in
  let i = (
    let open Earley3 in
    let l2 = Earley_interface.earley_full setup in
    -1) (* FIXME do something with the number of items *)
  in
  (* let j = List.length (Earley.Earley_interface.Set_nt_item_int.elements l2_prod5) in *)
  let _ = print_endline ("Done items: "^(string_of_int i)) in
  (* let _ = print_endline ("Productions: "^(string_of_int j)) in *)
  ())

let _ = main ()

