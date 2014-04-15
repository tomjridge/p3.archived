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

(* convenience method to transform a grammar using strings into a
   grammar where nts and tms are ints (even, odd respectively *)

module Map_sym = Map.Make(struct type t = P1_lib.P1_everything.symbol let compare = Pervasives.compare end)

module Map_int = E3_mods.Std_map_int

let mk_map g = (
  let counter = ref 0 in
  let even () = (
    let c = !counter in
    let c = if (c mod 2 = 0) then c else c+1 in
    let _ = counter := c+1 in
    c)
  in
  let odd () = (
    let c = !counter in
    let c = if (c mod 2 = 1) then c else c+1 in
    let _ = counter := c+1 in
    c)
  in
  let syms = P1_lib.P1_everything.syms_of_grammar g in
  let find k m = (
    try Some(Map_sym.find k m) with Not_found -> None)
  in             
  let get_int sym = (match sym with `NT _ -> even() | `TM _ -> odd()) in
  let f1 (m_si,m_is) sym = ( (* maps from sym to int, and int to sym *)
    match find sym m_si with
    | None -> (
      let i = (get_int sym) in
      let m_si = Map_sym.add sym i m_si in
      let m_is = Map_int.add i sym m_is in
      (m_si,m_is))
    | Some _ -> (m_si,m_is))
  in
  let m = List.fold_left f1 (Map_sym.empty,Map_int.empty) syms in
  m)

let map_grammar m_si g = (
  let f2 m rhs = List.map (fun sym -> Map_sym.find sym m_si) rhs in
  let f1 m (nt,rhs) = (Map_sym.find (`NT nt) m_si, f2 m_si rhs) in
  List.map (f1 m_si) g)

(* here we need the reverse map - from ints to terms *)
let map_tmparsers m_is tmps = (fun tm -> 
  let tm = Map_int.find tm m_is in
  let tm = (match tm with | `TM tm -> tm | _ -> failwith "map_tmparsers: impossible") in
  tmps tm)

let integerize_grammar (g,tmps) = (
  let (m_si,m_is) = mk_map g in
  (map_grammar m_si g, map_tmparsers m_is tmps))

(* FIXME in the following we should really check that the results of parsing are sensible *)
let main () = (
  let g = get_grammar args.grammar in
  let txt = read_file_as_string args.input in
  let len = String.length txt in
  (* FIXME change type of raw parser? and p1 should wrap the type that produces a list of int *)
  let tmps = (fun tm -> fun (s,i,j) -> 
        let rs = P1_terminal_parsers.RawParsers.term_to_parser tm (s,i,j) in
        List.map (fun ((s,i,j),_) -> j) rs)
  in
  match args.uni with 
  | "std" -> (
    let (g,tmps) = integerize_grammar (g,tmps) in
    let start_nt = fst (List.hd g) in
    let setup = (object
      method g7=g;
      method sym7=start_nt;
      method p_of_tm7=tmps;
      method string7=txt;
      method length7=len;
    end) in
    let i = (
      let open Earley3 in
      let l2 = Earley_int_interface.earley_full setup in
      -1) (* FIXME do something with the number of items *)
    in
    (* let j = List.length (Earley.Earley_interface.Set_nt_item_int.elements l2_prod5) in *)
    let _ = print_endline ("Done items: ?"(* ^(string_of_int i) *)) in
    (* let _ = print_endline ("Productions: "^(string_of_int j)) in *)
    ())
  | _ -> (
    let start_nt = fst (List.hd g) in
  (* let start_sym = `NT(start_nt) in *)
    let (setup:string Earley3.Earley_interface.ty_setup) = (object
      method g7=g;
      method sym7=start_nt;
      method p_of_tm7=tmps;
      method string7=txt;
      method length7=len;
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
    ()))

let _ = main ()

