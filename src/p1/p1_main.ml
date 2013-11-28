(**
# p1main.ml - Entry points (main functions)

The following command line args are accepted:

  * -f FILENAME (the input file)

  * -g FILENAME (the grammar file)

  * -output (true|false) (default true; whether to print output -
     useful for timing purposes)

  * -memo (true|false) (default true; whether to memoize)

  * -alg (cf|pt) (default cf; whether to parse for compact forms, or
     parse trees)

There are some experimental options:

  * -alg simp (memoization, but no context - the basic combinator
     parsing approach; won't handle recursive grammars)


*)
open P1_core

open Prelude
open Types
open BasicParsers
open Combinator
open Context
open GrammarToParser
open CommandLine
open ParseGrammar

let low = Substring.low

(* command line args *)
type ty_cl_args = { input: string; grammar: string; alg: string; output:bool; memo:bool; basedir:string }
let cl0 = { input="/tmp/input.txt"; grammar="/tmp/grammar.g"; alg="cf"; output=true; memo=true; basedir="." }

(* precedence to earlier args *)
let rec parse_CL = fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-basedir",[a]) -> {cl with basedir=a } (* only for main_gen *)
    | ("-output",["true"]) -> {cl with output=true }
    | ("-output",["false"]) -> {cl with output=false }
    | ("-memo",["true"]) -> {cl with memo=true }
    | ("-memo",["false"]) -> {cl with memo=false }
    | ("-f",[a]) -> {cl with input=a }
    | ("-g",[a]) -> {cl with grammar=a }
    | ("-alg",[a]) -> {cl with alg=a }
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

(* the compact form is a list of elements (nt,(l,h)), indicating that nt could be parsed between l and h *)
let compact_form_of_tbl tbl =
  let r = MyHashtbl.fold (fun k -> fun v -> fun acc ->
    let (nt,lc,(l,h)) = k in
    let v' = List.map (fun (a,s_rem) -> (nt, (l,low s_rem))) v in
    v'@acc) tbl []
  in
  unique r

let main =
  let rs = (parse_GRAMMAR (toinput (full (read_file_as_string args.grammar)))) in
  let _ = if List.length rs = 0 then (failwith ("Failed to parse grammar file: "^args.grammar)) in
  let _ = if List.length rs > 1 then (failwith ("Ambiguous grammar file: "^args.grammar)) in
  let (g,_) = List.hd rs in
  let txt = read_file_as_string args.input in
  let start_sym = `NT(fst (List.hd g)) in
  (
    match args.alg with
      | "cf" -> (
        let tbl = MyHashtbl.create 100 in
        let memo_grammar_to_parser tbl p_of_tm =
          let ps = {
            p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun _ -> tm));
            then_list3=(fun nt -> fun alt -> then_list alt >> (fun _ -> nt));
            check_and_upd_lctxt3=(fun nt -> memo_check_and_upd_lctxt tbl nt);
            unique3=(fun p i -> unique (p i));
          } in
          (fun g sym i -> g2p ps g sym i)
        in
        let p =
          if args.memo then
            memo_grammar_to_parser tbl term_to_parser g start_sym
          else
            failwith "Cannot use -alg cf with -memo false"
        in
        let _ = p (toinput (full txt)) in
        let _ = (match args.output with | false -> () | true ->
          let cfs = compact_form_of_tbl tbl in
          let _ = print_string("Length of cfs: "^(string_of_int (List.length cfs))^"\n") in
          let string_of_cf (nt,(l,h)) = "("^nt^","^(string_of_int l)^","^(string_of_int h)^")" in
          let _ = List.map (fun cf -> print_string ((string_of_cf cf)^"\n")) cfs in
          ())
        in
        ())
      | "pt" -> (
        let tbl = MyHashtbl.create 100 in
        let memo_grammar_to_parser tbl p_of_tm =
          let ps = {
            p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun v -> LF(tm,v)));
            then_list3=(fun nt -> fun alt -> then_list2 nt alt);
            check_and_upd_lctxt3=(fun nt -> memo_check_and_upd_lctxt tbl nt);
            unique3=(fun p i -> p i);
          } in
          (fun g sym i -> g2p ps g sym i)
        in
        let p =
          if args.memo then
            memo_grammar_to_parser tbl term_to_parser g start_sym
          else
            grammar_to_parser term_to_parser g start_sym
        in
        let pts = p (toinput (full txt)) in
        let _ = (match args.output with | false -> () | true ->
          print_string("Length of pts: "^(string_of_int (List.length pts))^"\n"))
        in
        ())
      | "simp" -> (
        let tbl = MyHashtbl.create 100 in
        (* this version discards the context *)
        let memo_check_and_upd_lctxt tbl nt p i =
          let i = { i with lc1=empty_context } in
          (* first look in the global memo table *)
          let k = (nt,i.lc1,lc_substring_of i.sb1) in
          if MyHashtbl.mem tbl k then MyHashtbl.find tbl k else
            (* if not already present then proceed as normal, but remember the value *)
            let v = p i in
            let _ = MyHashtbl.add tbl k v in
            v
        in
        let memo_grammar_to_parser tbl p_of_tm =
          let ps = {
            p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun _ -> tm));
            then_list3=(fun nt -> fun alt -> then_list alt >> (fun _ -> nt));
            check_and_upd_lctxt3=(fun nt -> memo_check_and_upd_lctxt tbl nt);
            unique3=(fun p i -> unique (p i));
          } in
          (fun g sym i -> g2p ps g sym i)
        in
        let p =
          if args.memo then
            memo_grammar_to_parser tbl term_to_parser g start_sym
          else
            failwith "Cannot use -alg simp with -memo false"
        in
        let _ = p (toinput (full txt)) in
        let _ = (match args.output with | false -> () | true ->
          let cfs = compact_form_of_tbl tbl in
          let _ = print_string("Length of cfs: "^(string_of_int (List.length cfs))^"\n") in
          let string_of_cf (nt,(l,h)) = "("^nt^","^(string_of_int l)^","^(string_of_int h)^")" in
          let _ = List.map (fun cf -> print_string ((string_of_cf cf)^"\n")) cfs in
          ())
        in
        ())
      | _ -> (failwith ("Unrecognized algorithm: "^args.alg)))

;;

