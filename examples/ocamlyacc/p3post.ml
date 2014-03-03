(* p3post.ml *)

let toks = 
  let open Parser in
  let open Lexer in
  let s = read_file_as_string args.input in
  let list_of_lexing s0 = (
    let rec f1 xs s0 = (
      let tok = Lexer.token s0 in
      if tok = EOF then tok::xs else f1 (tok::xs) s0)
    in
    List.rev (f1 [] s0))
  in
  let s0 = (Lexing.from_string s) in    
  let toks = Array.of_list (list_of_lexing s0) in
  toks

let main = (
  let p = parse_IMPLEMENTATION in
  let _ = oracle_of_parser p toks (Array.length toks) in
  ())

