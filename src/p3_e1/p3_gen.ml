(**
{1 p3gen.ml}
{2 P3 parser generator}

Modules Codegen3 and P3Gen: top-level code generation based on P3

*)

module Codegen3 = struct
  
  open P1_lib.P1_core.Types
  
  let nts_of_grammar = P1_lib.P1_core.Common.nts_of_grammar
  let drop_actions = P1_lib.P1_core.ParseGrammar.drop_actions
  
  
  let str_of_SYM sym = match sym with 
    | `NT nt -> "parse_"^nt
    | `TM tm -> (
      let tm = String.escaped tm in
      "(mktmparser \""^tm^"\" (term_to_parser \""^tm^"\"))")
  
  let str_of_SYMS alt = "(" ^ (String.concat "***>" (List.map str_of_SYM alt)) ^ ")"
  
  let str_of_ACT act = act
  
  let str_of_SYMSACT (alt,act) = "("^ (str_of_SYMS alt) ^" >>>> ("^ (str_of_ACT act) ^"))"
  
  let str_of_RHS rhs = "("^ (String.concat "||||" (List.map str_of_SYMSACT rhs)) ^")"
  
  let str_of_RULE (nt,rhs) = (str_of_SYM (`NT nt))^" = fun i -> (mcu4 tbl_"^nt^" \""^nt^"\" (fun i -> unique4 ("^(str_of_RHS rhs)^" i)) i)"
  
  let str_of_RULES rs = "let rec "^(String.concat "\n\n and " (List.map str_of_RULE rs))
  
  let str_of_GRAMMAR g = (
    let nts = nts_of_grammar (drop_actions g) in
    let g' = List.map (fun nt -> (nt,List.map snd (List.filter (fun (nt',_) -> nt' = nt) g))) nts in
    str_of_RULES g')
    
  (* following just for writing out the grammar as a list or rules *)
  let ocaml_of_SYM sym = (match sym with
    | `NT x -> ("(NT \""^(String.escaped x)^"\")")
    | `TM x -> ("(TM \""^(String.escaped x)^"\")"))
  
  let ocaml_of_SYMS syms = ("["^(String.concat ";" (List.map ocaml_of_SYM syms))^"]")
  
  let ocaml_of_rule (nt,syms) = "(\""^(String.escaped nt)^"\","^(ocaml_of_SYMS syms)^")"
  
  let ocaml_of_grammar g0 = ("["^(String.concat ";\n" (List.map ocaml_of_rule g0))^"]")

end

(* FIXME remember to change this embedded code when changing the p3pre.ml etc files *)
(* code generated courtesy of ocamlify, see src/p3.mlify *)
module Embedded_code = struct
(* Include ./p3pre.ml *)
let p3pre = 
  "\
  \n\
  (* p3pre.ml start *)\n\
  \n\
  open P3_lib\n\
  open P3_everything\n\
  \n\
  (* command line args *)\n\
  type ty_cl_args = { input:string; debug:bool }\n\
  let cl0 = { \n\
  \  input=\"-\"; (* default to stdin *)\n\
  \  debug=false\n\
  }\n\
  \  \n\
  (* precedence to earlier args *)\n\
  let rec parse_CL = \n\
  \  let open P1_lib.P1_core.BasicParsers in\n\
  \  fun i -> (\n\
  \  let f1 (f,xs) cl = (match (f,xs) with\n\
  \    | (\"-f\",[a]) -> {cl with input=a }\n\
  \    | (\"-debug\",[]) -> {cl with debug=true } \n\
  \    | _ -> (failwith (\"parse_CL: unrecognized flag/arg combination: \"^f^\" \"^(String.concat \" \" xs))))\n\
  \  in\n\
  \  let sep = a \"\\x00\" in\n\
  \  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i\n\
  \n\
  let args = get_args parse_CL Sys.argv\n\
  \n\
  (* fix debug at this point *)\n\
  let _ = (debugging:=args.debug)\n\
  \n\
  (* this is no longer included in Everything, so we make it available here *)\n\
  let term_to_parser = P1_lib.P1_terminal_parsers.RawParsers.term_to_parser\n\
  \n\
  (* p3pre.ml end *)\n\
  \n\
  "
;;
(* Include ./p3mid.ml *)
let p3mid = 
  "\
  \n\
  (* p3mid.ml start, end *)\n\
  \n\
  "
;;
(* Include ./p3post.ml *)
let p3post = 
  "\
  \n\
  (* p3post.ml start *)\n\
  \n\
  (* debug, earley only *)\n\
  (*\n\
  let main =\n\
  \  let txt = read_file_as_string args.input in\n\
  \  let p = parse_start in\n\
  \  let _ = earley_prods p txt in\n\
  \  ()\n\
  *)\n\
  \n\
  \n\
  let main = (\n\
  \  let txt = read_file_as_string args.input in\n\
  \  let p = parse_start in\n\
  \  let rs = p3_run_parser p txt in\n\
  \  let _ = (\n\
  \    if (!debugging && rs=[]) then (\n\
  \      let Some(s0) = !Earley.Earley_interface.last_earley_state in\n\
  \      let m = Earley.Earley_debug.max_position s0 in\n\
  \      let _ = debug_endline (\"Failed to parse. Max position: \"^(string_of_int m)) in\n\
  \      let _ = debug_endline (\"Remaining text:\"^(String.sub txt m (String.length txt - (m)))) in\n\
  \      ()\n\
  \    ) else ())\n\
  \  in\n\
  \  ())\n\
  \n\
  \n\
  (* p3post.ml end *)\n\
  \n\
  "
end

module P3Gen = struct

  open P1_lib.P1_core.Prelude
  open P1_lib.P1_core.Types
  open P1_lib.P1_core.Combinator
  open P1_lib.P1_core.BasicParsers  
  open P1_lib.P1_core.CommandLine
  open P1_lib.P1_core.ParseGrammar
  open Codegen3
  
  
  (* command line args *)
  type ty_cl_args = { grammar: string; basedir:string }
  let cl0 = { grammar=""; basedir="" }
  
  (* precedence to earlier args *)
  let rec parse_CL = fun i -> (
    let f1 (f,xs) cl = (match (f,xs) with
      | ("-basedir",[a]) -> {cl with basedir=a } (* only for main_gen *)
      | ("-g",[a]) -> {cl with grammar=a }
      | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
    in
    let sep = a "\x00" in
    (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i
  
  let args = get_args parse_CL Sys.argv
  let _ = (if (args.grammar="") then
      failwith "p3gen: require -g command line argument"
    else
      ())

  (* FIXME following should have same checks on input wellformedness as other mains *)
  let main =
    let rs = (parse_GRAMMAR_WITH_ACTIONS (toinput (full (read_file_as_string args.grammar)))) in
    let ((header,g),_) = (match rs with 
      | [x] -> x | _ -> failwith ("main: failed to parse grammar file: "^args.grammar)) in
    let (start_sym0,_1) = (List.hd g) in
    let start_sym = `NT start_sym0 in
    let nts = nts_of_grammar (drop_actions g) in 
    let tbls = List.map (fun nt -> "let tbl_"^nt^" = MyHashtbl.create 10") nts in
    let tbl_reset = 
      "let tbl_reset _ = ("^"\n"^
      (String.concat "" (List.map (fun x -> "  let _ = MyHashtbl.clear tbl_"^x^" in \n") nts)) ^
      "())\n"
    in
    let tbls = (String.concat "\n" tbls)^"\n"^tbl_reset in
    let s = str_of_GRAMMAR g in
    let s = s ^ "\nlet parse_start = "^(str_of_SYM start_sym)^"\n" in
    let (p3pre,p3mid,p3post) = (
      (* if no args.basedir argument, default to the embedded code *)
      if (args.basedir="") then
        Embedded_code.(p3pre,p3mid,p3post) 
      else (
        let p3pre = (read_file_as_string (args.basedir^"/p3pre.ml")) in
        let p3mid = (read_file_as_string (args.basedir^"/p3mid.ml")) in
        let p3post = (read_file_as_string (args.basedir^"/p3post.ml")) in
        (p3pre,p3mid,p3post)))
    in
    let _ = print_string (
      p3pre^"\n"^
        header^"\n"^
        p3mid^"\n"^
        tbls^"\n"^
        s^
        p3post)
    in
    ()
  
end

