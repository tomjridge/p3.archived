open P3_lib;;
open P3_everything;;

let from = Sys.argv.(1)

let _ = (
  let [((h,g),_)] = parse_GRAMMAR_WITH_ACTIONS (toinput (full (read_file_as_string from))) in
  let (h,g) = mk_pt_actions (h,g) in
  (* alter the first action to actually print out debugging information *)
  let f1 (nt,(syms,s)) = (nt,(syms,"fun x -> let r = ("^s^") x in print_endline (string_of_pt r); print_newline (); r")) in
  let (r::rs) = g in
  let g = (f1 r)::rs in  
  let s = unparse_GRAMMAR_WITH_ACTIONS (h,g) in
  let _ = print_endline s in
  ())

