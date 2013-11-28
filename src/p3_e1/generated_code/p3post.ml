
(* p3post.ml start *)

(* debug, earley only *)
(*
let main =
  let txt = read_file_as_string args.input in
  let p = parse_start in
  let _ = earley_prods p txt in
  ()
*)


let main = (
  let txt = read_file_as_string args.input in
  let p = parse_start in
  let rs = p3_run_parser p txt in
  let _ = (
    if (!debugging && rs=[]) then (
      let Some(s0) = !Earley.Earley_interface.last_earley_state in
      let m = Earley.Earley_debug.max_position s0 in
      let _ = debug_endline ("Failed to parse. Max position: "^(string_of_int m)) in
      let _ = debug_endline ("Remaining text:"^(String.sub txt m (String.length txt - (m)))) in
      ()
    ) else ())
  in
  ())


(* p3post.ml end *)

