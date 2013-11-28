
(* p3pre.ml start *)

open P3_lib
open P3_everything

(* command line args *)
type ty_cl_args = { input:string; debug:bool }
let cl0 = { 
  input="-"; (* default to stdin *)
  debug=false
}
  
(* precedence to earlier args *)
let rec parse_CL = 
  let open P1_lib.P1.BasicParsers in
  fun i -> (
  let f1 (f,xs) cl = (match (f,xs) with
    | ("-f",[a]) -> {cl with input=a }
    | ("-debug",[]) -> {cl with debug=true } 
    | _ -> (failwith ("parse_CL: unrecognized flag/arg combination: "^f^" "^(String.concat " " xs))))
  in
  let sep = a "\x00" in
  (((listof parse_FLARGS sep) **> parse_EOF) >> (fun (xs,_) -> itlist f1 xs cl0))) i

let args = get_args parse_CL Sys.argv

(* fix debug at this point *)
let _ = (debugging:=args.debug)

(* this is no longer included in Everything, so we make it available here *)
let term_to_parser = P1_lib.P1_terminal_parsers.RawParsers.term_to_parser

(* p3pre.ml end *)

