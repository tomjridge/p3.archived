{{

(* strip comments from code; we want to be able to handle nested comments *)

(* note this doesn't take account e.g. of comment delimiters inside strings etc *)

open P1_lib.P1_terminal_parsers.RawParsers

(* we don't want to parse over a comment *)
let parse_until_significant = 
  let lits = ["(*";"*)"] in
  let llit = 2 in
  fun s ->
    let rec f1 n =
      if
        n+llit <= len s
        && List.mem (String.sub (string s) ((low s)+n) llit) lits
      then
        let (s1,l,h) = s in
        let s2 = (s1,l,l+n) in
        [((s1,l,l+n),(s1,l+n,h))]
      else if
          n+llit <= len s
      then
        f1 (n+1)
      else
        let (s1,l,h) = s in
        [(s,(s1,h,h))]
    in
    f1 0

let term_to_parser s = (match s with
  | "?sig?" -> parse_until_significant
  | _ -> (P1_lib.P1_terminal_parsers.RawParsers.term_to_parser s))

let c = content

}}

START -> S {{ fun s -> print_endline s }}

S -> ?sig? "(*" BRA S {{ fun (x,(y,(z,w))) -> (c x)^w }}
  | ?sig? ?EOF? {{ fun (x,_) -> (c x) }}

(* BRA is inside a comment - parses to a matching closing comment *)
BRA -> ?sig? "*)" {{ fun (x,y) -> "" }}
  | ?sig? "(*" BRA BRA {{ fun _ -> "" }}

