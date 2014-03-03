(**
{1 p3_extra.ml}
{2 P3_memo: various ways to memoize the parser combinators}

*)

(*

Interactive use

    #use "topfind";;
    #require "unix";;

    #directory "../p1";;
    #mod_use "p1_terminal_parsers.ml";;
    #mod_use "p1_core.ml";;
    #mod_use "p1_everything.ml";;
    #mod_use "p1_lib.ml";;

    #directory "../earley2";;
    #mod_use "earley2.ml";;
    
    #mod_use "p3_core.ml";;
*)


module P3_memo = struct

  open P3_core

  let memo tbl key_of_input f i = (
    let k = key_of_input i in
    match k with 
    | None -> (f i)
    | Some k -> (
      if (Hashtbl.mem tbl k) then 
        (Hashtbl.find tbl k) 
      else
        let v = f i in
        let _ = Hashtbl.add tbl k v in
        v))
  
  (* N.B. in P3 we drop the i.s4 component from the key; they are not
  needed, and including them causes lots of unnecessary comparisons of
  potentially very long strings *)
  let key_of_input i = (match i with 
    | Inr i -> (
      let ss = i.ss4 in
      let lc4 = normalize_context i i.lc4 ss in
      let SS(s,l,h) = ss in
      let k = (lc4,SS(Box.box_get_key i.box4,l,h)) in (* about 10% slowdown if we include box key *)
      Some k)
    | _ -> None)
  
  let memo_p3 tbl p i = (
    memo tbl key_of_input p i)

  (* version that creates table implicitly *)
  (*
  let memo_check_and_upd_lc4' p = (
    let tbl = Hashtbl.create 100 in
    fun i -> (memo_check_and_upd_lc4 tbl p i))
  *)

  (* see https://groups.google.com/forum/?fromgroups=#!topic/fa.caml/wrCpQFm5Keg 
    let memo_rec f =
      let m = ref [] in
      let rec g x =
        try
          List.assoc x !m
        with
            Not_found ->
              let y = f g x in
                m := (x, y) :: !m ;
                y
      in
        g                                                
  *)

  (* p takes an extra initial argument, its "completion" *)
  
  let memo_rec2 p = (
    let tbl = Hashtbl.create 100 in
    let rec g = (fun i -> 
      let nt = dest_NT (sym_of_parser (p g)) in
      memo tbl key_of_input (p g) i)
    in
    g)
  

  (* some additional functions used by the code generator - slight abbreviations of the above *)
  let mcu4 tbl nt p = memo_p3 tbl (mkntparser nt p)

  (* FIXME not sure about the use of unique everywhere *)
  (*
  let unique4 i = (match i with 
    | OutThree rs -> (OutThree (P1_lib.P1_core.Prelude.unique rs))
    | _ -> i)
  *)

end


(**
{2 P3 basic parsers}

P3 basic parsers (parsers for terminals) need names. We could just use gensym to generate the names, but this makes debugging somewhat harder (because the names of terminal parsers are meaningless). To aid debugging, we here provide basic parsers with somewhat meaningful names.

N.B. clearly two non-identical parsers need different names.

FIXME probably we should use the named versions as the FunctorBasicParsers, and just discard the names if we don't need them.

*)

module P3_basic_parsers = (struct

(*  open Earley.Earley_interface *)
(*  open P1_lib.P1_core.Prelude  
  open P1_lib.P1_core.Types
  open P1_lib.P1_core.Substring *)
  open P1_lib.P1_terminal_parsers.RawParsers
  open P3_core

  type 'a parser123 = 'a P3_core.ty_substring P3_core.parser3'
  type parser3'' = string ty_substring parser3'

  let mktmparser = (P3_core.mktmparser:term -> string P3_core.raw_parser -> parser3'')

  let wrap name (p:P1_lib.P1_core.Types.raw_parser) = mktmparser name (fun (SS(s,i,j)) -> List.map (fun ((s,i,j),y) -> SS(s,i,j)) (p (s,i,j)))

  let (_:term -> P1_lib.P1_core.Types.raw_parser -> parser3'') = wrap

  let q1 s = ("\""^s^"\"") (* quote *)

  let gensym = 
    let n = ref 0 in
    let inc () = (n:=1+(!n));!n in
    let f0 = (fun s -> "__gensym("^(string_of_int (inc()))^","^s^")") in
    f0

  (* string -> substring parser *)
  let a lit = wrap ("a "^(q1 lit)) (a lit)
  
  (* FIXME change this to take an underlying parser *)
  let until_a lit = wrap ("until_a "^(q1 lit)) (until_a lit)    
  
  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = wrap (gensym "parse1") (parse1 pred)  
  
  let parse_EOF = wrap "parse_EOF" (parse_EOF)

  let parse_while pred = wrap (gensym "parse_while") (parse_while pred)
    
  let parse_azAZ = wrap "parse_azAZ" parse_azAZ
  
  let parse_AZ = wrap "parse_AZ" parse_AZ
  
  let parse_az = wrap "parse_az" parse_az
  
  let parse_azs = wrap "parse_azs" parse_azs  
  
  let parse_AZS = wrap "parse_AZS" parse_AZS
  
  let parse_ws = wrap "parse_ws" parse_ws
  
  let parse_epsws = wrap "parse_epsws" parse_epsws
  
  let parse_newline = wrap "parse_newline" parse_newline
  
  let parse_azAZs = wrap "parse_azAZs" parse_azAZs
  
  let parse_notdquote = wrap "parse_notdquote" parse_notdquote
  
  let parse_notsquote = wrap "parse_notsquote" parse_notsquote
  
  let parse_notlt = wrap "parse_notlt" parse_notlt
  
  let parse_notgt = wrap "parse_notgt" parse_notgt
  
  let parse_notltgt = wrap "parse_notltgt" parse_notltgt
  
  let parse_notbracket = wrap "parse_notbacket" parse_notbracket
  
  let parse_notws = wrap "parse_notws" parse_notws
  
  let parse_notcurlyr = wrap "parse_notcurlyr" parse_notcurlyr
  
  let parse_all = wrap "parse_all" parse_all
  
  let parse_num = wrap "parse_num" parse_num
  
  let parse_float = wrap "parse_float" parse_float
  
  let parse_ident = wrap "parse_ident" parse_ident
  
  let parse_never = wrap "parse_never" never

  let term_to_parser s = (match s with
    | "?all?"   -> parse_all
    | "?AZS?" -> parse_AZS
    | "?AZ?" -> parse_AZ
    | "?az?" -> parse_az
    | "?azs?" -> parse_azs
    | "?azAZ?" -> parse_azAZ
    | "?azAZs?" -> parse_azAZs
    | "?EOF?" -> parse_EOF
    | "?epsws?" -> parse_epsws
    | "?ident?" -> parse_ident
    | "?newline?" -> parse_newline
    | "?notbracket?" -> parse_notbracket
    | "?notcurlyr?" -> parse_notcurlyr
    | "?notdquote?" -> parse_notdquote
    | "?notgt?" -> parse_notgt
    | "?notlt?" -> parse_notlt
    | "?notltgt?" -> parse_notltgt
    | "?notsquote?" -> parse_notsquote
    | "?num?" -> parse_num
    | "?float?" -> parse_float
    | "?ws?" -> parse_ws
    | "\"\"" -> a ""
    | _ -> ( (* interpret as a literal *)
        if String.length s < 2 then failwith ("term_to_parser: "^s)
        else
    let _ = () (* print_string ("term_to_parser: treating "^s^" as a literal\n") *) in
    (a (String.sub s 1 (String.length s - 2)))))

  let (_:term -> parser3'') = term_to_parser


end)


;;
