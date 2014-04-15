(*
# Graph of dependencies

An SVG file showing dependencies is:

<img src='code_modules.svg' width='800' />

An informal description of the contents of these files is as follows:

  * `p1.ml`: basic combinator parsing, with context
  * `earley.ml`: Earley parsing
  * `p2gen.ml`: parser generator, using information from the Earley parsing phase to seed the combinator parsing of `p1.ml`
  * `p3.ml`: synthesis of combinator parsing from `p1.ml` and Earley parsing
  * `p3gen.ml`: parser generator based on `p3.ml`
  * `p3_lib.ml`: not shown above, basically everything in one file (actually equal to `p3.ml`)

`p1.ml` modules are:

  * Prelude
  * Types
  * Substring
  * Common
  * RawParsers
  * BasicParsers
  * Combinator
  * Context
  * GrammarToParser
  * ParseGrammar
  * CommandLine
  * Codegen

`p1main.ml` adds the following to `p1.ml`:

  * P1Main

`earley.ml` adds the following to `p1.ml`:

  * EarleyTypes
  * EarleyCore - core algorithm
  * Earley - wraps up core algorithm to be used by others

`earley_main.ml` adds the following to `earley.ml`:

  * EarleyMain - main routine for running earley code directly (without P3)

`p2gen.ml` adds the following to `p1.ml` (really earley.ml conceptually, since the generated code depends on earley.ml):

  * P2Gen - parser generator; generated code relies on earley

`p3.ml` adds the following to `earley.ml`:

  * P3 - combinator parsing, integrated with Earley parsing

`p3gen.ml` adds the following to `p3.ml`:

  * Codegen3 - code generation based on P3, string output functions; used by P3Gen
  * P3Gen - code generation based on P3

`p3_lib.ml` is basically everything, without the main code such as `earley_main.ml`. *You probably want to use `p3_lib.ml` when developing your own parsers.*


*)
(*
# Interactive top-level directives

In bash, before running toplevel, or executing native code, the following may improve performance (but is not necessary):

    # from http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual024.html
    export OCAMLRUNPARAM="s=8M,l=8M,i=1M"

Then start the ocaml toplevel interpreter:

    ocaml

And type the following to load the `p3_lib.ml` file:

    #cd "/tmp/l/general/research/parsing/src";; (* or wherever the p3_lib.ml file is located *)
    #use "p3_lib.ml";;

You should then be able to e.g. execute the code inside the `P3Examples` module.

*)
(*
# p1.ml - Combinators
## Prelude - lots of ugly stuff
*)

module Prelude = struct
  
  (* disable debugging *)
  let debugging = ref false
  let debug_endline s = (if !debugging then (print_endline s) else ())
  
  (* careful - this is taken from hol_light_lib.ml and maybe overwritten (with different mutable state) when including hol_light_lib.ml *)
  
  let report_timing = ref false;;
  
  let report s =
    Format.print_string s; Format.print_newline();;
  
(* removed so that we can cut and paste p3_lib.ml into http://try.ocamlpro.com/
  let time f x =
    if not (!report_timing) then f x else
    let start_time = Sys.time() in
    try let result = f x in
        let finish_time = Sys.time() in
        report("CPU time (user): "^(string_of_float(finish_time -. start_time)));
        result
    with e ->
        let finish_time = Sys.time() in
        Format.print_string("Failed after (user) CPU time of "^
                            (string_of_float(finish_time -. start_time))^": ");
        raise e;;
*)  
  
  module type MYSET = sig
    type elt 
    type t
    val add : elt -> t -> t
    val choose : t -> elt
    val diff : t -> t -> t
    val elements : t -> elt list
    val empty : t
    val filter : (elt -> bool) -> t -> t
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val from_list : elt list -> t
    val is_empty : t -> bool
    val list_union : elt list -> t -> t
    val map : (elt -> elt) -> t -> t
    val maximal_less_than : elt -> t -> elt option
    val mem : elt -> t -> bool
    val remove : elt -> t -> t
    val split : elt -> t -> t * bool * t
    val union : t -> t -> t
  end
  
  module MySet_Make = functor (Ord:Set.OrderedType) -> (struct
    include Set.Make(Ord)
    let maximal_less_than e s = (
      let (smaller,_,larger) = split e s in
      if (is_empty smaller) then None else (Some(max_elt smaller)))
    let rec itlist f l b =
      match l with
        [] -> b
      | (h::t) -> f h (itlist f t b)
    let list_union xs s =
      itlist (fun x -> fun s -> (*Set_earley_item.*)add x s) xs s
    let map f s =
      let f1 x s = (*Set_earley_item.*)add (f x) s in
      (*Set_earley_item.*)fold f1 s (*Set_earley_item.*)empty
    let from_list elts = 
      let f1 elt s = add elt s in
      itlist f1 elts empty
  end : MYSET with type elt = Ord.t)  

  (* we want to override the create function to provide a global_clear method *)
  module MyHashtbl = struct
    include Hashtbl
    let reset_funs = ref []
    let original_create = create
    let create n = (
      let tbl = original_create n in 
      let _ = (reset_funs := (fun () -> Hashtbl.clear tbl)::(!reset_funs)) in (* NB not concurrent safe; prefer reset to clear *)
      tbl)
    let global_reset () = (List.map (fun f -> f ()) (!reset_funs); ())
  end
  
  (* basic library functions *)
  
  type ('a,'b) sum = Inl of 'a | Inr of 'b
  
  let dest_Inl x = (match x with | Inl x -> x | _ -> failwith "dest_Inl")
  let dest_Inr x = (match x with | Inr x -> x | _ -> failwith "dest_Inr")
  
  (* FIXME change names of predefined combinators to reflect use of not_epsilon (i.e. default is epsilon) *)
  
  let rec itlist f l b =
    match l with
      [] -> b
    | (h::t) -> f h (itlist f t b);;
  
  let rec mem x lis =
    match lis with
      [] -> false
    | (h::t) -> Pervasives.compare x h = 0 or mem x t;;
  
  let insert x l =
    if mem x l then l else x::l;;
  
  let union l1 l2 = itlist insert l1 l2;;
  
  let unions l = itlist union l [];;
  
  
  let ($) f g x = f(g x)
  
  (*
  let read_file_as_string fn = 
    let f = open_in fn in
    let s = ref "" in
    let _ = try (while(true) do s := (!s) ^ (input_line f) ^ "\n" done) with _ -> () in
    let _ = close_in f in
    !s
  *)
  
  let lines fname = 
    let lines = ref [] in
    let chan = if fname="-" then Pervasives.stdin else open_in fname in
    try
      while true; do
        lines := input_line chan :: !lines
      done; []
    with End_of_file ->
      close_in chan;
      List.rev !lines
  
  let read_file_as_string fn = 
    let ls = lines fn in
    ((String.concat "\n" ls)^"\n")
  
  
  (* our contexts are sorted; we need insertion into a sorted list; we expect no duplicates  *)
  let rec myinsert cmp elt lst = match lst with
    [] -> [elt]
  | head :: tail -> let r = cmp elt head in if r < 0  then elt :: lst else (
    if r = 0 then failwith "myinsert" else head :: myinsert cmp elt tail)
  
  (* get a list with no duplicates; inefficient? FIXME do we mean List.memq? *)
  let unique_f res e = if List.mem e res then res else e::res
  
  (* this is insertion sort; alternatives? *)
  let unique = fun e -> List.fold_left unique_f [] e
  
  let is_Some x = x <> None
  
  let dest_Some x = match x with Some y -> y | _ -> failwith "dest_Some"

  let rec allpairs f l1 l2 =
    match l1 with
     h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
     | [] -> []
  
end

(*
## Types
*)

module Types = struct

  type term = string
  
  type nonterm = string
  
  let string_of_tm tm = tm
  
  let string_of_nt nt = nt
  
  let eps = "\"\""
  
  type substring = string * int * int
  
  type symbol = NT of nonterm | TM of term
  
  let is_NT s = (match s with NT _ -> true | _ -> false)
  
  let dest_NT sym = (match sym with NT x -> x | _ -> failwith "dest_NT")
  
  let is_TM sym = (match sym with TM _ -> true | _ -> false)
  
  let dest_TM sym = (match sym with NT _ -> failwith "dest_TM" | TM tm -> tm)

  let string_of_symbol sym = match sym with | NT nt -> "NT("^(string_of_nt nt)^")" | TM tm -> "TM("^(string_of_tm tm)^")"
    
  (* NB this used to be (symbol list) list, but this was a bit stupid *)
  type rhs = symbol list
  
  type parse_rule = nonterm * rhs
  
  type grammar = parse_rule list
  
  type parse_tree = NODE of nonterm * parse_tree list | LF of term * substring

  (* FIXME following just for debugging - improve output *)  
  let content (s,l,h) = String.sub s l (h-l)
  let rec string_of_pt pt = (match pt with
    | LF(x,s) -> ("LF("^x^","^(String.escaped (content s))^")")
    | NODE(x,pts) -> (
        "NODE("^
         x^","^"["^
         (String.concat "," (List.map string_of_pt pts))^
         "])"))

  (* local_context invariant: each entry has the same substring (ie the
  most restrictive substring); entries are sorted on nonterm *)
  
  type lc_substring = int * int
  
  type local_context = LC of (nonterm * lc_substring) list 

  (* some basic functions for the context *)
  let empty_context = LC []
  
  type ty_input1 = { lc1 : local_context; sb1 : substring }
  
  (*
  type 'a ty_input2 = { lc1 : local_context; sb1 : 'a }
  *)

  type raw_parser = substring -> (substring * substring) list
  
  type 'a ty_parser = ty_input1 -> ('a * substring) list

  (* packaging a grammar with the relevant terminal parsers - here restricted to be finite *)
  type grammar8 = { g8:grammar; raw_parsers8:(term * raw_parser) list }
  
  type ty_p_of_tm = term -> substring ty_parser
  
  (* collecting all the bits *)
  type ty_setup = {
    g7      : grammar                     ; (* grammar *)                           
    sym7    : nonterm                     ; (* start symbol *) (* FIXME nt7 not sym7? or change nonterm to symbol *)
    p_of_tm7: term -> raw_parser          ; (* term_to_parser *)     
    string7 : string                      ; (* input *)         
  }
  
  
  (* memoization *)
  type key = (nonterm * local_context * lc_substring)
  
  type ty_compact_form = (nonterm * lc_substring) list
  
  (* moved to earley 
  (* parse results *)
  type parsed_sym = (symbol * lc_substring)
  type parse_result = 
    | PNODE of (nonterm * lc_substring) * (parsed_sym list)
    | PLEAF of (term * lc_substring)
  *)
  
  (* grammar_to_parser parameterization *)
  
  type 'a g2p_params = {
    p_of_tm3: term -> 'a ty_parser;
    then_list3: nonterm -> 'a ty_parser list -> 'a ty_parser;
    check_and_upd_lctxt3: nonterm -> 'a ty_parser -> 'a ty_parser;
    unique3: 'a ty_parser -> 'a ty_parser;
  }
  
end (* Types *)

(*
## Substrings
*)

module Substring = struct

(*  let insert = Prelude.insert *)

  let string (s,l,h) = s
  
  let (low,high,len) = (
    (fun (s,l,h) -> l), 
    (fun (s,l,h) -> h), 
    (fun (s,l,h) -> h-l))
  
  let full s = (s,0,String.length s)
  
  let dec_low n (s,l,h) = (s,l-n,h)
  let inc_low n (s,l,h) = (s,l+n,h)
  let dec_high n (s,l,h) = (s,l,h-n)
  let inc_high n (s,l,h) = (s,l,h+n)
  
  let content s = 
    String.sub (string s) (low s) (len s)
  
  let concatenate_two s1 s2 = 
    if (string s1 = string s2) && (high s1 = low s2) then
      Some (string s1, low s1, high s2)
    else
      None
  
  let rec concatenate_list ss = match ss with 
    [] -> None
  | s1::ss -> (match ss with
      [] -> Some s1
    | _ -> (match concatenate_list ss with 
        None -> None
    |   Some s2 -> concatenate_two s1 s2))
  
end

(*
## Common functions
*)

module Common = struct

  open Prelude  
  open Types
  
  let string_of_substring (s,l,h) = "("^s^","^(string_of_int l)^","^(string_of_int h)^")"
  
  let lc_substring_of (s,l,h) = (l,h)
  
  let eps = TM(eps) (* fix one particular terminal for eps *)
  
  let toinput s = { lc1=empty_context; sb1=s } 
  
  let (_:substring -> ty_input1) = toinput
  
  let substr i = i.sb1
  
  let (_:ty_input1 -> substring) = substr
  
  let lift f i = { i with sb1=(f i.sb1) }
  
  let (_: (substring -> substring) -> (ty_input1 -> ty_input1)) = lift  
  
  let syms_of_rhs rhs = rhs
  
  let syms_of_parse_rule (nt,rhs) = insert (NT(nt)) (syms_of_rhs rhs)
  
  let syms_of_grammar g = unions (List.map syms_of_parse_rule g)
  
  let nts_of_grammar g = (
    let syms = syms_of_grammar g in
    let nts = List.map dest_NT (List.filter is_NT syms) in
    nts)


  (* sometimes we need to get the underlying function from substrings rather than inputs *)
  let raw_parser_of_parser (p:substring ty_parser) s = (p (toinput s))

  let (_:substring ty_parser -> raw_parser) = raw_parser_of_parser

  let parser_of_raw_parser p i = p (i.sb1)
  
  let (_:raw_parser -> substring ty_parser) = parser_of_raw_parser
  
end

(*
## Raw parsers, of type `substring -> (substring * substring) list`

Raw parsers are typically those parsers corresponding to terminals.

*)

module RawParsers = struct

  open Prelude  
  open Types
  open Substring
 
  type rawparser = substring -> (substring * substring) list 

  (* these combinators are only for raw parsers *)
  let ( **>@ ) p1 p2 = (fun s ->
    let f (e1,s1) = 
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 s1) 
    in
    ((List.concat $ (List.map f) $ p1) s))

  let ( |||@ ) p1 p2 = (fun s -> List.append (p1 s) (p2 s))

  let (>>@) p f = (List.map (fun (e,s) -> (f e, s))) $ p

  let never = fun i -> []

  let noteps p = (fun s -> 
    List.filter (fun (_,srem) -> srem <> s) (p s))


  let a lit = fun s ->
    let n = String.length lit in
    if 
      (n <= len s) 
      && (String.sub (string s) (low s) n = lit) 
    then
      let (s1,l,h) = s in
      let s2 = (s1,l,l+n) in
      [(s2,inc_low n s)]
    else
      []
  
  let (_:raw_parser) = (a "1")
  
  (* FIXME change this to take an underlying parser *)
  let until_a lit = fun s -> 
    let llit = String.length lit in
    let rec f1 n =
      if 
        n+llit <= len s
        && (String.sub (string s) ((low s)+n) llit) = lit
      then
        let (s1,l,h) = s in
        let s2 = (s1,l,l+n) in
        [(s2,inc_low n s)]
      else if 
          n+llit <= len s
      then 
        f1 (n+1)
      else
        let (s1,l,h) = s in
        [(s,(s1,h,h))]
    in
    f1 0    
  
  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = fun s -> 
    if (1 <= len s && pred (String.sub (string s) (low s) 1)) then
      [((string s, low s, 1+low s),inc_low 1 s)]
    else 
      []
    
  let parse_EOF = fun s -> (
    if (low s = high s) && (high s = String.length (string s)) then 
      (a "") s
    else 
      never s)
  
  (* can return eps; FIXME this is incredibly dangerous, and breaks wf of terminal parsers *)
  let parse_while pred = fun s ->
    let rec f = fun n -> 
      if n = len s then len s else
      let c = String.sub (string s) ((low s)+n) 1 in
      if pred c then f (n+1) else n 
    in
    let n = f 0 in
    let r = (string s, low s, (low s)+n) in
    [(r,inc_low n s)]
  
  let (_:(string -> bool) -> raw_parser) = parse_while
  
  (* FIXME could tidy up the following *)
  
  let parse_azAZ = 
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    parse1 pred
  
  let (_:raw_parser) = parse_azAZ
  
  let parse_AZ = 
    let pred c = 
      (String.compare "A" c <= 0) 
      && (String.compare c "Z" <= 0) 
    in
    parse1 pred
  
  let parse_az = 
    let pred c = 
      (String.compare "a" c <= 0) 
      && (String.compare c "z" <= 0) 
    in
    parse1 pred
  
  let parse_azs = 
    let pred c = 
      (String.compare "a" c <= 0) 
      && (String.compare c "z" <= 0) 
    in
    parse_while pred
    
  let parse_AZS = 
    let pred c = 
      (String.compare "A" c <= 0) 
      && (String.compare c "Z" <= 0) 
    in
    noteps (parse_while pred)
  
  let parse_ws = noteps (parse_while (fun s -> s = " " || s = "\n"))
  
  let parse_epsws = (parse_while (fun s -> s = " " || s = "\n"))
  
  let parse_newline = a "\n"
  
  let parse_azAZs = 
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    noteps (parse_while pred)
  
  let parse_notdquote = 
    parse_while (fun c -> not (c = "\""))
  
  let parse_notsquote = 
    parse_while (fun c -> not (c = "'"))
  
  let parse_notlt = 
    parse_while (fun c -> not (c = "<"))
  
  let parse_notgt = 
    parse_while (fun c -> not (c = ">"))
  
  let parse_notltgt = 
    parse_while (fun c -> not ((c = "<") || (c = ">")))
  
  let parse_notbracket = 
    parse_while (fun c -> not ((c = "(") || (c = ")")))
  
  let parse_notws =
    parse_while (fun c -> not (c = " "))
  
  let parse_notcurlyr = parse_while (fun c -> not (c = "}"))
  
  let parse_all = 
    parse_while (fun c -> true)
  
  let parse_num = 
    let pred = fun c ->
      (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    in
    noteps (parse_while pred)
  
  (* the following is hopeless, obviously; nums are non-empty  *)
  let parse_float = ((parse_num **>@ (a ".") **>@ parse_num) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z])))
  
  let parse_ident = 
    let pred = fun c -> 
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
      || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
      || (c = "_") || (c = "'")
    in
    noteps (parse_while pred)
  
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
    
  let (_:term -> raw_parser) = term_to_parser

end


(*
## Functorized raw parsers

We lift the raw parsers to operate over an arbitrary input type. In BasicParsers We then instantiate the input type to `ty_input1`.

*)

module type SubstringPlus = sig
  open Types  
  type ty_input
  val substr:ty_input -> substring
  val with_substr:ty_input -> substring -> ty_input
end

(*
module A = (Input : SubstringPlus)
*)

module FunctorBasicParsers = functor(A:SubstringPlus) -> struct

  open Prelude  
  open Types
  open Substring
  open RawParsers

  type ty_input = A.ty_input

  type 'a myparser = A.ty_input -> ('a * substring) list

  let seq p1 p2 = (fun i ->
    let f (e1,s1) = 
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 (A.with_substr i s1))
    in
    ((List.concat $ (List.map f) $ p1) i))

  let alt p1 p2 = (fun i -> List.append (p1 i) (p2 i))

  let with_action p f = (fun i -> List.map (fun (e,s) -> (f e, s)) (p i))

  let wrap p i = (p (A.substr i))

  let (_:rawparser -> substring myparser) = wrap

  (* these combinators are only for raw_parsers *)
  let ( **>@ ) p1 p2 = seq p1 p2

  let ( |||@ ) p1 p2 = alt p1 p2

  let (>>@) p f = with_action p f

  (* string -> substring ty_parser *)
  let a lit = wrap (a lit)
  
  let (_:substring myparser) = (a "1")
  
  (* FIXME change this to take an underlying parser *)
  let until_a lit = wrap (until_a lit)    
  
  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = wrap (parse1 pred)  
  
  let parse_EOF = wrap (parse_EOF)
  
  let parse_while pred = wrap (parse_while pred)
  
  let (_:(string -> bool) -> substring myparser) = parse_while
  
  (* FIXME could tidy up the following *)
  
  let parse_azAZ = wrap parse_azAZ
  
  let (_:substring myparser) = parse_azAZ
  
  let parse_AZ = wrap parse_AZ
  
  let parse_az = wrap parse_az
  
  let parse_azs = wrap parse_azs  
  
  let parse_AZS = wrap parse_AZS
  
  let parse_ws = wrap parse_ws
  
  let parse_epsws = wrap parse_epsws
  
  let parse_newline = wrap parse_newline
  
  let parse_azAZs = wrap parse_azAZs
  
  let parse_notdquote = wrap parse_notdquote
  
  let parse_notsquote = wrap parse_notsquote
  
  let parse_notlt = wrap parse_notlt
  
  let parse_notgt = wrap parse_notgt
  
  let parse_notltgt = wrap parse_notltgt
  
  let parse_notbracket = wrap parse_notbracket
  
  let parse_notws = wrap parse_notws
  
  let parse_notcurlyr = wrap parse_notcurlyr
  
  let parse_all = wrap parse_all
  
  let parse_num = wrap parse_num
  
  let parse_float = wrap parse_float
  
  let parse_ident = wrap parse_ident
  
  let term_to_parser s = wrap (term_to_parser s)
    
  let (_:term -> substring myparser) = term_to_parser

end


(*
## Basic parsers

We lift the raw parsers to take inputs of type `ty_input1` rather than `substring`. N.B. these bindings shadow those from RawParsers - don't get confused! As with raw parsers, basic parsers correspond to terminals in the grammar. However, here they take a type `ty_input1` rather than `substring`.

*)

module BasicParsers = struct 

  (* desired instantiation *)
  module Input = struct
    open Types
    type ty_input = ty_input1
    let substr i = i.sb1
    let with_substr i s = {i with sb1=s}
  end 

  include FunctorBasicParsers(Input)

end

(*
## Combinators

It is worth noting that nothing in the following definitions depends on the notion of context. Context comes later, and is modularly combined with the following.

*)

module Combinator = struct 

  open Prelude  
  open Types
  open Substring
  
  let substr = Common.substr
  let lift = Common.lift
  
  (* 'a ty_parser -> ('a -> 'b) -> 'b ty_parser *)
  let (>>) p f = 
    (List.map (fun (e,s) -> (f e, s))) $ p
  
  (* 'a ty_parser -> 'a ty_parser -> 'a ty_parser *)
  let (|||) p1 p2 = fun s -> List.append (p1 s) (p2 s)
  
  (* a version of the combinator that ignores duplicate entries FIXME *)
  let ( **> ) p1 p2 = fun i ->
    let f (e1,s1) = 
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 { lc1=i.lc1; sb1=s1 }) 
    in
    ((List.concat $ (List.map f) $ p1) i)
  
  let (_:'a ty_parser -> 'b ty_parser -> ('a * 'b) ty_parser) = ( **> )
  
  
  let always = fun i -> [([],substr i)]
  
  let never = fun i -> []
  
  
  let rec then_list ps = match ps with 
  | [] -> always
  | p::ps -> (p **> (then_list ps)) 
      >> (fun (x,xs) -> (x::xs))
  
  let then_list2 nt = fun ps -> then_list ps >> (fun xs -> NODE(nt,xs))
  
  
  let rec or_list ps = match ps with 
  | [] -> never
  | p::ps -> (p ||| (or_list ps))
  
  
  (* 'a ty_parser -> 'a ty_parser *)
  let ignr_last p = fun i -> 
    if len (substr i) = 0 then [] else
    let inc_high (e,s) = (e,inc_high 1 s) in 
    ((List.map inc_high) $ p $ (lift (dec_high 1))) i
  
  let (_:'a ty_parser -> 'a ty_parser) = ignr_last
  
  
  let not_epsilon p = fun i ->
    List.filter (fun (v,_) -> not (len v = 0)) (p i)
  
  let (_:substring ty_parser -> substring ty_parser) = not_epsilon
  
  let noteps p = fun i -> 
    List.filter (fun (_,srem) -> srem <> substr i) (p i)
  
  let (_:'a ty_parser -> 'a ty_parser) = noteps

  let peps = (fun i ->
    let (s,l,h) = i.sb1 in
    let s1 = (s,l,l) in
    [(s1,i.sb1)])
    
  (* FIXME following allows: item sep as a list *)
  let rec listof item sep = fun i -> 
    ((peps >> (fun _ -> []))
     ||| (item >> (fun x -> [x]))
     ||| ((item **> sep **> (listof item sep)) >> (fun (x,(_,xs)) -> x::xs))) i
  
  let rec star item = fun i -> 
    ((peps >> (fun _ -> []))
     ||| ((item **> (star item)) >> (fun (x,xs) -> x::xs))) i
  
  let rec itern item n = (match n with 
    | 0 -> (peps >> (fun _ -> []))
    | _ -> ((item **> (itern item (n-1))) >> (fun (x,xs) -> x::xs)) )
  
  let just a = (always >> (fun _ -> a))
  
  (* consume until another parser produces results - other parser MUST produce a result! *)  
  (* FIXME very inefficient *)
  let rec until p = fun i -> (
    let rs = p i in 
    let (s,l,h) = i.sb1 in
    if rs <> [] then [(s,l,l),(s,l,h)] else
      if len i.sb1 = 0 then [] else
        let rs = until p (lift (inc_low 1) i) in
        let f1 (s1,s2) = (dec_low 1 s1,s2) in
        List.map f1 rs)
  
  let (_:(ty_input1 -> 'a list) -> ty_input1 -> (substring * substring) list) = until
  
  (* FIXME following is tail recursive; not sure about correctness *)
  let until p = fun i -> (
    let (s,l,h) = i.sb1 in
    let rec f1 n = (
      if l+n <= h then (
        let rs = p {i with sb1=(s,l+n,h)} in 
        if rs <> [] then [(s,l,l+n),(s,l+n,h)] else f1 (n+1)
      ) else [])
    in 
    f1 0)
  
  (* consume until another parser produces results, or end of string *)  
  let rec until' p = fun i -> (
    let rs = p i in 
    let (s,l,h) = i.sb1 in
    if rs <> [] then [(s,l,l),(s,l,h)] else
      if len i.sb1 = 0 then [(s,l,h),(s,l,h)] else
        let rs = until' p (lift (inc_low 1) i) in
        let f1 (s1,s2) = (dec_low 1 s1,s2) in
        List.map f1 rs)
  
  (* not combinator *)
  (* FIXME what should be the correct semantics?
  let parse_not p = fun i -> (
    let (s,l,h) = i.sb1 in
    let rs = p i in
    if rs = [] then [(s,l,l),(s,l,h)] else [])
  *)
  
  (* and combinator; results are those from first parser; matched substrings have to match each parser *)
  let (&&&) p1 p2 = fun i -> (
    let rs1 = p1 i in 
    let rs2 = p2 i in
    let rs3 = List.map snd rs2 in
    let f1 = fun (r,s) -> mem s rs3 in
    let rs1 = List.filter f1 rs1 in
    rs1)
  
end

(*
## Context
*)

module Context = struct 

  open Prelude
  open Types
  open Substring
  let lc_substring_of = Common.lc_substring_of
  let ignr_last = Combinator.ignr_last
  let substr = Common.substr
  let lift = Common.lift

  (* debug version; assumes s1 = s2 (since the only part of the context that matters is...) *)
  let lc_cmp (nt1,(l1,h1)) (nt2,(l2,h2)) = 
    if (l1,h1) <> (l2,h2) then failwith "lc_cmp" else Pervasives.compare nt1 nt2

  (* when parsing the input between l and h, the only part of the
   context that matters are those entries (nt,(l',h')) st (l',h') =
   (l,h); so there is a notion of a normalized context (important
   for memoization) *)

  let normalize_context (LC(lc)) (l,h) = (
    LC(List.filter (fun (nt',(l',h')) -> (l',h') = (l,h)) lc))

  let update_context c (nt,(l,h)) = (
    let LC(lc) = normalize_context c (l,h) in
    LC(Prelude.myinsert lc_cmp (nt,(l,h)) lc))

  let context_contains (LC(lc)) (nt,(l,h)) = (
    List.exists ((=) (nt,(l,h))) lc)

  
  (* remember what NT is called on what input *)
  (* nonterm -> 'a ty_parser -> 'a ty_parser *)
  let update_lctxt nt p = (fun i -> 
    p { i with lc1=(update_context i.lc1 (nt,lc_substring_of i.sb1)) })

  let (_:nonterm -> 'a ty_parser -> 'a ty_parser) = update_lctxt
  
  (* nonterm -> 'a ty_parser -> 'a ty_parser *)
  let check_and_upd_lctxt nt p = fun i ->
    let should_trim = context_contains i.lc1 (nt,lc_substring_of i.sb1) in 
    if should_trim && (len i.sb1 = 0) then 
      []
    else if should_trim then
      (ignr_last (update_lctxt nt p)) i
    else 
      (update_lctxt nt p) i
  
  let (_:nonterm -> 'a ty_parser -> 'a ty_parser) = check_and_upd_lctxt
  
  
  (* simple memoization *)

  let generic_memo tbl key_of_input f i = (
    let k = key_of_input i in
    match k with 
    | None -> (f i)
    | Some k -> (
      if (MyHashtbl.mem tbl k) then (MyHashtbl.find tbl k) else
        let v = f i in
        let _ = MyHashtbl.add tbl k v in
        v))
  
  let key_of_input nt = (fun i -> 
    let i = { i with lc1=(normalize_context i.lc1 (lc_substring_of i.sb1)) } in
    let k = (nt,i.lc1,lc_substring_of i.sb1) in
    Some k)

  (* (key,('a * substring)list) MyHashtbl.t -> nonterm -> 'a ty_parser -> 'a ty_parser *)
  let memo_check_and_upd_lctxt tbl nt p i = (
    generic_memo tbl (key_of_input nt) (check_and_upd_lctxt nt p) i)
  
  let (_:(key,('a * substring)list) MyHashtbl.t -> nonterm -> 'a ty_parser -> 'a ty_parser) = 
    memo_check_and_upd_lctxt
  
  
  (* parameterization by ignr_last *)
  
  (* f1 argument takes a (nt,(i,j)) and return an int option k < j *)
  (* FIXME change name of following *)
  let sti_ignr_last f1 nt p = fun i -> 
    if len (substr i) = 0 then [] else (
      let k = f1 (nt,lc_substring_of i.sb1) in
      match k with | None -> [] | Some k -> 
        let delta = high i.sb1 - k in
        let inc_high (e,s) = (e,inc_high delta s) in 
        ((List.map inc_high) $ p $ (lift (dec_high delta))) i)
  let (_:(nonterm * (int * int) -> int option) -> nonterm -> 'a ty_parser -> 'a ty_parser) = sti_ignr_last
  
  (* FIXME change name of following *)
  let il_memo_check_and_upd_lctxt ignr_last = 
    let check_and_upd_lctxt nt p = fun i ->
      let should_trim = context_contains i.lc1 (nt,lc_substring_of i.sb1) in 
      if should_trim && (len i.sb1 = 0) then 
        []
      else if should_trim then
        (ignr_last nt (update_lctxt nt p)) i
      else 
        (update_lctxt nt p) i
    in
    (fun tbl nt p i ->
      generic_memo tbl (key_of_input nt) (check_and_upd_lctxt nt p) i)
  
end

;;

(*
## `grammar_to_parser` and caneps

This is the plain version that appears in the paper and the HOL4 formalization.

*)

module GrammarToParser = struct 

  open Prelude   
  open Types
  open Combinator
  open Context
  
  let rec grammar_to_parser p_of_tm g sym i = (match sym with 
    TM tm -> ((p_of_tm tm) >> (fun v -> LF(tm,v))) i | NT nt -> 
    let rules = List.filter (fun (nt',rhs) -> nt' = nt) g in
    let alts1 = List.map snd rules in
    let alts2 = List.map (List.map (grammar_to_parser p_of_tm g)) alts1 in
    let p = or_list (List.map (then_list2 nt) alts2) in
    check_and_upd_lctxt nt p i)
  
  let (_: (term -> substring ty_parser) -> grammar -> symbol -> parse_tree ty_parser) = grammar_to_parser
  
  let g2p_params p_of_tm = {
    p_of_tm3=(fun tm -> (p_of_tm tm) >> (fun v -> LF(tm,v)));
    then_list3=(fun nt -> then_list2 nt);
    check_and_upd_lctxt3=(fun nt -> check_and_upd_lctxt nt);
    unique3=(fun p -> p);
  }
  
  let rec g2p params g sym i = (match sym with 
    TM tm -> params.p_of_tm3 tm i | NT nt -> 
    let rules = List.filter (fun (nt',rhs) -> nt' = nt) g in
    let alts1 = List.map snd rules in
    let alts2 = List.map (List.map (g2p params g)) alts1 in
    let p = or_list (List.map (params.then_list3 nt) alts2) in
    let q = params.unique3 p in
    params.check_and_upd_lctxt3 nt q i)
  
  (* version via parameterization *)
  let grammar_to_parser p_of_tm = g2p (g2p_params p_of_tm)


  let toinput = Common.toinput
  let full = Substring.full

  let caneps p_of_tm g start_sym = 
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
    let p = memo_grammar_to_parser tbl p_of_tm g start_sym in
    let rs = p (toinput (full "")) in
    rs <> []

end

;;



(*
## Parse a grammar file
*)

module ParseGrammar = struct 

  (* FIXME code generation doesn't work correctly if we have a nt with no rules *)

  open Types
  open BasicParsers
  open Combinator
  let content = Substring.content
  
  let tm_of_lit quote lit = TM(quote^lit^quote)
  
  let parse_comm = fun i -> ((a "(*") **> until_a "*)" **> (a "*)")) i
  
  (* FIXME only one comment in ws? *)
  let parse_wscomm = 
    ((parse_ws >> (fun _ -> ""))
     ||| ((parse_ws **> parse_comm **> parse_ws) >> (fun _ -> "")))
  
  let rec parse_GRAMMAR = fun i -> 
    ((parse_RULES **> parse_wscomm **> parse_EOF) >> (fun (rs,(_,_)) -> rs)) i
  
  and parse_RULES = fun i -> 
    ((listof parse_RULE parse_wscomm) >> (fun xs -> List.concat xs)) i
  
  and parse_RULE = fun i ->
    ((parse_SYM **> parse_wscomm **> (a "->") **> parse_wscomm **> parse_SYMSLIST) 
      >> (fun (NT nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (nt,syms)) symss))) i
  
  and parse_SYMSLIST = fun i -> 
    (listof parse_SYMS (parse_wscomm **> (a "|") **> parse_wscomm)) i
  
  (* N.B. we do not allow empty lists here *)
  and parse_SYMS = fun i ->
    (noteps (listof parse_SYM parse_wscomm))  i
  
  and parse_SYM = fun i ->
    ((((a "\"") **> parse_notdquote **> (a "\"")) >> (fun (_,(s,_)) -> tm_of_lit "\"" (content s)))
    ||| (((a "'") **> parse_notsquote **> (a "'")) >> (fun (_,(s,_)) -> tm_of_lit "'" (content s)))
    ||| (parse_AZS >> (fun s -> NT (content s)))
    ||| (((a "?") **> parse_azAZs **> (a "?")) >> (fun (_,(s,_)) -> TM("?" ^ (content s) ^ "?"))))
      i
  
  let (_:ty_input1 -> (grammar * substring) list) = parse_GRAMMAR
  
  (* FIXME version with actions; we allow parsing multiple action blocks *)
  
  let rec parse_GRAMMAR_WITH_ACTIONS' = fun i -> 
    ((parse_HG **> parse_wscomm **> parse_EOF) >> (fun (h,_) -> h)) i
  
  and parse_HG = fun i -> 
    (parse_RULES >> (fun rs -> ("",rs))
    ||| ((parse_HEADER **> parse_wscomm **> parse_RULES) >> (fun (h,(_,rs)) -> (h,rs)))) i
  
  and parse_HEADER = fun i -> parse_CODE i
  
  and parse_RULES = fun i -> 
    ((listof parse_RULE parse_wscomm) >> (fun xs -> List.concat xs)) i
  
  and parse_RULE = fun i ->
    ((parse_SYM **> parse_wscomm **> (a "->") **> parse_wscomm **> parse_RHS) 
      >> (fun (NT nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (nt,syms)) symss))) i
  
  and parse_RHS = fun i ->
    (listof parse_SYMSACT (parse_wscomm **> (a "|") **> parse_wscomm)) i
  
  and parse_SYMSACT = fun i -> 
    ((parse_SYMS **> parse_wscomm **> parse_ACT) >> (fun (syms,(_,act)) -> (syms,act))) i
  
  and parse_ACT = fun i -> parse_CODES i
  
  and parse_CODES = fun i -> listof parse_CODE parse_wscomm i

  and parse_CODE = fun i -> 
    (((a "{{") **> until_a "}}" **> (a "}}")) >> (fun (_lt,(act,_gt)) -> (content act))) i
  
  let (_:ty_input1 -> ((string * (nonterm * (symbol list * string list)) list) * substring) list)
      = parse_GRAMMAR_WITH_ACTIONS'
  
  let drop_actions g = List.map (fun (nt,y) -> (nt,fst y)) g

  let parse_GRAMMAR_WITH_ACTIONS = fun i -> 
    let rs = parse_GRAMMAR_WITH_ACTIONS' i in
    let f2 (nt,(syms,acts)) = (nt,(syms,List.hd acts)) in
    let f1 (h,rules) = (h,List.map f2 rules) in
    let f3 (v,srem) = (f1 v,srem) in
    List.map f3 rs

  let (_:ty_input1 -> ((string * (nonterm * (symbol list * string)) list) * substring) list)
      = parse_GRAMMAR_WITH_ACTIONS

  let get_grammar fname = (
    let open Prelude in
    let open Common in
    let open Substring in
    let rs = (parse_GRAMMAR (toinput (full (read_file_as_string fname)))) in
    let _ = if List.length rs = 0 then (
      failwith ("Failed to parse grammar file: "^fname^""))
    in
    let _ = if List.length rs > 1 then (failwith ("Ambiguous grammar file: "^fname)) in
    let (g,_) = List.hd rs in
    g)


  (* following functions are for printing a grammar with altered actions *)
  let unparse_GRAMMAR_WITH_ACTIONS (h,g) = (
    let nts = Common.nts_of_grammar (drop_actions g) in
    let rhss nt = List.map snd (List.filter (fun (nt',_) -> nt' = nt) g) in
    let rhss = List.map (fun nt -> (nt,rhss nt)) nts in
    let f1 (nt,rhss) = (
      let string_of_symbol sym = (match sym with
        | NT nt -> nt
        | TM tm -> tm)
      in
      let string_of_rhs rhs = String.concat " " (List.map string_of_symbol rhs) in
      let f2 (rhs,s) = (string_of_rhs rhs)^" {{"^s^"}}" in
      nt^" -> "^(String.concat "\n| " (List.map f2 rhss))^"\n")
    in
    "{{"^h^"}}\n"
    ^(String.concat "\n" (List.map f1 rhss)))

  let pt_fun_of_rhs nt syms = (
    let rec upto a b = if a=b then [] else a::(upto (a+1) b) in
    let var n = "x"^(string_of_int n) in
    let ns = upto 0 (List.length syms) in
    let vars = List.map var ns in
    let rec pat1 vars = (match vars with 
      | [] -> (failwith "pt_fun_of_rhs")
      | [x] -> x
      | x::xs -> ("("^x^","^(pat1 xs)^")"))
    in
    let pat = pat1 vars in
    let vars = List.combine vars syms in
    let dquote="\"" in
    let vars = List.map (fun (v,sym) -> if is_TM sym then "LF("^dquote^(String.escaped(dest_TM sym))^dquote^","^v^")" else v) vars in
    let list_pat = "["^(String.concat ";" vars)^"]" in
    " fun "^pat^" -> NODE(\""^nt^"\","^list_pat^") ")

  let mk_pt_actions (h,g) = (
    let f1 (nt,(syms,s)) = (nt,(syms,pt_fun_of_rhs nt syms)) in
    let g = List.map f1 g in
    (h,g))

end

(*
## Command line helper functions
*)

module CommandLine = struct 

  let toinput = Common.toinput
  let full = Substring.full

  let get_args parse_CL argv = (
    let argv = List.tl (Array.to_list argv) in
  (*  let _ = print_endline ("Command line: "^(String.concat " " argv)) in *)
    let rs = (parse_CL (toinput (full (String.concat "\x00" argv)))) in
    match rs with 
      | [] -> failwith "get_args: failed to parse command line (0 parses)"
      | [(r,_)] -> r
      | _ -> failwith "get_args: failed to parse command line (>=2 parses)")

  open Substring
  open BasicParsers
  open Combinator
  
  (* parsers for command line parsing *)
  
  (* we use "\x00" as an arg separator - assumes this char does not appear on the cl *)
  let parse_FLAG = ((a "-") **> parse_while (fun s -> s <> "\x00")) >> (fun (_,s) -> "-"^(content s))
  
  (* first char should not be a - *)
  let parse_ARG = 
    let parse_not_minus = parse1 (fun c -> c <> "-") in
    (parse_not_minus **> parse_while (fun s -> s <> "\x00")) >> (fun (s1,s2) -> ((content s1)^(content s2)))
  
  let parse_FLARGS = (
    let sep = a "\x00" in
    (parse_FLAG >> fun f -> (f,[]))
    ||| ((parse_FLAG **> sep **> (listof parse_ARG sep)) >> (fun (f,(_,xs)) -> (f,xs))))

end


(*
## Codegen
*)

module Codegen = struct
  
  open Types
  
  let nts_of_grammar = Common.nts_of_grammar
  let drop_actions = ParseGrammar.drop_actions
  
  
  let str_of_SYM sym = match sym with 
    | NT nt -> "parse_"^nt
    | TM tm -> "(parser_of_raw_parser (term_to_parser \""^(String.escaped tm)^"\"))"
  
  let str_of_SYMS alt = "(" ^ (String.concat "**>" (List.map str_of_SYM alt)) ^ ")"
  
  let str_of_ACT act = act
  
  let str_of_SYMSACT (alt,act) = "("^ (str_of_SYMS alt) ^" >> ("^ (str_of_ACT act) ^"))"
  
  let str_of_RHS rhs = "("^ (String.concat "|||" (List.map str_of_SYMSACT rhs)) ^")"
  
  let str_of_RULE (nt,rhs) = (str_of_SYM (NT nt))^" = fun i -> (memo_check_and_upd_lctxt tbl_"^nt^" \""^nt^"\" (fun i -> unique ("^(str_of_RHS rhs)^" i)) i)"
  
  let str_of_RULES rs = "let rec "^(String.concat "\n\n and " (List.map str_of_RULE rs))
  
  let str_of_GRAMMAR g = (
    let nts = nts_of_grammar (drop_actions g) in
    let g' = List.map (fun nt -> (nt,List.map snd (List.filter (fun (nt',_) -> nt' = nt) g))) nts in
    str_of_RULES g')
    
  (* following just for writing out the grammar as a list or rules *)
  let ocaml_of_SYM sym = (match sym with
    | NT x -> ("(NT \""^(String.escaped x)^"\")")
    | TM x -> ("(TM \""^(String.escaped x)^"\")"))
  
  let ocaml_of_SYMS syms = ("["^(String.concat ";" (List.map ocaml_of_SYM syms))^"]")
  
  let ocaml_of_rule (nt,syms) = "(\""^(String.escaped nt)^"\","^(ocaml_of_SYMS syms)^")"
  
  let ocaml_of_grammar g0 = ("["^(String.concat ";\n" (List.map ocaml_of_rule g0))^"]")
  
end

;;

(*
## Grammar transformer - transform a grammar to output parse trees for debugging
*)

(*
# `earley.ml`
## Earley types

  * The `loop_2` type is the type of the state used when running the earley loop.
  * The `prod5` field is a set of `(nt_item,int)`. The `nt_item` is a blocked item; the int is the extent that the next symbol in the blocked item covered. For example, suppose we have the following member of `prod5`:
 
        ((Y -> alpha.Xbeta,i,j),k)

    This is supposed to indicate that the item in question was reachable, and that an item `(X -> ...,j,k)` was also reachable, and when these were combined, we got an item `(Y -> alphaX.beta,i,k)`


*)

module EarleyTypes = struct

  open Prelude  
  open Types
  
  (* FIXME move to core types? *)
  type ty_ctxt = {
    g5:grammar;
    sym5:nonterm; (* nonterm initial symbol *)
    (* caneps5:symbol -> bool; ( * may be dummy value, to be computed later FIXME don't need * ) *)
    string5:string;
    p_of_tm5:term -> substring -> lc_substring list
  }
  
  type nt_item = { nt2: nonterm; a2: symbol list; (* c2:(int*int) list;*) b2: symbol list; i2:int; j2:int }
  
  type earley_key = int * nonterm
  
  (*
  let ij_of itm = (itm.i2,itm.j2)
  *)
  
  (* FIXME EPSITM? *)
  (*
  let sym_of itm = (NT itm.nt2)
  *)
  
  (*
  type ty_input3 = { lc3:local_context }
  *)
  
  (* total ordering on items, for sets and maps which require such orderings *)
  let nt_compare x y =
    let n = Pervasives.compare x.nt2 y.nt2 in
    if n <> 0 then n else
      let n = Pervasives.compare x.i2 y.i2 in
      if n <> 0 then n else
        let n = Pervasives.compare x.j2 y.j2 in
        if n <> 0 then n else
          Pervasives.compare (x.a2,x.b2(*,x.c2*)) (y.a2,y.b2(*,y.c2*))
  
  module Set_nt_item = MySet_Make(
    struct
      type t = nt_item
      let compare x y = nt_compare x y
    end)
  
  module Set_nt_item_int = MySet_Make(
    struct
      type t = nt_item * int
      let compare (x,i) (y,j) = (
        let n = Pervasives.compare i j in
        if n <> 0 then n else
          nt_compare x y)
    end)
  
  module type MYMAP = sig
    type key
    type value
    type ty_map
    val empty : ty_map
    val add : key -> value -> ty_map -> ty_map
    val find2 : key -> ty_map -> value
    val bindings : ty_map -> (key * value) list
  end
  
  (* argument to Map functor *)
  module type MAPINPUT = sig
      type key
      type value
      val compare : key -> key -> int
      val default: value
  end
  
  module MyMap = functor (MapInput:MAPINPUT) -> (struct
    module Ord = struct
        type t = MapInput.key
        let compare = MapInput.compare
    end
    include Map.Make(Ord)
    type value=MapInput.value
    type ty_map=MapInput.value t
    let find2 k m =
      if (mem k m) then (find k m) else MapInput.default
  end : (MYMAP with type key = MapInput.key and type value = MapInput.value))
  
  module Map_earley_key = MyMap(
    struct
      type key = earley_key
      type value = Set_nt_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)
  
  (* this is the type used by the earley routines - typically external routines should invoke via Earley.earley_main, so they see only Set_nt_item.t rather than this type *)

  type ty_loop2 = {
    done5: Set_nt_item.t;
    todo5: Set_nt_item.t;
    prod5: Set_nt_item_int.t;
    blocked5: Map_earley_key.ty_map;
    complete5: Map_earley_key.ty_map
  }

  
end

(*
## Earley core

See EarleyTypes for an explanation of the `prod` field.

*)

module EarleyCore = struct
  
  open Types
  open EarleyTypes

  type ty_loop1 = {
    todo7: nt_item list; 
    blocked7: bool;
    complete7: bool;
    prod7: (nt_item * int) list
  }
    
  let key_of_blocked itm = (itm.j2,dest_NT (List.hd itm.b2))
  let key_of_complete itm = (itm.i2,itm.nt2)
  
  let rules_for_nt ctxt nt = (
    let rs = List.filter (fun (nt',rhs) -> nt' = nt) ctxt.g5 in
    rs)         
  
  let shift_a2_b2_c2 itm = {itm with
    a2=(List.hd itm.b2)::itm.a2;
    b2=(List.tl itm.b2)
  }
    
  (* blocked and complete can only be itm0 *)
  let loop1 ctxt itm0 = (
    let s1 = { todo7=[]; blocked7=false; complete7=false; prod7=[] } in
    match itm0.b2 with 
    | [] -> {s1 with complete7=true} 
    | sym::syms -> (
      match sym with 
      | NT nt -> ( 
        let rs = rules_for_nt ctxt nt in
        let alts = List.map snd rs in
        let f1 alt = {nt2=nt; a2=[]; b2=alt; i2=itm0.j2; j2=itm0.j2} in
        {s1 with blocked7=true; todo7=(List.map f1 alts) })
      | TM tm -> (
        let p = ctxt.p_of_tm5 tm in
        let rs = p (ctxt.string5,itm0.j2,String.length ctxt.string5) in (* assume String.length fast/cached *)
        let f2 (i,j) = { (shift_a2_b2_c2 itm0) with j2=j } in
        let f3 (i,j) = (itm0,j) in 
        {s1 with todo7=(List.map f2 rs); prod7=(List.map f3 rs) })))
  
  let loop2 ctxt s0 = (
    let itm0 = Set_nt_item.choose s0.todo5 in
    let r = loop1 ctxt itm0 in
    let s0 = {s0 with done5=(Set_nt_item.add itm0 s0.done5); todo5=(Set_nt_item.remove itm0 s0.todo5) } in
    let s0 = (match r.complete7 with | false -> s0 | true -> 
      let k = key_of_complete itm0 in
      let complete_items = Map_earley_key.find2 k s0.complete5 in
      let blocked_items = Map_earley_key.find2 k s0.blocked5 in
      let f1 bitm = {(shift_a2_b2_c2 bitm) with j2=itm0.j2} in
      let todo_items = Set_nt_item.map f1 blocked_items in
      let todo_items = Set_nt_item.diff todo_items s0.done5 in
      {s0 with 
        todo5=(Set_nt_item.union s0.todo5 todo_items);
        complete5=(Map_earley_key.add k (Set_nt_item.add itm0 complete_items) s0.complete5);
        prod5=(
          let blocked_items = Set_nt_item.elements blocked_items in
          let f1 b = (b,itm0.j2) in
          Set_nt_item_int.list_union (List.map f1 blocked_items) s0.prod5)
      })
    in
    let s0 = (match r.blocked7 with | false -> s0 | true -> 
      let k = key_of_blocked itm0 in
      let blocked_items = Map_earley_key.find2 k s0.blocked5 in
      let complete_items = Map_earley_key.find2 k s0.complete5 in
      let f1 citm = {(shift_a2_b2_c2 itm0) with j2=citm.j2} in
      let todo_items = Set_nt_item.map f1 complete_items in
      let todo_items = Set_nt_item.diff todo_items s0.done5 in
      {s0 with 
        todo5=(Set_nt_item.union s0.todo5 todo_items);
        blocked5=(Map_earley_key.add k (Set_nt_item.add itm0 blocked_items) s0.blocked5);
        prod5=(
          let complete_items = Set_nt_item.elements complete_items in
          let f1 c = (itm0,c.j2) in
          Set_nt_item_int.list_union (List.map f1 complete_items) s0.prod5)
      })
    in
    (* handle prod7 *)
    let s0 = { s0 with prod5=(Set_nt_item_int.list_union r.prod7 s0.prod5)} in
    let todo_items = Set_nt_item.from_list r.todo7 in 
    let todo_items = Set_nt_item.diff todo_items s0.done5 in
    let s0 = {s0 with todo5=(Set_nt_item.union todo_items s0.todo5)} in
    s0)
  
  let rec earley ctxt s0 = (if Set_nt_item.is_empty s0.todo5 then s0 else (earley ctxt (loop2 ctxt s0)))

  let earley_initial_state ctxt0 = ({
      done5=Set_nt_item.empty;
      todo5=(
        let initial_itm = ({nt2=ctxt0.sym5;a2=[];b2=[NT ctxt0.sym5];i2=0;j2=0}) in
        Set_nt_item.add initial_itm Set_nt_item.empty);
      prod5=Set_nt_item_int.empty;
      blocked5=Map_earley_key.empty;
      complete5=Map_earley_key.empty
    })

end

(*
## Earley

Wrap up Earley routines to be used by others

*)

module Earley = struct

  open Prelude
  open Types
  open EarleyTypes
  open Substring
  open Common
  open EarleyCore

  (* wrap the usual p_of_tm; FIXME perhaps we want to change the p_of_tm type? *)
  let earley_term_to_parser p_of_tm tm s =
    let i = s in
    let f1 ((s,i,j),s_rem) = (i,j) in
    let rs = p_of_tm tm i in
    List.map f1 rs

  let last_earley_state = ref None
  
  let earley_full setup0 = (
    let ctxt0 = {
      g5=setup0.g7;
      sym5=setup0.sym7;
      string5=setup0.string7;
      p_of_tm5=(earley_term_to_parser setup0.p_of_tm7);
    } 
    in
    let s0 = earley_initial_state ctxt0 in
    let s1 = earley ctxt0 s0 in
    let _ = debug_endline "earley_main: 1" in
    let _ = (last_earley_state := Some s1) in
    s1)

  let earley_main setup0 = (earley_full setup0).done5
  
  let (_:ty_setup -> Set_nt_item.t) = earley_main
  
end


(*
# `p3.ml`
## P3: third version of parser combinators

  * The key type here is `Map_symbol_int_int`, which is a map from `(sym2,i,k)` to a set (list) of integers `j`. It is used in the definition of the `*>` combinator. The idea is that we have two parsers, `p1` and `p2`. Parser `p2` corresponds to symbol `sym2`. When parsing `p1 *> p2` for the extent `(i,k)` we need to know `j` such that we can parse `p1` on the extent `(i,j)` and `p2` on the extent `(j,k)`. To achieve this, we look up `(sym2,i,k)` in the map, to give the possible `j`s.

  * To form the map, we take the productions `((Y -> alpha.Xbeta,i,j),k)` from the earley stage. Then the relevant symbol is `X`, and `i,j,k` are as described above. FIXME FIXME except that we don't really know i because we discard alpha.

  * *In addition* to the above, for a terminal `TM tm`, and an extent `(j,k)` we need to know if we can parse that terminal for that extent. This is the case if we have an `((X->alpha.(TM tm)beta,i,j),k)`. Note that in this case, the input is `(j,k)`, whereas in the previous use of `Map_symbol_int_int`, the input was `(i,k)`.


*)

(* we want to represent a parser as a triple of functions, one which
   gives the nt, one which gives the rhs, and the final that actually
   does the parsing and applying the actions *)

(* we want to define the combinators on each index, then project out *)

module P3 = struct

  open Prelude
  open Types
  open EarleyTypes
  let allpairs = Prelude.allpairs
  let insert = Prelude.insert
  let parser_of_raw_parser = Common.parser_of_raw_parser

  module Set_int = MySet_Make(
    struct
      type t = int
      let compare x y = Pervasives.compare x y
    end)  
  
  module Map_symbol_int_int = MyMap(
    struct
      type key = symbol * int * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

  module Map_term_int_int = MyMap(
    struct
      type key = term * int * int
      type value = bool
      let compare x y = Pervasives.compare x y
      let default = false
    end)
    
  type ty_in4 = { 
    s4:string;
    i4:int; 
    j4:int; 
    lc4: local_context; 
    pprods4:Map_symbol_int_int.ty_map;
    tmpprods4:Map_term_int_int.ty_map
  }

  let lc4_substring_of i = (i.i4,i.j4)

  type ty_rhs = Atom of symbol | Seq of symbol * symbol | Alt of symbol * symbol

  type ty_exp = nonterm * ty_rhs

  type ty_rules = ty_exp list
 
  type ty_rules_tmparsers = { rules7: ty_rules; tmparsers7: ((term * raw_parser) list) } (* FIXME really finite map *)

  type 'a myfun = { sym9:unit->symbol; rhs9:ty_rules_tmparsers->ty_rules_tmparsers; p9:ty_in4->'a }

  type inthree = One | Two of ty_rules_tmparsers | Three of ty_in4

  type 'a outthree = OutOne of symbol | OutTwo of ty_rules_tmparsers | OutThree of 'a

  type 'a myfun2 = inthree -> 'a outthree

  type 'a parser123 = 'a list myfun2

  let dest_OutOne i = (match i with OutOne x -> x | _ -> failwith "dest_OutOne")

  let dest_OutTwo i = (match i with OutTwo x -> x | _ -> failwith "dest_OutTwo")

  let dest_OutThree i = (match i with OutThree x -> x | _ -> failwith "dest_OutThree")

  let wrap3 f = (fun i -> match i with
    | One -> (OutOne(f.sym9 ()))
    | Two c -> (OutTwo(f.rhs9 c))
    | Three i -> (OutThree(f.p9 i)))

  let (_:'a myfun -> 'a myfun2) = wrap3

  let unwrap3 f = {
    sym9=(fun () -> dest_OutOne (f One));
    rhs9=(fun c -> dest_OutTwo (f (Two c)));
    p9=(fun _ -> failwith "unwrap3")
  }

  let (_:'a myfun2 -> 'a myfun) = unwrap3

  let sym_of_parser p = ((dest_OutOne (p One)))

  let grammar_of_parser p = (
    let f1 x = (match x with 
       | Atom (x) -> [[x]]
       | Seq (x,y) -> [[x;y]]
       | Alt (x,y) -> [[x];[y]])
    in
    let f2 (nt,x) = List.map (fun x -> (nt,x)) (f1 x) in
    let {rules7=rs;tmparsers7=tms} = dest_OutTwo (p(Two{rules7=[];tmparsers7=[]})) in
    {g8=(List.concat (List.map f2 rs));raw_parsers8=tms})

  (* the symbol corresponding to a seq, alt etc *)

  let seq1 p1 p2 () = (
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let (sym1,sym2) = (p1.sym9 (), p2.sym9 ()) in
    (NT ("("^(string_of_symbol sym1)^"***>"^(string_of_symbol sym2)^")")))

  let (_:'a myfun2 -> 'b myfun2 -> unit -> symbol) = seq1

  let alt1 p1 p2 () = (
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let (sym1,sym2) = (p1.sym9 (), p2.sym9 ()) in
    (NT ("("^(string_of_symbol sym1)^"||||"^(string_of_symbol sym2)^")")))

  let (_:'a myfun2 -> 'b myfun2 -> unit -> symbol) = alt1

  let seq2 p1 p2 c0 = (
    let sym = seq1 p1 p2 () in
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let {rules7=c;tmparsers7=tms} = c0 in
    (* we may already have done this *)
    if (List.mem (dest_NT sym) (List.map fst c)) then c0 else (
      (* make a new rule and add it to the grammar *)
      let c1 = {c0 with rules7=(dest_NT sym,Seq(p1.sym9 (),p2.sym9 ()))::c0.rules7} in
      let c2 = p1.rhs9 c1 in 
      let c3 = p2.rhs9 c2 in
      c3))

  let (_:'a myfun2 -> 'b myfun2 -> ty_rules_tmparsers -> ty_rules_tmparsers) = seq2

  let alt2 p1 p2 c0 = (
    let sym = alt1 p1 p2 () in
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let {rules7=c;tmparsers7=tms} = c0 in
    (* we may already have done this *)
    if (List.mem (dest_NT sym) (List.map fst c)) then c0 else (
      (* make a new rule and add it to the grammar *)
      let c1 = {c0 with rules7=(dest_NT sym,Alt(p1.sym9 (),p2.sym9 ()))::c0.rules7} in
      let c2 = p1.rhs9 c1 in 
      let c3 = p2.rhs9 c2 in
      c3))

  (* associates to the right *)
  let ( ***> ) p1 p2 = (fun i -> 
    wrap3 {
      sym9=(seq1 p1 p2);
      rhs9=(seq2 p1 p2);
      p9=(fun i -> 
        let sym2 = dest_OutOne(p2 One) in
        let key = (sym2,i.i4,i.j4) in
        let is = Set_int.elements (Map_symbol_int_int.find2 key i.pprods4) in (* FIXME inefficient *)
        let f1 j = (
          let rs1 = dest_OutThree (p1 (Three { i with j4=j })) in
          (* optimization, can avoid non-termination in some cases, without employing context eg star operator *)
          if rs1 = [] then [] else
          let rs2 = dest_OutThree (p2 (Three { i with i4=j })) in
          allpairs (fun x -> fun y -> (x,y)) rs1 rs2)
        in
        (List.concat (List.map f1 is)))
    } i)

  let ( |||| ) p1 p2 = (fun i -> 
    wrap3 {
      sym9=(alt1 p1 p2);
      rhs9=(alt2 p1 p2);
      p9=(fun i -> 
        let rs1 = dest_OutThree (p1 (Three i)) in
        let rs2 = dest_OutThree (p2 (Three i)) in
        List.append rs1 rs2)
    } i)

  (* want a combinator p1 orelse p2 that applies p1, and if no results, applies p2 *)
  let altstar1 p1 p2 () = (
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let (sym1,sym2) = (p1.sym9 (), p2.sym9 ()) in
    (NT ("("^(string_of_symbol sym1)^"||||*"^(string_of_symbol sym2)^")")))

  (* FIXME in the following, OK to use Alt - we need that earley stage is correct for tms, but over approx for nts *)
  let altstar2 p1 p2 c0 = (
    let sym = altstar1 p1 p2 () in
    let p1 = unwrap3 p1 in
    let p2 = unwrap3 p2 in
    let {rules7=c;tmparsers7=tms} = c0 in
    (* we may already have done this *)
    if (List.mem (dest_NT sym) (List.map fst c)) then c0 else (
      (* make a new rule and add it to the grammar *)
      let c1 = {c0 with rules7=(dest_NT sym,Alt(p1.sym9 (),p2.sym9 ()))::c0.rules7} in
      let c2 = p1.rhs9 c1 in 
      let c3 = p2.rhs9 c2 in
      c3))

  let ( ||||* ) p1 p2 = (fun i -> 
    wrap3 {
      sym9=(altstar1 p1 p2);
      rhs9=(altstar2 p1 p2);
      p9=(fun i -> 
        let rs1 = dest_OutThree (p1 (Three i)) in
        if rs1 <> [] then rs1 else
          let rs2 = dest_OutThree (p2 (Three i)) in
          List.append rs1 rs2)
    } i)


  let ( >>>> ) p f = (fun i -> match i with
    | Three _ -> (
      let rs = dest_OutThree (p i) in
      OutThree(List.map f rs))
    (* additional clauses required to get general type for this combinator *)
    | One -> (OutOne (dest_OutOne (p One)))
    | Two x -> (OutTwo (dest_OutTwo (p (Two x)))))

  (* the next two functions deal with updating the context *)

  let update_lc4 nt p = (fun i ->
    match i with
    | Three i -> (
      p (Three { i with lc4=(Context.update_context i.lc4 (nt,lc4_substring_of i)) }))
    | _ -> p i)
  
  (* FIXME these combinators could be made more efficient *)
 
  let check_and_upd_lc4 p = (fun i -> 
    match i with 
    | Three i -> (
      let sym = dest_OutOne (p One) in
      let nt = dest_NT sym in 
      let should_trim = Context.context_contains i.lc4 (nt,lc4_substring_of i) in
      if should_trim then (OutThree []) else ((update_lc4 nt p (Three i))))
    | _ -> p i)

  let mkntparser' nonterm f = (fun i -> match i with
    | One -> (OutOne (NT nonterm))
    | Two rs -> (
      let sym_f = dest_OutOne (f One) in
      let {rules7=rs';tmparsers7=tms} = dest_OutTwo (f i) in
      (* FIXME what if sym is a TM *)
      OutTwo({rules7=(insert (nonterm,Atom sym_f) rs');tmparsers7=tms}))
    | Three _ -> (
      (* FIXME we have adjusted results to return only unique items *)
      let rs = dest_OutThree (f i) in
      OutThree (Prelude.unique rs)))

  (* now, mkntparser includes a context check *)
  let mkntparser nonterm f = (check_and_upd_lc4 (mkntparser' nonterm f))

  let mktmparser term (p:raw_parser) = (fun i -> match i with
    | One -> (OutOne (TM term))
    | Two rs0 -> (
      let {rules7=rs; tmparsers7=tms} = rs0 in
      if List.mem term (List.map fst tms) then (OutTwo rs0) else
        OutTwo {rs0 with tmparsers7=(term,p)::tms})      
    | Three i -> (
      let key = (term,i.i4,i.j4) in
      let rs = Map_term_int_int.find2 key i.tmpprods4 in
      if rs then (OutThree ([(i.s4,i.i4,i.j4)])) else (OutThree ([]))))




  (* take a parser, extract the grammar, run earley, and produce a set of (nt,int) *)
  let earley_prods p txt = (
    (* FIXME expects NT parser *)
    let nt0 = dest_NT (sym_of_parser p) in
    let {g8=g0;raw_parsers8=tms} = grammar_of_parser p in
    let p_of_tm term = (* parser_of_raw_parser *) (List.assoc term tms) in (* FIXME List.assoc inefficient *)
    let setup = {
      g7=g0;
      sym7=nt0;
      p_of_tm7=p_of_tm;
      string7=txt;
    }
    in
    (Earley.earley_full setup).prod5)
  let (_:'a parser123 -> string -> Set_nt_item_int.t) = earley_prods

 
  (* take the result from earley_prods, and process to give two maps, which are then used when applying the actions in earley_actions *)
  
  let process_prods prods = (
    let prods = Set_nt_item_int.elements prods in
    let f1 m prod = (
      let (itm,k) = prod in
      let key = (List.hd itm.b2,itm.i2,k) in
      let v = itm.j2 in
      let s = Map_symbol_int_int.find2 key m in
      let s' = Set_int.add v s in
      let m'  = Map_symbol_int_int.add key s' m in
      m')
    in
    let pprods = List.fold_left f1 Map_symbol_int_int.empty prods in
    let f1 m prod = (
      let (itm,k) = prod in
      match List.hd itm.b2 with
      | NT _ -> m
      | TM tm -> (
        let key = (tm,itm.j2,k) in
        let v = true in
        let m'  = Map_term_int_int.add key v m in
        m'))
    in
    let tmpprods = List.fold_left f1 Map_term_int_int.empty prods in
    (pprods,tmpprods))
  
  (* single pass over prods 
  let process_prods prods = (
    let prods = Set_nt_item_int.elements prods in
    let f1 m prod = (
      let (itm,k) = prod in
      let key = (List.hd itm.b2,itm.i2,k) in
      let v = itm.j2 in
      let s = Map_symbol_int_int.find2 key m in
      let s' = Set_int.add v s in
      let m'  = Map_symbol_int_int.add key s' m in
      m')
    in
    let f2 m prod = (
      let (itm,k) = prod in
      match List.hd itm.b2 with
      | NT _ -> m
      | TM tm -> (
        let key = (tm,itm.j2,k) in
        let v = true in
        let m'  = Map_term_int_int.add key v m in
        m'))
    in
    let f3 (m1,m2) prod = (
      let m1' = f1 m1 prod in
      let m2' = f2 m2 prod in
      (m1',m2'))
    in
    let (pprods,tmpprods) = List.fold_left f3 (Map_symbol_int_int.empty,Map_term_int_int.empty) prods in
    (pprods,tmpprods))
  *)
  let (_:Set_nt_item_int.t -> Map_symbol_int_int.ty_map * Map_term_int_int.ty_map) = process_prods

  (* perform earley actions on a given extent *)
  let earley_actions p (pprods,tmpprods) txt (i,j) = (dest_OutThree (p (Three ({ s4=txt; i4=i; j4=j; lc4=empty_context; pprods4=pprods; tmpprods4=tmpprods }))))

  let time f x =
    let open Prelude in
    if not (!report_timing) then f x else
    let start_time = Sys.time() in
    try let result = f x in
        let finish_time = Sys.time() in
        report("CPU time (user): "^(string_of_float(finish_time -. start_time)));
        result
    with e ->
        let finish_time = Sys.time() in
        Format.print_string("Failed after (user) CPU time of "^
                            (string_of_float(finish_time -. start_time))^": ");
        raise e;;

  (* top-level routine - apply a parser to a string, get results *)
  let p3_run_parser p txt = (
    let _ = Prelude.report_timing := false in
    let prods = time (earley_prods p) txt in
    let (pprods,tmpprods) = time process_prods prods in
    time (earley_actions p (pprods,tmpprods) txt) (0,String.length txt))
  let (_:'a parser123 -> string -> 'a list) = p3_run_parser
  
end

(*
## P3_memo: various ways to memoize the parser combinators


*)


module P3_memo = struct

  open Prelude
  open Types
  open EarleyTypes
  open P3

  let generic_memo tbl key_of_input f i = (
    let k = key_of_input i in
    match k with 
    | None -> (f i)
    | Some k -> (
      if (MyHashtbl.mem tbl k) then (MyHashtbl.find tbl k) else
        let v = f i in
        let _ = MyHashtbl.add tbl k v in
        v))

  (* FIXME do we already know that the context is normalized? No. It is not necessarily normalized relative to i.i4,i.j4 *)
  let key_of_input nt = (fun i -> match i with 
    | Three i -> (
      let i = { i with lc4=(Context.normalize_context i.lc4 (lc4_substring_of i)) } in
      let k = (nt,i.lc4,lc4_substring_of i) in
      Some k)
    | _ -> None)

  let memo_p3 tbl p i = (
    let nt = dest_NT (sym_of_parser p) in
    generic_memo tbl (key_of_input nt) p i)

  (* don't need anymore - all nt parsers check context anyway 
  let memo_check_and_upd_lc4 tbl p i = (
    let nt = dest_NT (dest_OutOne (p One)) in
    generic_memo tbl (key_of_input nt) (check_and_upd_lc4 p) i)
  *)

  (* version that creates table implicitly *)
  (*
  let memo_check_and_upd_lc4' p = (
    let tbl = MyHashtbl.create 100 in
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
    let tbl = MyHashtbl.create 100 in
    let rec g = (fun i -> 
      let nt = dest_NT (sym_of_parser (p g)) in
      generic_memo tbl (key_of_input nt) (p g) i)
    in
    g)

  (* some additional functions used by the code generator - slight abbreviations of the above *)
  let mcu4 tbl nt p = memo_p3 tbl (mkntparser nt p)

  (* FIXME not sure about the use of unique everywhere *)
  let unique4 i = (match i with 
    | OutThree rs -> (OutThree (Prelude.unique rs))
    | _ -> i)

end


(*
## P3 Basic parsers

P3 basic parsers (parsers for terminals) need names. We could just use gensym to generate the names, but this makes debugging somewhat harder (because the names of terminal parsers are meaningless). To aid debugging, we here provide basic parsers with somewhat meaningful names.

N.B. clearly two non-identical parsers need different names.

FIXME probably we should use the named versions as the FunctorBasicParsers, and just discard the names if we don't need them.

*)

module P3BasicParsers = struct

  open Prelude  
  open Types
  open Substring
  open RawParsers
  open EarleyTypes
  type 'a parser123 = 'a P3.parser123
  let mktmparser = P3.mktmparser

  let wrap name p = mktmparser name p

  let (_:term -> raw_parser -> substring parser123) = wrap

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
  
end

(*
## Everything

Wrapping everything up into a single top-level module

*)

module Everything = struct

  include Prelude  
  include Types
  include Substring
  include Common
(*  include RawParsers - lots of names that clash with eg BasicParsers and P3BasicParsers *)
  include Combinator
(*  include BasicParsers *)
  include Context
  include GrammarToParser
  include ParseGrammar
  include Codegen
  include CommandLine
  include EarleyTypes
  include EarleyCore 
  include Earley
  include P3
  include P3_memo
(*  include P3BasicParsers *)

end

;;
