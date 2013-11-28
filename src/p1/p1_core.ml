(*
# Interactive top-level directives

In bash, before running toplevel, or executing native code, the following may improve performance (but is not necessary):

    # from http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual024.html
    export OCAMLRUNPARAM="s=8M,l=8M,i=1M"

Then start the ocaml toplevel interpreter:

    ocaml

And type the following to load the `p3_lib.ml` file:

    #cd "/tmp/l/general/research/parsing/src/p3";; (* or wherever the p3_lib.ml file is located *)
    #use "p3_lib.toplevel.ml";;

You should then be able to e.g. execute the code inside the `P3Examples` module.
*)

(**
p1_core.ml
*)

(**

Combinator parsers following CPP'11.

*)

(**
{2 Prelude}
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
    let global_reset () = (ignore (List.map (fun f -> f ()) (!reset_funs)); ())
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
    | (h::t) -> Pervasives.compare x h = 0 || mem x t;;

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

(**
{2 Types}
*)

module Types = struct

  type term = string

  type nonterm = string

  let string_of_tm tm = tm

  let string_of_nt nt = nt

  let eps = "\"\""

  type substring = string * int * int

  type symbol = [ `NT of nonterm | `TM of term ]

  let is_NT s = (match s with `NT _ -> true | _ -> false)

  let dest_NT sym = (match sym with `NT x -> x | _ -> failwith "dest_NT")

  let is_TM sym = (match sym with `TM _ -> true | _ -> false)

  let dest_TM sym = (match sym with `NT _ -> failwith "dest_TM" | `TM tm -> tm)

  let string_of_symbol sym = match sym with | `NT nt -> "NT("^(string_of_nt nt)^")" | `TM tm -> "TM("^(string_of_tm tm)^")"

  (* NB this used to be (symbol list) list, but this was a bit stupid *)
  type rhs = symbol list

  type parse_rule = nonterm * rhs

  type grammar = parse_rule list

  type parse_tree = NODE of nonterm * parse_tree list | LF of term * substring

  (* FIXME following just for debugging - improve output *)
  let content (s,l,h) = String.sub s l (h-l)
  let rec string_of_pt pt = (match pt with
    | LF(x,s) -> ("LF("^x^",\""^(String.escaped (content s))^"\")")
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

(**
{2 Substrings}
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

(**
{2 Common functions}
*)

(* FIXME these are all simple conversions between types - probably belong in types *)
module Common = struct

  open Prelude
  open Types

  let string_of_substring (s,l,h) = "("^s^","^(string_of_int l)^","^(string_of_int h)^")"

  let lc_substring_of (s,l,h) = (l,h)

  let eps = `TM(eps) (* fix one particular terminal for eps *)

  let toinput s = { lc1=empty_context; sb1=s }

  let (_:substring -> ty_input1) = toinput

  let substr i = i.sb1

  let (_:ty_input1 -> substring) = substr

  let lift f i = { i with sb1=(f i.sb1) }

  let (_: (substring -> substring) -> (ty_input1 -> ty_input1)) = lift

  let syms_of_rhs rhs = rhs

  let syms_of_parse_rule (nt,rhs) = insert (`NT(nt)) (syms_of_rhs rhs)

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

(**
{2 Basic parsers}

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

  include P1_terminal_parsers.FunctorBasicParsers(Input)

end

(**
{2 Combinators}

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

(**
{2 Context}
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

(**
{2 `grammar_to_parser` and caneps}

This is the plain version that appears in the paper and the HOL4 formalization.

*)

module GrammarToParser = struct

  open Prelude
  open Types
  open Combinator
  open Context

  let rec grammar_to_parser p_of_tm g sym i = (match sym with
    `TM tm -> ((p_of_tm tm) >> (fun v -> LF(tm,v))) i | `NT nt ->
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
    `TM tm -> params.p_of_tm3 tm i | `NT nt ->
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



(**
{2 Parse a grammar file}
*)

module ParseGrammar = struct

  (* FIXME code generation doesn't work correctly if we have a nt with no rules *)

  open Types
  open BasicParsers
  open Combinator
  let content = Substring.content

  let tm_of_lit quote lit = `TM(quote^lit^quote)

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
      >> (fun (nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (dest_NT nt,syms)) symss))) i

  and parse_SYMSLIST = fun i ->
    (listof parse_SYMS (parse_wscomm **> (a "|") **> parse_wscomm)) i

  (* N.B. we do not allow empty lists here *)
  and parse_SYMS = fun i ->
    (noteps (listof parse_SYM parse_wscomm))  i

  and parse_SYM = fun i ->
    ((((a "\"") **> parse_notdquote **> (a "\"")) >> (fun (_,(s,_)) -> tm_of_lit "\"" (content s)))
    ||| (((a "'") **> parse_notsquote **> (a "'")) >> (fun (_,(s,_)) -> tm_of_lit "'" (content s)))
    ||| (parse_AZS >> (fun s -> `NT (content s)))
    ||| (((a "?") **> parse_azAZs **> (a "?")) >> (fun (_,(s,_)) -> `TM("?" ^ (content s) ^ "?"))))
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
      >> (fun (nt,(_,(_,(_,symss)))) -> (List.map (fun syms -> (dest_NT nt,syms)) symss))) i

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
        | `NT nt -> nt
        | `TM tm -> tm)
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

(**
{2 Command line helper functions}
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



;;
