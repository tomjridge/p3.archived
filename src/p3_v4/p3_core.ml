(**
{1 minip3_e2.ml}

{2 Prelude}
*)

(*
Interactive use:

    #directory "../earley3_v3";;
    #mod_use "earley3.ml";;

*)

module Box = struct

  type 'a box = [ `Box of int * 'a ] (* FIXME change `Box to Box *)

  let counter = ref 0

  let box x = (
    let c = !counter in
    let _ = counter := c+1 in
    `Box(c,x))

  let unbox x = (match x with
    | `Box(c,x) -> x)

  let box_get_key x = (match x with
    | `Box(c,x) -> c)

end

open Box

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b)

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []

let list_product l1 l2 = allpairs (fun x -> fun y -> (x,y)) l1 l2

let rec myinsert cmp elt lst = match lst with
  [] -> [elt]
| head :: tail -> let r = cmp elt head in if r < 0  then elt :: lst else (
  if r = 0 then failwith "myinsert" else head :: myinsert cmp elt tail)

let unique_f res e = if List.mem e res then res else e::res

(* this is insertion sort; alternatives? *)
let unique = fun e -> List.fold_left unique_f [] e

(* upto' i j = [i+1..j-1] *)
let rec upto' i j = (if i+1<j then (i+1)::(upto' (i+1) j) else [])

(* upto i j = [i..j] *)
let rec upto i j = (if i<=j then i::(upto (i+1) j) else [])


(* version of map that avoids some stack overflows with List.map *)
(*
let list_array_map f l =
  Array.to_list (Array.map f (Array.of_list l))
*)
let rev_map f l = (List.fold_left (fun acc -> fun x -> (f x)::acc) [] l)


(**
{2 Types}
*)

type ('dom,'cod) fmap = ('dom * 'cod) list

let fmap_update m (a,b) = (a,b)::m
let fmap_lookup m a = List.assoc a m
let fmap_dom m = List.map fst m
let empty_fmap = []

(* invariant: SS(s,i,j): i<=j<=(String.length s) *)
type 'a ty_substring = SS of 'a * int * int
type ty_substring' = string ty_substring

let dest_SS (SS(s,i,j)) = (s,i,j)

let content (SS(s,i,j)) = String.sub s i (j-i)

let concatenate_two (SS(s1,i1,j1)) (SS(s2,i2,j2)) = (
  if (s1=s2) && (j1=i2) then
    Some (SS(s1,i1,j2))
  else
    None)

let rec concatenate_list ss = (match ss with
|  [] -> None
| s1::ss -> (match ss with
  | [] -> Some s1
  | _ -> (match concatenate_list ss with
      None -> None
  |   Some s2 -> concatenate_two s1 s2)))

type term = string

type nonterm = string

type symbol = [ `NT of nonterm | `TM of term ]

let dest_NT sym = (match sym with `NT x -> x | _ -> failwith "dest_NT")

let string_of_tm tm = tm
let string_of_nt nt = nt
(*
let string_of_symbol sym = (match sym with 
  | NT nt -> "NT("^(string_of_nt nt)^")" 
  | `TM tm -> "`TM("^(string_of_tm tm)^")")
*)

(* we assume nonterminals and terminals correspond to disjoint subsets of string *)
let string_of_symbol sym = (match sym with 
  | `NT nt -> (string_of_nt nt)
  | `TM tm -> (string_of_tm tm))


(* note binarized rules - ref Scott et al. *)
type rhs = Atom of symbol | Seq of symbol * symbol | Alt of symbol * symbol

type parse_rule = nonterm * rhs

type grammar = parse_rule list

type 'a raw_parser = 'a ty_substring -> 'a ty_substring list

type ty_oracle = (symbol * symbol) -> (int * int) -> int list
(* let empty_oracle = (fun (sym1,sym2) -> fun ss -> []) *)

(* we also cache the results of terminal parsing; note that the first argument is a term, not a string! *)
type ty_tmoracle = term -> int * int -> bool

(* invariant: LC(lc): forall (_,s1) (_,s2) in lc. s1=s2 *)
type 'string local_context = LC of (nonterm * 'string ty_substring) list 
type local_context' = string local_context
let empty_context = (LC [])


type ('a,'b,'c) sum3 = Inl of 'a | Inm of 'b | Inr of 'c
let dest_inl (Inl x) = x
let dest_inm (Inm x) = x
let dest_inr (Inr x) = x

let sum3 (f,g,h) = (fun i -> match i with
  | Inl l -> Inl(f l)
  | Inm m -> Inm(g m)
  | Inr r -> Inr(h r))

let unsum3 u = (
  let f = (fun x -> dest_inl (u (Inl x))) in
  let g = (fun x -> dest_inm (u (Inm x))) in
  let h = (fun x -> dest_inr (u (Inr x))) in
  (f,g,h))

type inl = unit
type outl = symbol

type 'string mid = { 
  rules7: parse_rule list; 
  tmparsers7: (term,'string raw_parser) fmap 
}
let empty_mid = { rules7=[]; tmparsers7=empty_fmap }
type 'string inm = 'string mid
type 'string outm = 'string mid
type inm' = string inm
type outm' = string outm

type 'string inr = {
  ss4: 'string ty_substring;
  box4: 'string box; (* the underlying value is a boxed version of the value in .ss4 *)
  lc4: int local_context; (* contexts work with box keys, not 'string *)
  oracle4: ty_oracle;
  tmoracle4: ty_tmoracle;
}
type inr' = string inr
type 'a outr = 'a list

type 'string input = (inl,'string inm,'string inr) sum3
type ('string,'a) output = (outl,'string outm,'a outr) sum3

type ('string,'a) parser3 = ('string input -> ('string,'a) output)

type 'a parser3' = (string,'a) parser3

(**
{2 Definitions}
*)

let sym_of_parser p = (dest_inl (p (Inl ())))

(* we assume that _, * and + cannot appear in user supplied tms and nts *)
let mk_symbol rhs = (match rhs with
  | Atom x -> (`NT("_"^(string_of_symbol x))) (* FIXME really? we want to invent a symbol for the lhs *)
  | Seq(sym1,sym2) -> (
    `NT ("("^(string_of_symbol sym1)^"*"^(string_of_symbol sym2)^")"))
  | Alt(sym1,sym2) -> (
    `NT ("("^(string_of_symbol sym1)^"+"^(string_of_symbol sym2)^")")))

let seql p1 p2 = (fun () -> 
  let (f1,_,_) = unsum3 p1 in
  let (f2,_,_) = unsum3 p2 in
  let rhs = Seq(f1 (),f2 ()) in 
  mk_symbol rhs)
let (_:'a parser3'->'a parser3'->inl->outl) = seql

let altl p1 p2 = (fun () -> 
  let (f1,_,_) = unsum3 p1 in
  let (f2,_,_) = unsum3 p2 in
  mk_symbol (Alt(f1 (),f2 ())))

let seqm p1 p2 = (fun m -> 
  let nt = dest_NT(seql p1 p2 ()) in 
  if List.mem nt (List.map fst m.rules7) then m 
  else (
    let (f1,g1,_) = unsum3 p1 in
    let (f2,g2,_) = unsum3 p2 in
    let new_rule = (nt,Seq(f1 (), f2 ())) in
    let m1 = { m with rules7=(new_rule::m.rules7) } in
    let m2 = g1 m1 in
    let m3 = g2 m2 in
    m3))
let (_:'a parser3'->'a parser3'->inm'->outm') = seqm

let altm p1 p2 = (fun m -> 
  let nt = dest_NT (altl p1 p2 ()) in 
  if List.mem nt (List.map fst m.rules7) then m 
  else (
    let (f1,g1,_) = unsum3 p1 in
    let (f2,g2,_) = unsum3 p2 in
    let new_rule = (nt,Alt(f1 (), f2 ())) in
    let m1 = { m with rules7=(new_rule::m.rules7) } in
    let m2 = g1 m1 in
    let m3 = g2 m2 in
    m3))

(* this is where we use the productions from earley *)
let seqr p1 p2 = (fun i0 -> 
  let sym1 = sym_of_parser p1 in
  let sym2 = sym_of_parser p2 in
  let SS(s,i,j) = i0.ss4 in
  let ks = i0.oracle4 (sym1,sym2) (i,j) in
  let f1 k = (
    let rs1 = dest_inr (p1 (Inr { i0 with ss4=(SS(s,i,k)) })) in
    let rs2 = dest_inr (p2 (Inr { i0 with ss4=(SS(s,k,j)) })) in
    list_product rs1 rs2)
  in
  List.concat (List.map f1 ks))
let (_:'a parser3'->'a parser3'->inr'->('a*'a)outr) = seqr

let altr p1 p2 = (fun i -> 
  let rs1 = dest_inr (p1 (Inr i)) in
  let rs2 = dest_inr (p2 (Inr i)) in
  List.append rs1 rs2) 
let (_:'a parser3'->'a parser3'->inr'->'a outr) = altr

let ( ***> ) p1 p2 = (fun i0 -> 
  let f = seql p1 p2 in
  let g = seqm p1 p2 in
  let h = seqr p1 p2 in
  sum3 (f,g,h) i0)
let (_:'a parser3' -> 'b parser3' -> ('a * 'b) parser3') = ( ***> )

let ( |||| ) p1 p2 = (fun i0 -> 
  let f = altl p1 p2 in
  let g = altm p1 p2 in
  let h = altr p1 p2 in
  sum3 (f,g,h) i0)
let (_:'a parser3' -> 'a parser3' -> 'a parser3') = ( |||| )

let ( >>>> ) p f = (fun i -> match i with
  | Inl _ -> (Inl (dest_inl (p i)))
  | Inm _ -> (Inm (dest_inm (p i)))
  | Inr _ -> (Inr (List.map f (dest_inr (p i)))))
let (_:'a parser3' -> ('a -> 'b) -> 'b parser3') = ( >>>> )

let mkntparser' nt p = (fun i -> match i with
  | Inl () -> Inl (`NT nt)
  | Inm m -> (
    if List.mem nt (List.map fst m.rules7) then Inm m 
    else (
      let sym = sym_of_parser p in
      let new_rule = (nt,Atom sym) in
      p (Inm { m with rules7=(new_rule::m.rules7) })))
  | Inr r -> (
    let Inr rs = p i in
    Inr (unique rs)))

(*
(* N.B. in the real P3, this reuses results from the Earley parse *)
let mktmparser tm p = (fun i -> match i with
  | Inl () -> Inl (`TM tm)
  | Inm m -> (
    if List.mem tm (fmap_dom m.tmparsers7) then Inm m
    else
      Inm { m with tmparsers7=(fmap_update m.tmparsers7 (tm,p))})
  | Inr i -> (
    let ss = i.ss4 in
    let rs = p ss in
    (* we want to parse exactly, but raw_parsers are prefix parsers *)
    if List.mem ss rs then
      Inr[ss]
    else
      Inr[]))
*)

(* version which reuses results from earley parse *)
let mktmparser tm p = (fun i -> match i with
  | Inl () -> Inl (`TM tm)
  | Inm m -> (
    if List.mem tm (fmap_dom m.tmparsers7) then Inm m
    else
      Inm { m with tmparsers7=(fmap_update m.tmparsers7 (tm,p))})
  | Inr i0 -> (
    let SS(s,i,j) = i0.ss4 in
    if i0.tmoracle4 tm (i,j) then
      Inr[SS(s,i,j)]
    else
      Inr[]))

let grammar_of_parser p = (dest_inm (p (Inm empty_mid)))

let run_parser3' (o,tmo) p s len = (
  let i0 = { 
    ss4=(SS(s,0,len)); 
    box4=(box s);
    lc4=empty_context; 
    oracle4=o;
    tmoracle4=tmo } 
  in
  let rs = dest_inr (p (Inr i0)) in
  unique rs)

(**
{2 Earley parsing}
*)

(* open Earley3.Earley_interface (* for field selectors *) *)

(*
type item = Earley3.Earley_interface.nt_item
type production = item * int
*)

(* the following is boring code to coerce minip3 values to P3 values, so we can use P3_lib.Earley *)
let p3_of_sym sym = sym

let sym_of_p3 sym = sym

let item_of_p3 itm = itm

let oracle_of_parser p s len = (
  let earley_grammar_of_grammar g = (
    let f1 x = (match x with 
      | Atom (x) -> [[x]]
      | Seq (x,y) -> [[x;y]]
      | Alt (x,y) -> [[x];[y]])
    in
    let f2 (nt,x) = List.map (fun x -> (nt,x)) (f1 x) in
    let rs = List.concat (List.map f2 g) in
    List.map (fun (nt,rhs) -> (nt,List.map p3_of_sym rhs)) rs)
  in
  let setup_of_parser p s = (
    let m = grammar_of_parser p in
    let g = earley_grammar_of_grammar m.rules7 in
    let nt = dest_NT (sym_of_parser p) in
    let p_of_tm =(
        fun tm -> 
          let p = List.assoc tm m.tmparsers7 in
          fun (s,i,j) -> 
            let rs = p (SS(s,i,j)) in
            List.map (fun s1 -> let SS(s',i',j') = s1 in ((s',i',j'),(s',j',j))) rs)
    in
    `Setup(`G7(g),`Sym7(nt),`P_of_tm7(p_of_tm),`String7(s,len)))
  in
  let s = setup_of_parser p s in
  let (o,tmo) = (
    (* if we think the size overhead will not be too big, we use the
       imperative version, otherwise the functional version. FIXME
       total hack *)
    if len < 5000 then 
      let r = Earley3_imp.Earley_interface.earley_full s in
      let (o,tmo) = match r with `Loop2(_,`Oracle(o,tmo)) -> (o,tmo) in
      (o,tmo)
    else
      let r = Earley3_fun.Earley_interface.earley_full s in
      let (o,tmo) = match r with `Loop2(_,`Oracle(o,tmo)) -> (o,tmo) in
      (o,tmo))
  in
  (o,tmo))
let (_:'a parser3' -> string -> int -> (ty_oracle * ty_tmoracle)) = oracle_of_parser

let run_parser3 p s len = (
  let (o,tmo) = oracle_of_parser p s len in
  run_parser3' (o,tmo) p s len)

let p3_run_parser = run_parser3

let p3_run_parser_string p s = run_parser3 p s (String.length s) (* FIXME remove run_parser3 in favour of p3_run_parser *)

(**
{2 Context definitions}
*)


(* memoization works best if contexts are represented using sorted lists *)
(*
let lc_cmp (nt1,SS(s1,i1,j1)) (nt2,SS(s2,i2,j2)) = (
  Pervasives.compare (nt1,(i1,j1)) (nt2,(i2,j2)))
*)

let lc_cmp (nt1,ss1) (nt2,ss2) = (
  Pervasives.compare (nt1,ss1) (nt2,ss2))

(* this version is equivalent to the previous if we use normalized contexts *)
(*
let lc_cmp (nt1,ss1) (nt2,ss2) = (
  if ss1 <> ss2 then failwith "lc_cmp: impossible" 
  else Pervasives.compare nt1 nt2)
*)

(* when parsing the input between l and h, the only part of the
 context that matters are those entries (nt,(l',h')) st (l',h') =
 (l,h); so there is a notion of a normalized context (important
 for memoization) *)

(* memoization works best if contexts are normalized *)
(*
let normalize_context (LC(lc)) (SS(s,i,j)) = (
  LC(List.filter (fun (nt,SS(s',i',j')) -> (i',j')=(i,j)) lc))
*)

let normalize_context i0 (LC(lc)) (SS(s,l,h)) = (
  let SS(s,l,h) = SS(box_get_key i0.box4,l,h) in
  LC(List.filter (fun (nt,ss') -> ss'=SS(s,l,h)) lc))

(* for contexts, we work with substrings whose values are ints *)
let update_context i0 c (nt,SS(s,l,h)) = (
  let LC(lc) = normalize_context i0 c (SS(s,l,h)) in
  let SS(s,l,h) = SS(box_get_key i0.box4,l,h) in
  LC(myinsert lc_cmp (nt,SS(s,l,h)) lc))

(* simpler definition; don't need lc_cmp, normalize_context and previous update_context

let update_context (LC(lc)) (nt,SS(s,l,h)) = (
  LC((nt,SS(s,l,h))::lc))

*)

(*
let context_contains (LC(lc)) (nt,SS(s,l,h)) = (
  List.exists (fun (nt',SS(_,i',j')) -> (nt,i',j') = (nt,l,h)) lc)
*)

let context_contains i0 (LC(lc)) (nt,SS(s,l,h)) = (
  let SS(s,l,h) = SS(box_get_key i0.box4,l,h) in
  List.exists (fun (nt',ss') -> (nt',ss') = (nt,SS(s,l,h))) lc)

let update_lc4 nt p = (fun i -> match i with
  | Inr i0 -> p (Inr { 
    i0 with lc4=(update_context i0 i0.lc4 (nt,i0.ss4)) })
  | _ -> p i)

let check_and_upd_lc4 p = (fun i -> match i with 
  | Inr i -> (
    let (f,g,h) = unsum3 p in
    let nt = dest_NT (sym_of_parser p) in
    let should_trim = context_contains i i.lc4 (nt,i.ss4) in
    if should_trim then 
      Inr []
    else
      update_lc4 nt p (Inr i))
  | _ -> p i)

let mkntparser nt p = (
  check_and_upd_lc4 (mkntparser' nt p))

