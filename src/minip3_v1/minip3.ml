(*

# Interactive use

This code depends on an implementation of Earley parsing from the P3
library. In the interactive OCaml toplevel, type the following before
using the rest of the code:

    #mod_use "mini_p3_lib.ml";;
*)

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


(*
# Types
*)

type ('dom,'cod) fmap = ('dom * 'cod) list

let fmap_update m (a,b) = (a,b)::m
let fmap_lookup m a = List.assoc a m
let fmap_dom m = List.map fst m
let empty_fmap = []

(* invariant: SS(s,i,j): i<=j<=(String.length s) *)
type substring = SS of string * int * int

let dest_SS (SS(s,i,j)) = (s,i,j)

type term = string

type nonterm = string

type symbol = NT of nonterm | TM of term 

let dest_NT sym = (match sym with NT x -> x | _ -> failwith "dest_NT")

let string_of_tm tm = tm
let string_of_nt nt = nt
(*
let string_of_symbol sym = (match sym with 
  | NT nt -> "NT("^(string_of_nt nt)^")" 
  | TM tm -> "TM("^(string_of_tm tm)^")")
*)

(* we assume nonterminals and terminals correspond to disjoint subsets of string *)
let string_of_symbol sym = (match sym with 
  | NT nt -> (string_of_nt nt)
  | TM tm -> (string_of_tm tm))


(* note binarized rules - ref Scott et al. *)
type rhs = Atom of symbol | Seq of symbol * symbol | Alt of symbol * symbol

type parse_rule = nonterm * rhs

type grammar = parse_rule list

type raw_parser = substring -> substring list

type ty_oracle = (symbol * symbol) -> substring -> int list
let empty_oracle = (fun (sym1,sym2) -> fun ss -> [])

(* invariant: LC(lc): forall (_,s1) (_,s2) in lc. s1=s2 *)
type local_context = LC of (nonterm * substring) list 
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

type mid = { 
  rules7: parse_rule list; 
  tmparsers7: (term,raw_parser) fmap 
}
let empty_mid = { rules7=[]; tmparsers7=empty_fmap }
type inm = mid
type outm = mid

type inr = {
  ss4: substring;
  lc4: local_context;
  oracle4: ty_oracle
}
type 'a outr = 'a list

type input = (inl,inm,inr) sum3
type 'a output = (outl,outm,'a outr) sum3

type 'a parser3 = (input -> 'a output)

(*
# Definitions
*)

let sym_of_parser p = (dest_inl (p (Inl ())))

(* we assume that _, * and + cannot appear in user supplied tms and nts *)
let mk_symbol rhs = (match rhs with
  | Atom x -> (NT("_"^(string_of_symbol x))) (* FIXME really? we want to invent a symbol for the lhs *)
  | Seq(sym1,sym2) -> (
    NT ("("^(string_of_symbol sym1)^"*"^(string_of_symbol sym2)^")"))
  | Alt(sym1,sym2) -> (
    NT ("("^(string_of_symbol sym1)^"+"^(string_of_symbol sym2)^")")))

let seql p1 p2 = (fun () -> 
  let (f1,_,_) = unsum3 p1 in
  let (f2,_,_) = unsum3 p2 in
  let rhs = Seq(f1 (),f2 ()) in 
  mk_symbol rhs)
let (_:'a parser3->'a parser3->inl->outl) = seql

let altl p1 p2 = (fun () -> 
  let (f1,_,_) = unsum3 p1 in
  let (f2,_,_) = unsum3 p2 in
  mk_symbol (Alt(f1 (),f2 ())))

let seqm p1 p2 = (fun m -> 
  let NT nt = seql p1 p2 () in 
  if List.mem nt (List.map fst m.rules7) then m 
  else (
    let (f1,g1,_) = unsum3 p1 in
    let (f2,g2,_) = unsum3 p2 in
    let new_rule = (nt,Seq(f1 (), f2 ())) in
    let m1 = { m with rules7=(new_rule::m.rules7) } in
    let m2 = g1 m1 in
    let m3 = g2 m2 in
    m3))
let (_:'a parser3->'a parser3->inm->outm) = seqm

let altm p1 p2 = (fun m -> 
  let NT nt = altl p1 p2 () in 
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
  let ks = i0.oracle4 (sym1,sym2) i0.ss4 in
  let SS(s,i,j) = i0.ss4 in
  let f1 k = (
    let rs1 = dest_inr (p1 (Inr { i0 with ss4=(SS(s,i,k)) })) in
    let rs2 = dest_inr (p2 (Inr { i0 with ss4=(SS(s,k,j)) })) in
    list_product rs1 rs2)
  in
  List.concat (List.map f1 ks))
let (_:'a parser3->'a parser3->inr->('a*'a)outr) = seqr

let altr p1 p2 = (fun i -> 
  let rs1 = dest_inr (p1 (Inr i)) in
  let rs2 = dest_inr (p2 (Inr i)) in
  List.append rs1 rs2) 
let (_:'a parser3->'a parser3->inr->'a outr) = altr

let ( ***> ) p1 p2 = (fun i0 -> 
  let f = seql p1 p2 in
  let g = seqm p1 p2 in
  let h = seqr p1 p2 in
  sum3 (f,g,h) i0)
let (_:'a parser3 -> 'b parser3 -> ('a * 'b) parser3) = ( ***> )

let ( |||| ) p1 p2 = (fun i0 -> 
  let f = altl p1 p2 in
  let g = altm p1 p2 in
  let h = altr p1 p2 in
  sum3 (f,g,h) i0)
let (_:'a parser3 -> 'a parser3 -> 'a parser3) = ( |||| )

let ( >>>> ) p f = (fun i -> match i with
  | Inl _ -> (Inl (dest_inl (p i)))
  | Inm _ -> (Inm (dest_inm (p i)))
  | Inr _ -> (Inr (List.map f (dest_inr (p i)))))
let (_:'a parser3 -> ('a -> 'b) -> 'b parser3) = ( >>>> )

let mkntparser' nt p = (fun i -> match i with
  | Inl () -> Inl (NT nt)
  | Inm m -> (
    if List.mem nt (List.map fst m.rules7) then Inm m 
    else (
      let sym = sym_of_parser p in
      let new_rule = (nt,Atom sym) in
      p (Inm { m with rules7=(new_rule::m.rules7) })))
  | Inr r -> (
    let Inr rs = p i in
    Inr (unique rs)))

(* N.B. in the real P3, this reuses results from the Earley parse *)
let mktmparser tm p = (fun i -> match i with
  | Inl () -> Inl (TM tm)
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

let grammar_of_parser p = (dest_inm (p (Inm empty_mid)))

let run_parser3' oracle p s = (
  let i0 = { 
    ss4=(SS(s,0,String.length s)); 
    lc4=empty_context; 
    oracle4=oracle } 
  in
  let rs = dest_inr (p (Inr i0)) in
  unique rs)

(*
# Example terminal parsers
*)

(* consume a single "1" from the input; N.B. prefix parsing! *)
let raw_a1 (SS(s,i,j)) = (
  if i<j && s.[i] = '1' then 
    [SS(s,i,i+1)]
  else
    [])

let a1 = mktmparser "1" raw_a1

(* consume (!) the empty string *)
let raw_eps (SS(s,i,j)) = [SS(s,i,i)]

let eps = mktmparser "eps" raw_eps


(*
# Earley parsing
*)

type item = { 
  nt2: nonterm; 
  a2: symbol list; 
  b2: symbol list; 
  i2:int; 
  j2:int 
}

type production = item * int

(* the following is boring code to coerce minip3 values to P3 values, so we can use P3_lib.Earley *)
let p3_of_sym sym = (
  let module X = Mini_p3_lib.Types in
  match sym with
  | NT nt -> (X.NT nt)
  | TM tm -> (X.TM tm))

let sym_of_p3 sym = (
  let module X = Mini_p3_lib.Types in
  match sym with
  | X.NT nt -> (NT nt)
  | X.TM tm -> (TM tm))

let item_of_p3 itm = (
  let module X = Mini_p3_lib.EarleyTypes in 
  {
    nt2=itm.X.nt2;
    a2=(List.map sym_of_p3 itm.X.a2);
    b2=(List.map sym_of_p3 itm.X.b2);
    i2=itm.X.i2;
    j2=itm.X.j2
  })

let earley_prods_of_parser p s = (
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
    let open Mini_p3_lib.Types in
    {
      g7=g;
      sym7=nt;
      p_of_tm7=(
        fun tm -> 
          let p = List.assoc tm m.tmparsers7 in
          fun (s,i,j) -> 
            let rs = p (SS(s,i,j)) in
            List.map (fun s1 -> let SS(s',i',j') = s1 in ((s',i',j'),(s',j',j))) rs);
      string7=s
    })
  in
  let open Mini_p3_lib.EarleyTypes in 
  let s = setup_of_parser p s in
  let prods = (Mini_p3_lib.Earley.earley_full s).prod5 in
  let elts = Set_nt_item_int.elements prods in
  let map = rev_map in
  map (fun (itm,k) -> (item_of_p3 itm,k)) elts)
let (_:'a parser3 -> string -> production list) = earley_prods_of_parser

let oracle_of_prods ps = (fun (sym1,sym2) -> fun (SS(s,i',j')) ->
  let f1 (itm,l) = (itm.a2=[sym1]) 
    && (itm.b2=[sym2]) 
    && (i',j') = (itm.i2,l) 
  in
  let ps = List.filter f1 ps in
  List.map (fun (itm,_) -> itm.j2) ps)

(* alternative, more efficient oracle_of_prods *)
module Map_sym_sym_int_int = Mini_p3_lib.EarleyTypes.MyMap(
  struct
    type key = symbol * symbol * int * int
    type value = int list
    let compare x y = Pervasives.compare x y
    let default = []
  end)

let oracle_of_prods ps = (
  let module X = Map_sym_sym_int_int in
  let m = X.empty in
  let f1 m (itm,l) = (
    if not ((List.length itm.a2=1) && (List.length itm.b2=1)) then 
      m
    else (
      let (sym1,sym2) = (List.hd itm.a2,List.hd itm.b2) in
      let (i,j) = (itm.i2,l) in
      let k = itm.j2 in
      let key = (sym1,sym2,i,j) in
      let ks = X.find2 key m in
      let ks = unique (k::ks) in
      let m = X.add key ks m in
      m))
  in
  let m = List.fold_left f1 m ps in
  fun (sym1,sym2) -> fun (SS(s,i,j)) -> (
    (* assume always called on the same string s *)
    X.find2 (sym1,sym2,i,j) m))

let run_parser3 p s = (
  let ps = earley_prods_of_parser p s in
  let oracle = oracle_of_prods ps in
  run_parser3' oracle p s)

(*
# Context definitions
*)


(* memoization works best if contexts are represented using sorted lists *)
let lc_cmp (nt1,ss1) (nt2,ss2) = (
  Pervasives.compare (nt1,ss1) (nt2,ss2))

(* this version is equivalent to the previous if we use normalized contexts *)
let lc_cmp (nt1,ss1) (nt2,ss2) = (
  if ss1 <> ss2 then failwith "lc_cmp: impossible" 
  else Pervasives.compare nt1 nt2)


(* when parsing the input between l and h, the only part of the
 context that matters are those entries (nt,(l',h')) st (l',h') =
 (l,h); so there is a notion of a normalized context (important
 for memoization) *)

(* memoization works best if contexts are normalized *)
let normalize_context (LC(lc)) ss = (
  LC(List.filter (fun (nt,ss') -> ss'=ss) lc))

let update_context c (nt,SS(s,l,h)) = (
  let LC(lc) = normalize_context c (SS(s,l,h)) in
  LC(myinsert lc_cmp (nt,SS(s,l,h)) lc))

(* simpler definition; don't need lc_cmp, normalize_context and previous update_context

let update_context (LC(lc)) (nt,SS(s,l,h)) = (
  LC((nt,SS(s,l,h))::lc))

*)

let context_contains (LC(lc)) (nt,SS(s,l,h)) = (
  List.exists (fun x -> x = (nt,SS(s,l,h))) lc)

let update_lc4 nt p = (fun i -> match i with
  | Inr i -> p (Inr { 
    i with lc4=(update_context i.lc4 (nt,i.ss4)) })
  | _ -> p i)

let check_and_upd_lc4 p = (fun i -> match i with 
  | Inr i -> (
    let (f,g,h) = unsum3 p in
    let NT nt = sym_of_parser p in
    let should_trim = context_contains i.lc4 (nt,i.ss4) in
    if should_trim then 
      Inr []
    else
      update_lc4 nt p (Inr i))
  | _ -> p i)

let mkntparser nt p = (
  check_and_upd_lc4 (mkntparser' nt p))


(*
# Memoization
*)

(* example memo function *)

let memo tbl f i = (
  if (Hashtbl.mem tbl i) then
    (Hashtbl.find tbl i)
  else (
    let v = f i in
    let _ = Hashtbl.add tbl i v in
    v))

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
    let lc4 = normalize_context i.lc4 ss in
    let k = (lc4,ss) in
    Some k)
  | _ -> None)

let memo_p3 tbl p i = (
  memo tbl key_of_input p i)

(*
# Examples
*)

let rec parse_E = (fun i -> mkntparser' "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| (a1 >>>> (fun _ -> 1)))
  i)
let (_:int parser3) = parse_E

(* a more precise oracle; in fact, the only syms we get called on are E and (E*E) *)
let oracle = (fun (sym1,sym2) -> fun (SS(s,i,j)) -> 
  match (sym1,sym2) with
  | (NT "E",NT("(E*E)")) -> (upto' i (j-1))
  | (NT "E",NT("E")) -> (upto' i j))

let _ = run_parser3' oracle parse_E "1111111"
(* - : int list = [7] *)

(* we can afford to be sloppy in our handling of terminal parsers because mktmparser only returns a result if called on exactly the right substring *)
let oracle = fun (sym1,sym2) -> fun (SS(s,i,j)) -> (upto' i j)

let _ = run_parser3' oracle parse_E "1111111"
(* - : int list = [7] *)

let _ = sym_of_parser parse_E
(* - : symbol = NT "E" *)

let m = grammar_of_parser parse_E
(*
val m : mid =
{rules7 =
  [("(E*E)", Seq (NT "E", NT "E")); ("(E*(E*E))", Seq (NT "E", NT "(E*E)"));
   ("((E*(E*E))+1)", Alt (NT "(E*(E*E))", TM "1"));
   ("E", Atom (NT "((E*(E*E))+1)"))];
 tmparsers7 = [("1", <fun>)]}
*)

let ps = earley_prods_of_parser parse_E "1111111"

let oracle = oracle_of_prods ps

let _ = oracle (NT "E",NT "(E*E)") (SS("1111111",0,7))

let _ = run_parser3 parse_E "1111111"


(* example 4 *)
let tbl = Hashtbl.create 0

let rec parse_E = (fun i -> memo_p3 tbl (mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> x+y+z))
  |||| (a1 >>>> (fun _ -> 1))
  |||| (eps >>>> (fun _ -> 0))))
  i)
let (_:int parser3) = parse_E

let _ = (
  let [n] = run_parser3 parse_E "1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111" in
  print_endline (string_of_int n))

(* example 5 *)

let tbl = Hashtbl.create 0

let rec parse_E = (fun i -> memo_p3 tbl (mkntparser "E" (
  ((parse_E ***> parse_E ***> parse_E) >>>> (fun (x,(y,z)) -> `NODE(x,y,z)))
  |||| (a1 >>>> (fun _ -> `LEAF(1)))
  |||| (eps >>>> (fun _ -> `LEAF(0)))))
  i)

let _ = (List.length (run_parser3 parse_E "111111"))
(* 1 1 3 19 150 1326 12558  A120590 *)
