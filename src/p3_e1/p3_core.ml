(*
# Interactive top-level directives

In bash, before running toplevel, or executing native code, the following may improve performance (but is not necessary):

    # from http://caml.inria.fr/pub/docs/manual-ocaml-4.00/manual024.html
    export OCAMLRUNPARAM="s=8M,l=8M,i=1M"

Then start the ocaml toplevel interpreter:

    ocaml

For development, type the following to load the dependencies:

    #cd "/tmp/l/general/research/parsing/src/p3";; (* or wherever the p3_lib.ml file is located *)
    #use "p1/p1_lib.toplevel.ml";;
    #use "earley/earley.toplevel.ml";;
    (* now start including definitions from p3.ml *)

To use `p3_lib` itself: 

    #cd "/tmp/l/general/research/parsing/src/p3";; (* or wherever the p3_lib.ml file is located *)
    #use "p3_lib.toplevel.ml";;

You should then be able to e.g. execute the code inside the `P3Examples` module.
*)

(**
{1 p3_core.ml}

{2 P3: third version of parser combinators}

  * The key type here is `Map_symbol_int_int`, which is a map from `(sym2,i,k)` to a set (list) of integers `j`. It is used in the definition of the `*>` combinator. The idea is that we have two parsers, `p1` and `p2`. Parser `p2` corresponds to symbol `sym2`. When parsing `p1 *> p2` for the extent `(i,k)` we need to know `j` such that we can parse `p1` on the extent `(i,j)` and `p2` on the extent `(j,k)`. To achieve this, we look up `(sym2,i,k)` in the map, to give the possible `j`s.

  * To form the map, we take the productions `((Y -> alpha.Xbeta,i,j),k)` from the earley stage. Then the relevant symbol is `X`, and `i,j,k` are as described above. FIXME FIXME except that we don't really know i because we discard alpha.

  * *In addition* to the above, for a terminal `TM tm`, and an extent `(j,k)` we need to know if we can parse that terminal for that extent. This is the case if we have an `((X->alpha.(TM tm)beta,i,j),k)`. Note that in this case, the input is `(j,k)`, whereas in the previous use of `Map_symbol_int_int`, the input was `(i,k)`.


*)

(* we want to represent a parser as a triple of functions, one which
   gives the nt, one which gives the rhs, and the final that actually
   does the parsing and applying the actions *)

(* we want to define the combinators on each index, then project out *)

open P1_lib.P1_core.Prelude
open P1_lib.P1_core.Types
open Earley.Earley_interface

let allpairs = P1_lib.P1_core.Prelude.allpairs
let insert = P1_lib.P1_core.Prelude.insert
let parser_of_raw_parser = P1_lib.P1_core.Common.parser_of_raw_parser

module Set_int = MySet_Make(
  struct
    type t = int
    let compare x y = Pervasives.compare x y
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
  (`NT ("("^(string_of_symbol sym1)^"***>"^(string_of_symbol sym2)^")")))

let (_:'a myfun2 -> 'b myfun2 -> unit -> symbol) = seq1

let alt1 p1 p2 () = (
  let p1 = unwrap3 p1 in
  let p2 = unwrap3 p2 in
  let (sym1,sym2) = (p1.sym9 (), p2.sym9 ()) in
  (`NT ("("^(string_of_symbol sym1)^"||||"^(string_of_symbol sym2)^")")))

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
  (`NT ("("^(string_of_symbol sym1)^"||||*"^(string_of_symbol sym2)^")")))

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
    p (Three { i with lc4=(P1_lib.P1_core.Context.update_context i.lc4 (nt,lc4_substring_of i)) }))
  | _ -> p i)

(* FIXME these combinators could be made more efficient *)

let check_and_upd_lc4 p = (fun i -> 
  match i with 
  | Three i -> (
    let sym = dest_OutOne (p One) in
    let nt = dest_NT sym in 
    let should_trim = P1_lib.P1_core.Context.context_contains i.lc4 (nt,lc4_substring_of i) in
    if should_trim then (OutThree []) else ((update_lc4 nt p (Three i))))
  | _ -> p i)

let mkntparser' nonterm f = (fun i -> match i with
  | One -> (OutOne (`NT nonterm))
  | Two rs -> (
    let sym_f = dest_OutOne (f One) in
    let {rules7=rs';tmparsers7=tms} = dest_OutTwo (f i) in
    (* FIXME what if sym is a TM *)
    OutTwo({rules7=(insert (nonterm,Atom sym_f) rs');tmparsers7=tms}))
  | Three _ -> (
    (* FIXME we have adjusted results to return only unique items *)
    let rs = dest_OutThree (f i) in
    OutThree (P1_lib.P1_core.Prelude.unique rs)))

(* now, mkntparser includes a context check *)
let mkntparser nonterm f = (check_and_upd_lc4 (mkntparser' nonterm f))

let mktmparser term (p:raw_parser) = (fun i -> match i with
  | One -> (OutOne (`TM term))
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
  let setup = `Setup(`G7(g0),`Sym7(nt0),`P_of_tm7(p_of_tm),`String7(txt,String.length txt)) in
  let r = Earley.Earley_interface.earley_full setup in
  match r with
  | `Loop2(`Done(_),`Prod(prods)) -> prods)
let (_:'a parser123 -> string -> Set_nt_item_int.t) = earley_prods


(* take the result from earley_prods, and process to give two maps, which are then used when applying the actions in earley_actions *)

let process_prods prods = (
  let open Earley.Earley_interface in
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
    | `NT _ -> m
    | `TM tm -> (
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
    | `NT _ -> m
    | `TM tm -> (
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
  let open P1_lib.P1_core.Prelude in
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
  let _ = P1_lib.P1_core.Prelude.report_timing := false in
  let prods = time (earley_prods p) txt in
  let (pprods,tmpprods) = time process_prods prods in
  time (earley_actions p (pprods,tmpprods) txt) (0,String.length txt))
let (_:'a parser123 -> string -> 'a list) = p3_run_parser

;;
