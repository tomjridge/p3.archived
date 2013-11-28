(*
# Interactive top-level directives

    #cd "/tmp/l/general/research/parsing/src/earley";; (* or wherever the earley.toplevel.ml file is located *)
    #use "earley.toplevel.ml";;

*)

(**
earley.ml
*)

(**
Earley parser
*)

(**
{2 Earley public and private types}

  * The `loop_2` type is the type of the state used when running the earley loop.

  * The `prod5` field is a set of `(nt_item,int)`. The `nt_item` is a blocked item; the int is the extent that the next symbol in the blocked item covered. For example, suppose we have the following member of `prod5`:
 
        ((Y -> alpha.Xbeta,i,j),k)

    This is supposed to indicate that the item in question was reachable, and that an item `(X -> ...,j,k)` was also reachable, and when these were combined, we got an item `(Y -> alphaX.beta,i,k)`

*)

module Earley_public_types = struct

  type term = string
  type nonterm = string
  type 'a substring = 'a * int * int
  type symbol = [`NT of nonterm | `TM of term]
  let dest_NT x = (match x with 
    | `NT x -> x | _ -> failwith "dest_NT")

  type rhs = symbol list
  type parse_rule = nonterm * rhs
  type grammar = parse_rule list
  type lc_substring = int * int

  type nt_item = { nt2: nonterm; a2: symbol list; (* c2:(int*int) list;*) b2: symbol list; i2:int; j2:int }

  (* total ordering on items, for sets and maps which require such orderings *)
  let nt_compare x y =
    let n = Pervasives.compare x.nt2 y.nt2 in
    if n <> 0 then n else
      let n = Pervasives.compare x.i2 y.i2 in
      if n <> 0 then n else
        let n = Pervasives.compare x.j2 y.j2 in
        if n <> 0 then n else
          Pervasives.compare (x.a2,x.b2(*,x.c2*)) (y.a2,y.b2(*,y.c2*))

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

  (* used to export results *)
  type full_earley_data = [ `Loop2 of
    [`Done of Set_nt_item.t] *
    [`Prod of Set_nt_item_int.t]]
  
end

(* FIXME rename to Earley_all_types since it includes public types *)
module Earley_private_types = struct

  include Earley_public_types
  
  (* FIXME move to core types? *)
  type 'a ty_ctxt = {
    g5:grammar;
    sym5:nonterm; (* nonterm initial symbol *)
    (* caneps5:symbol -> bool; ( * may be dummy value, to be computed later FIXME don't need * ) *)
    string5:'a ;
    length5: int; (* length of string5 *)
    p_of_tm5:term -> 'a substring -> lc_substring list
  }
  
  type earley_key = int * nonterm
  
  
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

  (* polymorphic record type hack *)
  type 'a ty_setup = [`Setup of
       [`G7 of grammar ] * 
       [`Sym7 of nonterm ] *
       [`P_of_tm7 of term -> 'a substring -> ('a substring * 'a substring) list ] *
       [`String7 of 'a * int ] ] (* int arg is length of 'a; assumed to start from 0 *)

end

(**
{2 Earley core}

See Earley types for an explanation of the `prod` field.

*)

module EarleyCore = struct
  
(*  open Types *)
  open Earley_private_types

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
      | `NT nt -> ( 
        let rs = rules_for_nt ctxt nt in
        let alts = List.map snd rs in
        let f1 alt = {nt2=nt; a2=[]; b2=alt; i2=itm0.j2; j2=itm0.j2} in
        {s1 with blocked7=true; todo7=(List.map f1 alts) })
      | `TM tm -> (
        let p = ctxt.p_of_tm5 tm in
        let rs = p (ctxt.string5,itm0.j2,ctxt.length5) in 
        let f2 (i,j) = { (shift_a2_b2_c2 itm0) with j2=j } in
        let f3 (i,j) = (itm0,j) in 
        {s1 with todo7=(List.map f2 rs); prod7=(List.map f3 rs) })))
  
  let loop2 ctxt s0 = (
    let itm0 = Set_nt_item.choose s0.todo5 in
    let r = loop1 ctxt itm0 in
    (* could done5 be a list? would this improve efficiency? *)
    let s0 = {s0 with done5=(Set_nt_item.add itm0 s0.done5); todo5=(Set_nt_item.remove itm0 s0.todo5) } in
    let s0 = (match r.complete7 with | false -> s0 | true -> 
      let k = key_of_complete itm0 in
      let complete_items = Map_earley_key.find2 k s0.complete5 in
      let blocked_items = Map_earley_key.find2 k s0.blocked5 in
      let f1 bitm = {(shift_a2_b2_c2 bitm) with j2=itm0.j2} in
      let todo_items = Set_nt_item.map f1 blocked_items in
      let todo_items = Set_nt_item.diff todo_items s0.done5 in (* or should we just check each item when we try and process it? *)
      {s0 with 
        todo5=(Set_nt_item.union s0.todo5 todo_items);
        complete5=(Map_earley_key.add k (Set_nt_item.add itm0 complete_items) s0.complete5);
        prod5=(
          let blocked_items = Set_nt_item.elements blocked_items in (* FIXME performance of following code *)
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
          let complete_items = Set_nt_item.elements complete_items in (* FIXME performance of following code *)
          let f1 c = (itm0,c.j2) in
          Set_nt_item_int.list_union (List.map f1 complete_items) s0.prod5)
      })
    in
    (* handle prod7 *)
    let s0 = { s0 with prod5=(Set_nt_item_int.list_union r.prod7 s0.prod5)} in (* FIXME change type of prod? *)
    let todo_items = Set_nt_item.from_list r.todo7 in 
    let todo_items = Set_nt_item.diff todo_items s0.done5 in
    let s0 = {s0 with todo5=(Set_nt_item.union todo_items s0.todo5)} in
    s0)
  
  let rec earley ctxt s0 = (if Set_nt_item.is_empty s0.todo5 then s0 else (earley ctxt (loop2 ctxt s0)))

  let earley_initial_state ctxt0 = ({
      done5=Set_nt_item.empty;
      todo5=(
        let initial_itm = ({nt2=ctxt0.sym5;a2=[];b2=[`NT ctxt0.sym5];i2=0;j2=0}) in
        Set_nt_item.add initial_itm Set_nt_item.empty);
      prod5=Set_nt_item_int.empty;
      blocked5=Map_earley_key.empty;
      complete5=Map_earley_key.empty
    })

end

(**
{2 Earley interface}

Wrap up Earley routines to be used by others

*)

module Earley_interface = struct

  include Earley_public_types

  open Earley_private_types
  open EarleyCore 


  (* wrap the usual p_of_tm; FIXME perhaps we want to change the p_of_tm type? *)
  let earley_term_to_parser p_of_tm tm s =
    let i = s in
    let f1 ((s,i,j),s_rem) = (i,j) in
    let rs = p_of_tm tm i in
    List.map f1 rs

  let last_earley_state = ref None
  
  let earley_full setup0 = (match setup0 with
    | `Setup(`G7(g),`Sym7(sym),`P_of_tm7(p_of_tm),`String7(s,len)) -> (
      let ctxt0 = {
        g5=g;
        sym5=sym;
        string5=s;
        length5=len;
        p_of_tm5=(earley_term_to_parser p_of_tm);
      } 
      in
      let s0 = earley_initial_state ctxt0 in
      let s1 = earley ctxt0 s0 in
      (* let _ = debug_endline "earley_main: 1" in *)
      let _ = (last_earley_state := Some s1) in
      `Loop2(`Done(s1.done5),`Prod(s1.prod5))))
  let (_:'a ty_setup -> full_earley_data) = earley_full

  let earley_main setup0 = (
    let r = earley_full setup0 in 
    match r with
    | `Loop2(`Done(itms),`Prod(_)) -> itms)
  let (_:'a ty_setup -> Set_nt_item.t) = earley_main
  
end


(**
{2 Earley debug}

Some debugging functions. Work with the last earley state that was found.

*)
module Earley_debug = struct

(*  open Prelude
  open Types *)
  open Earley_private_types

  module Map_int = MyMap(
    struct 
      type key = int
      type value = Set_nt_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)

  (* we want to find completed items that start from the same position; TODO ideally these should be items that contributed to a successful parse; to do this, we index by int *)
 
  (* FIXME this won't work: it will find lots of items e.g. START -> X will find both X and start; there are many positions i from which more than one nt can be parsed - this is very expected;  *)


  let first_ambiguous_position s0 = (
    (* process the completed items *)
    let cs = List.map snd (Map_earley_key.bindings s0.complete5) in
    let f1 acc itms = (Set_nt_item.elements itms)@acc in
    let cs = List.fold_left f1 [] cs in
    let key_of_itm itm = itm.i2 in
    let f1 m itm = (
        let k = key_of_itm itm in
        let itms = Map_int.find2 k m in
        let itms = Set_nt_item.add itm itms in
        let m' = Map_int.add k itms m in
        m')
    in
    let m = List.fold_left f1 Map_int.empty cs in
    (* also find the max k *)
    let mx = List.fold_left max 0 (List.map key_of_itm cs) in
    (* now we find the first set that has more than one completed item *)
    let rec f1 n = (
      if n > mx then None else
        let elts = Set_nt_item.elements (Map_int.find2 n m) in
        if List.length elts >1 then
          (Some elts)
        else 
          f1 (n+1))
    in
    let elts = f1 0 in
    (* now show the items that were completed at this stage *)
    elts)

  (* find the maximal position reached *)
  let max_position s0 = (
    let cs = List.map snd (Map_earley_key.bindings s0.complete5) in
    let f1 acc itms = (Set_nt_item.elements itms)@acc in
    let cs = List.fold_left f1 [] cs in
    let f1 m itm = if itm.j2 > m then itm.j2 else m in
    let m = List.fold_left f1 0 cs in
    m)

end

