(**
{1 earley.ml}

{2 Earley types}

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

  (* oracle for a parsed 'a substring *)
  type ty_oracle = (symbol*symbol) -> (int*int) -> int list

  (* we also cache the results of terminal parsing; FIXME we really only need to check whether tm,int,int is accepted, so result type could be bool  *)
  type ty_tmoracle = term -> int * int -> bool 

  (* FIXME move some of these types to private - we have the oracle now as the interface type *)
  type nt_item = { nt2: nonterm; a2: symbol list; (* c2:(int*int) list;*) b2: symbol list; i2:int; j2:int }
  type complete_nt_item = nt_item  (* those where b2=[] *)
  type tm_item = { tm5:term; i5:int; j5:int; comp5:bool }
  type complete_tm_item = tm_item  (* those where complete5=true *)
  type sym_item = NTITM of nt_item | TMITM of tm_item

  (* total ordering on items, for sets and maps which require such orderings *)
  let nt_compare x y =
    let n = Pervasives.compare x.nt2 y.nt2 in
    if n <> 0 then n else
      let n = Pervasives.compare x.i2 y.i2 in
      if n <> 0 then n else
        let n = Pervasives.compare x.j2 y.j2 in
        if n <> 0 then n else
          Pervasives.compare (x.a2,x.b2(*,x.c2*)) (y.a2,y.b2(*,y.c2*))

  let compare_sym_item x y = (
    Pervasives.compare x y)  (* FIXME make this more efficient *)
    

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

  module Set_sym_item = MySet_Make(
    struct
      type t = sym_item
      let compare x y = compare_sym_item x y
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
    [`Done of Set_sym_item.t] *
    [`Oracle of ty_oracle * ty_tmoracle]]
  
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


  module Map_blocked_key = MyMap(
    struct
      type key = int * symbol
      type value = Set_nt_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)
    

  module Set_int = MySet_Make(
    struct
      type t = int
      let compare x y = Pervasives.compare x y
    end)

  module Set_complete_item = Set_sym_item

  module Map_complete_key = MyMap(
    struct
      type key = int * symbol
      type value = Set_complete_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_complete_item.empty
    end)
  
  (* oracle implementation type *)
  module Map_sym_sym_int_int = MyMap(
    struct
      type key = symbol * symbol * int * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

  (* this is the type used by the earley routines - typically external routines should invoke via Earley.earley_main, so they see only Set_nt_item.t rather than this type *)

  (* tmoracle impl type *)
  module Map_term_int_int = MyMap(
    struct
      type key = term * int * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

  module Map_term_int = MyMap(
    struct
      type key = term * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)


  type ty_loop2 = {
    done5: Set_sym_item.t;
    todo5: Set_sym_item.t;
    oracle5: Map_sym_sym_int_int.ty_map;
    tmoracle5: Map_term_int.ty_map;
    blocked5: Map_blocked_key.ty_map;
    complete5: Map_complete_key.ty_map
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

  type ty_loop1 = 
    | Nt_blocked of (nt_item * sym_item list)  (* item was blocked *)
    | Nt_complete of complete_nt_item  (* item was complete, no processing occurred *)
    | Tm_incomplete of complete_tm_item list  (* item was incomplete, terminal parsing occurred *)
    | Tm_complete of complete_tm_item  (* item was complete, no processing occurred *)
    
(*
  let key_of_blocked itm = (itm.j2,List.hd itm.b2)
  let key_of_complete_tm citm = (citm.tm6,citm.i6,citm.k6)
  let key_of_complete_nt itm (itm.i2,`NT itm.nt2)
*)
  
  let rules_for_nt ctxt nt = (
    let rs = List.filter (fun (nt',rhs) -> nt' = nt) ctxt.g5 in
    rs)         
  
  let shift_a2_b2_c2 itm = {itm with
    a2=(List.hd itm.b2)::itm.a2;
    b2=(List.tl itm.b2)
  }

  let update_oracle m (itm,l) = (
    if not ((List.length itm.a2=1) && (List.length itm.b2=1)) then 
      m
    else (
      let (sym1,sym2) = (List.hd itm.a2,List.hd itm.b2) in
      let (i,j) = (itm.i2,l) in
      let k = itm.j2 in
      let key = (sym1,sym2,i,j) in
      let ks = Map_sym_sym_int_int.find2 key m in
      let ks = Set_int.add k ks in (* FIXME performance *)
      let m = Map_sym_sym_int_int.add key ks m in
      m))

  let update_tmoracle m citm = (
    let _ = if citm.comp5 <> true then failwith "update_tmoracle" else () in
    let key = (citm.tm5,citm.i5) in
    let js = Map_term_int.find2 key m in
    let js = Set_int.add citm.j5 js in
    let m = Map_term_int.add key js m in
    m)
    
  let loop1 ctxt itm1 = (
    match itm1 with
    | NTITM itm0 -> (
      match itm0.b2 with 
      | [] -> (Nt_complete itm0)
      | sym::syms -> (
        match sym with 
        | `NT nt -> ( 
          let rs = rules_for_nt ctxt nt in
          let alts = List.map snd rs in
          let f1 alt = NTITM {nt2=nt; a2=[]; b2=alt; i2=itm0.j2; j2=itm0.j2} in
          Nt_blocked(itm0,(List.map f1 alts)))
        | `TM tm -> (
          Nt_blocked(itm0,[TMITM{ tm5=tm; i5=itm0.j2; j5=itm0.j2; comp5=false }]))))
    | TMITM itm0 -> ( 
      match itm0.comp5 with 
      | true -> (Tm_complete itm0)
      | false -> (
        let tm = itm0.tm5 in
        let p = ctxt.p_of_tm5 tm in
        let rs = p (ctxt.string5,itm0.i5,ctxt.length5) in 
        let f2 (i,j) = { itm0 with j5=j; comp5=true } in
        Tm_incomplete(List.map f2 rs))))

  let j_of_itm itm = (
    match itm with
    | NTITM itm -> itm.j2
    | TMITM itm -> itm.j5)
  
  let loop2 ctxt s0 = (
    let itm0 = Set_sym_item.choose s0.todo5 in
    let r = loop1 ctxt itm0 in
    let s0 = {s0 with done5=(Set_sym_item.add itm0 s0.done5); todo5=(Set_sym_item.remove itm0 s0.todo5) } in
    let process_complete s0 citm = (
      let key_of_complete itm = (
        match itm with
        | NTITM itm -> (itm.i2,`NT itm.nt2)
        | TMITM itm -> (itm.i5,`TM itm.tm5))
      in
      let k = key_of_complete citm in
      let complete_items = Map_complete_key.find2 k s0.complete5 in
      let blocked_items = Map_blocked_key.find2 k s0.blocked5 in
      let map f s = (
        let f1 x s = Set_sym_item.add (f x) s in
        Set_nt_item.fold f1 s (Set_sym_item.empty))
      in
      let f1 bitm = NTITM {(shift_a2_b2_c2 bitm) with j2=(j_of_itm citm)} in
      let todo_items = map f1 blocked_items in
      let todo_items = Set_sym_item.diff todo_items s0.done5 in 
      {s0 with 
        todo5=(Set_sym_item.union s0.todo5 todo_items);
        complete5=(Map_complete_key.add k (Set_complete_item.add citm complete_items) s0.complete5);
        oracle5=(
          let f1 b oracle = update_oracle oracle (b,j_of_itm citm) in
          Set_nt_item.fold f1 blocked_items s0.oracle5);
        tmoracle5=(
          match citm with 
          | NTITM _ -> s0.tmoracle5 
          | TMITM itm -> (
            update_tmoracle s0.tmoracle5 itm))
        (* FIXME also want to process tmoracle *)
      })
    in
    match r with
    | Nt_complete itm -> (process_complete s0 (NTITM itm))
    | Tm_complete itm -> (process_complete s0 (TMITM itm))
    | Nt_blocked (bitm,todos) -> (
      let key_of_blocked bitm = (bitm.j2,List.hd bitm.b2) in
      let k = key_of_blocked bitm in
      let blocked_items = Map_blocked_key.find2 k s0.blocked5 in
      let complete_items = Map_complete_key.find2 k s0.complete5 in
      let f1 citm = NTITM {(shift_a2_b2_c2 bitm) with j2=(j_of_itm citm)} in
      let map f s = (
        let f1 x s = Set_sym_item.add (f x) s in
        Set_complete_item.fold f1 s (Set_sym_item.empty))
      in
      let todo_items = map f1 complete_items in
      let todo_items = Set_sym_item.union (Set_sym_item.from_list todos) todo_items in (* NB asymmetry *)
      let todo_items = Set_sym_item.diff todo_items s0.done5 in
      {s0 with 
        todo5=(Set_sym_item.union s0.todo5 todo_items);
        blocked5=(Map_blocked_key.add k (Set_nt_item.add bitm blocked_items) s0.blocked5);
        oracle5=(
          let f1 c oracle = update_oracle oracle (bitm,j_of_itm c) in
          Set_complete_item.fold f1 complete_items s0.oracle5)
      })
    | Tm_incomplete todos -> (
      let f1 citm = TMITM citm in
      {s0 with todo5=(Set_sym_item.list_union (List.map f1 todos) s0.todo5)}))
  
  let rec earley ctxt s0 = (if Set_sym_item.is_empty s0.todo5 then s0 else (earley ctxt (loop2 ctxt s0)))

  let earley_initial_state ctxt0 = ({
      done5=Set_sym_item.empty;
      todo5=(
        let initial_itm = (NTITM{nt2=ctxt0.sym5;a2=[];b2=[`NT ctxt0.sym5];i2=0;j2=0}) in
        Set_sym_item.add initial_itm Set_sym_item.empty);
      oracle5=Map_sym_sym_int_int.empty;
      tmoracle5=Map_term_int.empty;
      blocked5=Map_blocked_key.empty;
      complete5=Map_complete_key.empty
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


  let memo tbl f i = (
    if (Hashtbl.mem tbl i) then
      (Hashtbl.find tbl i)
    else (
      let v = f i in
      let _ = Hashtbl.add tbl i v in
      v))
    
  let export_oracle oracle = (
    let tbl = Hashtbl.create 100 in
    let f1 = (fun (sym1,sym2,i,j) -> 
      let rs = Map_sym_sym_int_int.find2 (sym1,sym2,i,j) oracle in
      Set_int.elements rs)
    in
    memo tbl (fun (sym1,sym2) -> fun (i,j) -> f1 (sym1,sym2,i,j)))

  let export_tmoracle tmoracle = (
    let tbl = Hashtbl.create 100 in
    memo tbl (fun tm -> fun (i,j) -> Set_int.mem j (Map_term_int.find2 (tm,i) tmoracle)))
  
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
      `Loop2(`Done(s1.done5),`Oracle(export_oracle s1.oracle5,export_tmoracle s1.tmoracle5))))
  let (_:'a ty_setup -> full_earley_data) = earley_full

  let earley_main setup0 = (
    let r = earley_full setup0 in 
    match r with
    | `Loop2(`Done(itms),_) -> itms)
  let (_:'a ty_setup -> Set_sym_item.t) = earley_main
  
end


(**
{2 Earley debug}

Some debugging functions. Work with the last earley state that was found.

*)
module Earley_debug = struct

(*  open Prelude
  open Types *)
  open Earley_private_types
  let j_of_itm = EarleyCore.j_of_itm

  module Map_int = MyMap(
    struct 
      type key = int
      type value = Set_nt_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)

  (* we want to find completed items that start from the same position; TODO ideally these should be items that contributed to a successful parse; to do this, we index by int *)
 
  (* FIXME this won't work: it will find lots of items e.g. START -> X will find both X and start; there are many positions i from which more than one nt can be parsed - this is very expected;  *)

(*
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
*)

  (* find the maximal position reached *)
  let max_position s0 = (
    let cs = List.map snd (Map_complete_key.bindings s0.complete5) in
    let f1 acc itms = (Set_complete_item.elements itms)@acc in
    let cs = List.fold_left f1 [] cs in
    let f1 m itm = if j_of_itm itm > m then j_of_itm itm else m in
    let m = List.fold_left f1 0 cs in
    m)

end

