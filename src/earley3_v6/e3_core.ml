(* 

A version of earley using objects to provide the set interface.

We also take this opportunity to eliminate object methods with
polymorphic type (explicit type annotation is rather boring).

Do we want to keep the ops? There is something quite nice about having
these stated explicitly. We need to ensure we can generalize over
these types though, and this requires that the types are generic, so
ty_ops must be parameterizable. 

*)


(* type 'a coord = { v9: 'a; i9: int; j9: int } *)
type 'a substring = 'a * int * int

module type EARLEY_TYPES = sig
  type nt
  type tm
  type sym

  (* type pre_nt_item *)
  type nt_item (* = pre_nt_item*int*int *)

  type tm_item (* = tm*int *)
  type sym_item (* = sym*int*int *)

  type item (* = nt_item+tm_item *)
 
end


(* the actual implementation we choose *)
(*
module XX : EARLEY_TYPES = (val (Obj.magic 0) : EARLEY_TYPES)

open XX
*)


(* FIXME make sym_case types align with universe; remove is_NTITM etc in favour of case *)
type 'a ty_ops = {
  sym_case       : 'sym -> [ `NT of 'nt | `TM of 'tm];
  sym_of_tm      : 'tm -> 'sym;
  mk_tm_coord    : ('tm * int) -> 'tm_item; (* need map tm -> i *)
  tm5            : 'tm_item -> 'tm; (* ignoring i,j, this is the only info in a tm_coord *) (* no-op - repn tm_coord by tm, which is an int for us FIXME no this is a map to tm, not to int - this only works if we have an injective map from syms to ints, which we do have! *)
  mk_sym_coord   : ('sym * int * int) -> 'sym_item; (* if syms are ints, no-op, providing we use {i|i repn symb} as the repn of items with a given sym *)
  sym6           : 'sym_item -> 'sym; (* ignoring i,j, this is the only info in a sym_coord *) (* repn i->sym *)
  nt2            : 'nt_item -> 'sym;  (* repn i->sym *)
  shift_a2_b2_c2 : 'nt_item -> 'nt_item; (* repn i->i *)
  a2_length_1    : 'nt_item -> bool;    (* repn i->b; shouldn't be used - only for debugging *)
  b2_nil         : 'nt_item -> bool;    (* repn i->b *)
  hd_a2          : 'nt_item -> 'sym;  (* repn i->sym ; only used on a singleton a2 list *)
  hd_b2          : 'nt_item -> 'sym;  (* repn i->sym; implement as hd_a2 o shift? possibly inefficient *)
  nt_items_for_nt: 'nt -> int -> 'nt_item list;
  mk_item        : [`NTITM of 'nt_item | `TMITM of 'tm_item ] -> 'item;
  dest_item      : 'item -> [`NTITM of 'nt_item | `TMITM of 'tm_item ];
  tm_dot_i9      : 'tm_item -> int;
  sym_dot_i9     : 'sym_item -> int;
  sym_dot_j9     : 'sym_item -> int;
  nt_dot_i9      : 'nt_item -> int;
  nt_dot_j9      : 'nt_item -> int;
  with_j9        : 'nt_item -> int -> 'nt_item;
} constraint 'a = <
  nt         :'nt         ;
  tm         :'tm         ;
  sym        :'sym        ;
  tm_item    :'tm_item    ;
  sym_item   :'sym_item   ;
(*  pre_nt_item:'pre_nt_item; *)
  nt_item    :'nt_item    ;
  item       :'item       ;
>


type 'a myset = {
  add: 'elt -> 't -> 't;
  empty: 't;
  fold: 'a. ('elt -> 'a -> 'a) -> 't -> 'a -> 'a;
  is_empty: 't -> bool;
  mem: 'elt -> 't -> bool;
  elements: 't -> 'elt list
} constraint 'a = <
  elt:'elt;
  t: 't
>

type 'a ctxt_set = {
  set_nt_item: <elt:'nt_item; t:'set_nt_item> myset;
  set_item: <elt:'item; t:'set_item> myset;
  set_sym_item: <elt:'sym_item; t:'set_sym_item> myset;
  set_int: <elt:int; t:'set_int> myset;
  set_todo_done: <elt:'todo_done; t:'set_todo_done> myset;
} constraint 'a = <
  nt_item:'nt_item;
  set_nt_item: 'set_nt_item;
  item:'item;
  set_item: 'set_item;
  sym_item:'sym_item;
  set_sym_item: 'set_sym_item;
  set_int: 'set_int;
  todo_done: 'todo_done; (* nt_item+tm_item *)
  set_todo_done: 'set_todo_done;
>

(* a map with default values *)
type 'a mymap = {
  empty: 't;
  add: 'key -> 'value -> 't -> 't;
  find2 : 'key -> 't -> 'value;
} constraint 'a = <
  key: 'key;
  value: 'value;
  t: 't
>

type 'a ctxt_map = {
  map_blocked_key: <key:int*'sym;value:'set_nt_item;t:'map_blocked_key> mymap;
  map_complete_key: <key:int*'sym;value:'set_sym_item;t:'map_complete_key> mymap;
  map_sym_sym_int_int: <key:'sym*'sym*int*int;value:'set_int;t:'map_sym_sym_int_int> mymap;
  map_tm_int: <key:'tm*int;value:'set_int;t:'map_tm_int> mymap;
} constraint 'a = <
  sym:'sym;
  tm:'tm;
  set_nt_item:'set_nt_item;
  set_sym_item:'set_sym_item;
  set_int:'set_int;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int;  
  map_tm_int: 'map_tm_int;  
>


(* FIXME move to core types? *)
type ('string,'a,'s,'m) ty_ctxt = {
  (* g5:grammar; *)
  (* sym5:nt; (\* nt initial sym *\) *)
  string5: 'string;
  length5: int; (* length of string5 *)
  p_of_tm5:'tm -> 'string substring -> int list;
  item_ops5: 'a ty_ops;
  sets: 's ctxt_set;
  maps: 'm ctxt_map;
} constraint 'a = <
  nt         :'nt         ;
  tm         :'tm         ;
  sym        :'sym        ;
  tm_item    :'tm_item    ;
  sym_item   :'sym_item   ;
(*  pre_nt_item:'pre_nt_item; *)
  nt_item    :'nt_item    ;
  item       :'item       ;
> constraint 's = <
  nt_item:'nt_item;
  set_nt_item: 'set_nt_item;
  item:'item;
  set_item: 'set_item;
  sym_item:'sym_item;
  set_sym_item: 'set_sym_item;
  set_int: 'set_int;
  todo_done: 'todo_done; (* nt_item+tm_item *)
  set_todo_done: 'set_todo_done;
> constraint 'm = <
  sym:'sym;
  tm:'tm;
  set_nt_item:'set_nt_item;
  set_sym_item:'set_sym_item;
  set_int:'set_int;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int;  
  map_tm_int: 'map_tm_int;  
>


type 'a ty_loop2 = {
  todo_done5: 'set_todo_done;
  todo5: 'item list;
  oracle5: 'map_sym_sym_int_int;
  tmoracle5: 'map_tm_int;
  blocked5: 'map_blocked_key;
  complete5: 'map_complete_key;
} constraint 'a = <
  item: 'item;
  set_todo_done: 'set_todo_done;
  map_blocked_key: 'map_blocked_key;  
  map_complete_key: 'map_complete_key;  
  map_sym_sym_int_int: 'map_sym_sym_int_int;  
  map_tm_int: 'map_tm_int;  
>

(* following for type checking only *)

module type EARLEY_X_TYPES = sig
  
  include EARLEY_TYPES
  
  type string

  type todo_done = item
  
  type set_int
  type set_item
  type set_nt_item
  type set_sym_item
  type set_todo_done
  
  type map_blocked_key
  type map_complete_key
  type map_sym_sym_int_int
  type map_tm_int
  
  type ty_x_ctxt = (string,'a,'s,'m) ty_ctxt 
  constraint 'a = <
    nt         :nt         ;
    tm         :tm         ;
    sym        :sym        ;
    tm_item    :tm_item    ;
    sym_item   :sym_item   ;
(*    pre_nt_item:pre_nt_item; *)
    nt_item    :nt_item    ;
    item       :item       ;
  > constraint 's = <
    nt_item:nt_item;
    set_nt_item: set_nt_item;
    item:item;
    set_item: set_item;
    sym_item:sym_item;
    set_sym_item: set_sym_item;
    set_int: set_int;
    todo_done: todo_done; (* nt_item+tm_item *)
    set_todo_done: set_todo_done;
  > constraint 'm = <
    sym:sym;
    tm:tm;
    set_nt_item:set_nt_item;
    set_sym_item:set_sym_item;
    set_int:set_int;
    map_blocked_key: map_blocked_key;  
    map_complete_key: map_complete_key;  
    map_sym_sym_int_int: map_sym_sym_int_int;  
    map_tm_int: map_tm_int;  
  >

  val x_ctxt: ty_x_ctxt

  type ty_x_loop2 = 'a ty_loop2 
constraint 'a = <
  item: item;
  set_todo_done: set_todo_done;
  map_blocked_key: map_blocked_key;  
  map_complete_key: map_complete_key;  
  map_sym_sym_int_int: map_sym_sym_int_int;  
  map_tm_int: map_tm_int;  
>

end


module YY : EARLEY_X_TYPES = (val (Obj.magic 0) : EARLEY_X_TYPES)


(* for this to have the meaning we think it does, we must be working with binarized grammars - see note 2014-01-10; still safe to process if |b2| >=1 *)
let update_oracle ctxt m (itm,l) = (
  let ops = ctxt.item_ops5 in
  if (not (ops.a2_length_1 itm)) || ops.b2_nil itm then 
    m
  else (
    let (sym1,sym2) = (ops.hd_a2 itm,ops.hd_b2 itm) in
    let (i,k,j) = (ops.nt_dot_i9 itm,ops.nt_dot_j9 itm,l) in
    let key = (sym1,sym2,i,j) in
    let ks = ctxt.maps.map_sym_sym_int_int.find2 key m in
    let ks = ctxt.sets.set_int.add k ks in (* FIXME performance *)
    let m = ctxt.maps.map_sym_sym_int_int.add key ks m in
    m))

let (_:YY.ty_x_ctxt -> YY.map_sym_sym_int_int -> YY.nt_item * int -> YY.map_sym_sym_int_int) = update_oracle

(*  let update_oracle m (itm,l) = m *)

let update_tmoracle ctxt m (tm,i,j) = (
  let key = (tm,i) in
  let js = ctxt.maps.map_tm_int.find2 key m in
  let js = ctxt.sets.set_int.add j js in
  let m = ctxt.maps.map_tm_int.add key js m in
  m)

let (_:YY.ty_x_ctxt -> YY.map_tm_int -> YY.tm * int * int -> YY.map_tm_int) = update_tmoracle

(* let update_tmoracle m (tm,i,j) = m *)

let todo_is_empty s0 = (s0.todo5=[])

let add_todo ctxt s0 itm = { s0 with 
  todo5=(itm::s0.todo5); 
  todo_done5=(ctxt.sets.set_todo_done.add itm s0.todo_done5) }

let _ = (fun () -> add_todo YY.x_ctxt)

let pop_todo s0 = (
  match s0.todo5 with
  | [] -> (failwith "pop_todo")
  | x::xs -> ({s0 with todo5=xs},x))

(* bitm is an nt_item *)
(* O(ln n) *)
let cut ctxt bitm citm s0 = (
  let ops = ctxt.item_ops5 in
  let nitm = ops.mk_item (`NTITM ((ops.shift_a2_b2_c2 bitm) |> (fun x -> ops.with_j9 x (ops.sym_dot_j9 citm)))) in
  let s0 = (
    (* if this could be made O(1) our implementation would be O(n^3) overall *)
    if (ctxt.sets.set_todo_done.mem nitm s0.todo_done5) then
      s0
    else
      add_todo ctxt s0 nitm)
  in
  s0)

let _ = (fun () -> cut YY.x_ctxt)

let loop2 ctxt s0 = (
  let ops = ctxt.item_ops5 in
  let (s0,itm0) = pop_todo s0 in
  (* FIXME add a case construct rather than dests *)
  match ops.dest_item itm0 with
  | `NTITM nitm -> (
    let complete = ops.b2_nil nitm in
    match complete with
    | true -> (
      let citm = ops.mk_sym_coord (ops.nt2 nitm, ops.nt_dot_i9 nitm, ops.nt_dot_j9 nitm) in
      let k = (ops.sym_dot_i9 citm (* FIXME could be from dot_i9 nitm *),ops.sym6 citm) in
      (* FIXME check whether citm has already been done? *)
      let citms = ctxt.maps.map_complete_key.find2 k s0.complete5 in 
      match false (* ctxt.sets.set_sym_item.mem citm citms *) with (* FIXME this optimization didn't buy much *)
      | true -> s0
      | false -> (
        let bitms = ctxt.maps.map_blocked_key.find2 k s0.blocked5 in
        let f1 bitm s1 = cut ctxt bitm citm s1 in
        (* O(n. ln n) *)
        let s0 = ctxt.sets.set_nt_item.fold f1 bitms s0 in
        let s0 = {s0 with 
          complete5=(ctxt.maps.map_complete_key.add k (ctxt.sets.set_sym_item.add citm citms) s0.complete5)} 
        in
        (* we also update the oracle at this point; FIXME this appears very costly *)
        let f1 bitm o5 = update_oracle ctxt o5 (bitm,ops.sym_dot_j9 citm) in
        (* O(n. ln n) *)
        let s0 = {s0 with oracle5=(ctxt.sets.set_nt_item.fold f1 bitms s0.oracle5) } in
        s0))
    | false -> (
      let bitm = nitm in
      let sym = ops.hd_b2 bitm in
      let k = (ops.nt_dot_j9 bitm,sym) in
      let bitms = ctxt.maps.map_blocked_key.find2 k s0.blocked5 in
      let s0 = {s0 with
        blocked5=(ctxt.maps.map_blocked_key.add k (ctxt.sets.set_nt_item.add bitm bitms) s0.blocked5)}
      in
      (* we should also process the blocked item against the relevant complete items *)
      let citms = ctxt.maps.map_complete_key.find2 k s0.complete5 in
      let f1 citm s1 = cut ctxt bitm citm s1 in
      (* O(n. ln n) *)
      let s0 = ctxt.sets.set_sym_item.fold f1 citms s0 in
      (* we also update the oracle at this point; FIXME this appears very costly *)
      let f1 citm o5 = update_oracle ctxt o5 (bitm,ops.sym_dot_j9 citm) in
      (* O(n. ln n) *)
      let s0 = {s0 with oracle5=(ctxt.sets.set_sym_item.fold f1 citms s0.oracle5) } in
      match ops.sym_case sym with
      | `NT nt -> (
        let rs = ops.nt_items_for_nt nt (ops.nt_dot_j9 nitm) in
        let f1 s1 pnitm = (
          let nitm = ops.mk_item (`NTITM pnitm) in
          if (ctxt.sets.set_todo_done.mem nitm s1.todo_done5) then s1 else
            add_todo ctxt s1 nitm)
        in
        let s0 = List.fold_left f1 s0 rs in
        s0)
      | `TM tm -> (
        let titm = ops.mk_item(`TMITM(ops.mk_tm_coord (tm,ops.nt_dot_j9 nitm))) in
        if (ctxt.sets.set_todo_done.mem titm s0.todo_done5) then s0 else 
          add_todo ctxt s0 titm)
	    ))
  | `TMITM titm -> (
    let tm = ops.tm5 titm in
    let p = ctxt.p_of_tm5 tm in
    let rs = p (ctxt.string5,ops.tm_dot_i9 titm,ctxt.length5) in 
    let sym = ops.sym_of_tm tm in
    let k = (ops.tm_dot_i9 titm,sym) in
    (* lots of new complete items, so complete5 must be updated, but we must also process blocked *)
    let bitms = ctxt.maps.map_blocked_key.find2 k s0.blocked5 in
    (* the following is intended to avoid repeated looking up of complete set cs in s0 *)
    let f2 (cs,s1) j = (
      let i = ops.tm_dot_i9 titm in 
      let citm = ops.mk_sym_coord (sym,i,j) in
      let cs = ctxt.sets.set_sym_item.add citm cs in
      let f1 bitm s1 = cut ctxt bitm citm s1 in
      (* O(n. ln n) *)
      let s1 = ctxt.sets.set_nt_item.fold f1 bitms s1 in
      (* we also update the oracle at this point *)
      let f1 bitm o5 = update_oracle ctxt o5 (bitm,ops.sym_dot_j9 citm) in
      (* O(n. ln n) *)
      let s1 = {s1 with oracle5=(ctxt.sets.set_nt_item.fold f1 bitms s1.oracle5) } in
      (* and the tmoracle *)
      let s1 = {s1 with tmoracle5=(update_tmoracle ctxt s1.tmoracle5 (tm,i,j)) } in
      (cs,s1))
    in
    let cs = ctxt.maps.map_complete_key.find2 k s0.complete5 in
    let (cs,s0) = List.fold_left f2 (cs,s0) rs in
    let s0 = {s0 with complete5=(ctxt.maps.map_complete_key.add k cs s0.complete5)} in
    s0))

let (_:unit -> YY.ty_x_loop2 -> YY.ty_x_loop2) = (fun () -> loop2 YY.x_ctxt)

(* if porting to an imperative language, use a while loop for the following *)
let rec earley ctxt s0 = (if todo_is_empty s0 then s0 else (earley ctxt (loop2 ctxt s0)))

