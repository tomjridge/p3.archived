(* a standard interface to e3_core *)

open E3_core

type nt = int (* assumed even *)
type tm = int (* odd *)
type sym = int
type nt_item = nt * sym list * sym list * int * int
type tm_item = tm * int
type sym_item = sym * int * int
type item = [ `NTITM of nt_item | `TMITM of tm_item ]

let mk_ops nt_items_for_nt = (
  let id = fun x -> x in
  {
    sym_case       =(fun x -> if x mod 2 = 0 then `NT x else `TM x);
    sym_of_tm      =id;
    mk_tm_coord    =id;
    tm5            =(fun (tm,i) -> tm);
    mk_sym_coord   =id;
    sym6           =(fun (sym,i,j) -> sym);
    nt2            =(fun (nt,_,_,_,_) -> nt);
    shift_a2_b2_c2 =(fun (nt,_as,b::bs,i,j) -> (nt,b::_as,bs,i,j));
    a2_length_1    =(fun (nt,_as,_,_,_) -> match _as with [x] -> true | _ -> false);
    b2_nil         =(fun (nt,_,bs,_,_) -> match bs with [] -> true | _ -> false);
    hd_a2          =(fun (_,a::_,_,_,_) -> a);
    hd_b2          =(fun (_,_,b::_,_,_) -> b);
    nt_items_for_nt=nt_items_for_nt;
    mk_item        =id;
    dest_item      =id;
    tm_dot_i9      =(fun (tm,i) -> i);
    sym_dot_i9     =(fun (sym,i,j) -> i);
    sym_dot_j9     =(fun (sym,i,j) -> j);
    nt_dot_i9      =(fun (nt,_,_,i,j) -> i);
    nt_dot_j9      =(fun (nt,_,_,i,j) -> j);
    with_j9        =(fun (nt,_as,bs,i,_) -> fun j -> (nt,_as,bs,i,j));
  })

type symbol = sym
type nonterm = nt
type term = tm

type rhs = symbol list
type parse_rule = nonterm * rhs
type grammar = parse_rule list

type 'a ty_setup = <
    std_g      : grammar;
    std_sym    : nonterm;
    std_p_of_tm: (term -> ('a substring -> int list)) ; (* make sure this is efficient! *)
    std_string : 'a; (* int arg is length of 'a; assumed to start from 0 *)
    std_length : int;
  >

type ty_oracle = (symbol*symbol) -> (int*int) -> int list
type ty_tmoracle = term -> int * int -> bool 
type ty_result = <
  oracle: ty_oracle;
  tmoracle: ty_tmoracle
>

(* sets and maps *)
open E3_mods

let compare_i x1 y1 = (x1 - y1)

let compare_ii (x1,x2) (y1,y2) = (
  let x = x1 - y1 in
  if x<>0 then x else
    x2-y2)
  
let compare_iii (x1,x2,x3) (y1,y2,y3) = (
  let x = x1 - y1 in
  if x<>0 then x else
    let x=x2 - y2 in
    if x<>0 then x else
      x3 - y3)

let compare_iiii (x1,x2,x3,x4) (y1,y2,y3,y4) = (
  let x = x1 - y1 in
  if x<>0 then x else
    let x=x2 - y2 in
    if x<>0 then x else
      let x=x3 - y3 in
      if x<>0 then x else
        x4 - y4)

let compare_nt_item (nt,_as,bs,i,j) (nt',_as',bs',i',j') = (
  let x = compare_iii (nt,i,j) (nt',i',j') in
  if x<>0 then x else
    Pervasives.compare (_as,bs) (_as',bs'))

let compare_item i1 i2 = (
  match (i1,i2) with
  | (`TMITM _, `NTITM _) -> -1
  | (`NTITM _, `TMITM _) -> 1
  | (`TMITM x, `TMITM y) -> (compare_ii x y)
  | (`NTITM x, `NTITM y) -> (compare_nt_item x y))


module Sets_maps = (struct

  module Set_nt_item = MySet_Make(
    struct
      type t = nt_item
      let compare : t -> t -> int = compare_nt_item
    end)
  
  module Set_item = MySet_Make(
    struct
      type t = item
      let compare : t -> t -> int = compare_item
    end)

  type sym_coord = sym_item
  
  module Set_sym_coord = MySet_Make(
    struct
      type t = sym_coord
      let compare : t -> t -> int = compare_iii
    end)
  
  (* an alternative version of set_item *)
  module Set_todo_done_item = Set_item

  let set_nt_item = let open Set_nt_item in {
    add=add;
    empty=empty;
    fold=fold;
    is_empty=is_empty;
    mem=mem;
    elements=elements;
  }

  let set_item = let open Set_item in {
    add=add;
    empty=empty;
    fold=fold;
    is_empty=is_empty;
    mem=mem;
    elements=elements;
  }

  let set_sym_item = let open Set_sym_coord in {
    add=add;
    empty=empty;
    fold=fold;
    is_empty=is_empty;
    mem=mem;
    elements=elements;
  }

  let sets = {
    set_nt_item; set_item; set_sym_item; set_int; set_todo_done=set_item;
  }
  
  module Map_blocked_key = MyMap(
    struct
      type key = int * sym
      type value = Set_nt_item.t
      let compare : key -> key -> int = compare_ii
      let default = Set_nt_item.empty
    end)
  
  module Map_complete_key = MyMap(
    struct
      type key = int * sym
      type value = Set_sym_coord.t
      let compare : key -> key -> int = compare_ii
      let default = Set_sym_coord.empty
    end)
  
  (* oracle implementation type *)
  module Map_sym_sym_int_int = MyMap(
    struct
      type key = sym * sym * int * int
      type value = Set_int.t
      let compare : key -> key -> int = compare_iiii
      let default = Set_int.empty
    end)
  
  (* tmoracle impl type *)
  module Map_tm_int = MyMap(
    struct
      type key = tm * int
      type value = Set_int.t
      let compare : key -> key -> int = compare_ii
      let default = Set_int.empty
    end)

  let map_blocked_key = let open Map_blocked_key in {
    empty=empty;
    add=add;
    find2=find2;
  }
  let map_complete_key = let open Map_complete_key in {
    empty=empty;
    add=add;
    find2=find2;
  }

  let map_sym_sym_int_int = let open Map_sym_sym_int_int in {
    empty=empty;
    add=add;
    find2=find2;
  }

  let map_tm_int = let open Map_tm_int in {
    empty=empty;
    add=add;
    find2=find2;
  }

  let maps = {
    map_blocked_key; map_complete_key; map_sym_sym_int_int; map_tm_int;
  }

end)


module Sets_maps_imp = (struct

  module Set_item = Sets_maps.Set_item

  (* alternative imperative version *)
  module Set_todo_done_item = struct
  
    module Set_item = Set_item
  
    type t = Set_item.t array array
  
    let empty n = (
      (* let _ = Printf.printf "Empty %i" n in *)
      Array.make_matrix n n Set_item.empty) 
  
    let ij_of x = (match x with
      | `TMITM (tm,i) -> (i,i)
      | `NTITM (nt,_as,bs,i,j) -> (i,j))
  
    let mem x a2 = (
      let (i,j) = ij_of x in
      let a1 = a2.(i) in
      let ss = a1.(j) in
      Set_item.mem x ss)
  
    let add x a2 = (
      let (i,j) = ij_of x in
      (* let _ = Printf.printf "%i %i\n" i j in *)
      let a1 = a2.(i) in
      let ss = a1.(j) in
      let ss = Set_item.add x ss in
      let _ = a1.(j) <- ss in
      a2)
  
    let elements xs = [] (* FIXME *)

    let fold x = failwith "universe: 1514"

    let is_empty x = failwith "universe: 1516"
  
  end

  let set_todo_done n = let open Set_todo_done_item in {
    add=add;
    empty=empty n;
    fold=fold;
    is_empty=is_empty;
    mem=mem;
    elements=elements;
  }

  let sets len = (
    let set_nt_item = Sets_maps.set_nt_item in
    let set_item = Sets_maps.set_item in
    let set_sym_item = Sets_maps.set_sym_item in
    let set_todo_done = set_todo_done len in
    { set_nt_item; set_item; set_sym_item; set_int; set_todo_done=set_todo_done })

  (* alternative implementation of Map_sym_sym_int_int *)
  module Map_sym_sym = MyMap(
    struct
      type key = sym * sym
      type value = Set_int.t
      let compare = compare_ii
      let default = Set_int.empty
    end)
  
  module Map_sym_sym_int_int = (
    struct
      
      type ty_map = Map_sym_sym.ty_map array array
  
      (* m is of type Map_int_2.ty_map; k is a set of int *)
      let add k v a2 = (
        let (s1,s2,x,y) = k in
        let a1 = a2.(x) in
        let mss = a1.(y) in
        let mss = Map_sym_sym.add (s1,s2) v mss in
        let _ = a1.(y) <- mss in
        a2)
  
      let find2 k a2 = (
        let (s1,s2,x,y) = k in
        let a1 = a2.(x) in
        let mss = a1.(y) in
        let ks = Map_sym_sym.find2 (s1,s2) mss in
        ks)
  
      let empty n = (Array.make_matrix n n Map_sym_sym.empty)
  
    end)

  let map_sym_sym_int_int n = let open Map_sym_sym_int_int in {
    empty=empty n;
    add=add;
    find2=find2;
  }

  module Set_nt_item = Sets_maps.Set_nt_item

  module Map_sym_nt_item = MyMap(
    struct
      type key = sym
      type value = Set_nt_item.t
      let compare = compare_i
      let default = Set_nt_item.empty
    end)

  module Map_blocked_key = (
    struct
      type key = int * sym
      type value = Set_nt_item.t
      type ty_map = Map_sym_nt_item.ty_map array

      let add k v arr = (
        let (i,sym1) = k in
        let m1 = arr.(i) in
        let m1' = Map_sym_nt_item.add sym1 v m1 in
        let _ = arr.(i) <- m1' in
        arr)

      let find2 k arr = (
        let (i,sym1) = k in
        let m1 = arr.(i) in
        let (set1:Set_nt_item.t) = Map_sym_nt_item.find2 sym1 m1 in
        set1)

      let empty n = (Array.make n Map_sym_nt_item.empty)

    end)

  let map_blocked_key n = let open Map_blocked_key in {
    empty=empty n;
    add=add;
    find2=find2;
  }

  let (_:int -> <key:int*sym; value:Set_nt_item.t; t:Map_sym_nt_item.ty_map array> mymap) = map_blocked_key

  module Set_sym_coord = Sets_maps.Set_sym_coord

  module Map_sym_sym_coord = MyMap(
    struct
      type key = sym
      type value = Set_sym_coord.t
      let compare = compare_i
      let default = Set_sym_coord.empty
    end)

  module Map_complete_key = (
    struct
      type key = int * sym
      type value = Set_sym_coord.t
      type ty_map = Map_sym_sym_coord.ty_map array

      let add k v arr = (
        let (i,sym1) = k in
        let m1 = arr.(i) in
        let m1' = Map_sym_sym_coord.add sym1 v m1 in
        let _ = arr.(i) <- m1' in
        arr)

      let find2 k arr = (
        let (i,sym1) = k in
        let m1 = arr.(i) in
        let set1 = Map_sym_sym_coord.find2 sym1 m1 in
        set1)

      let empty n = (Array.make n Map_sym_sym_coord.empty)

    end)

  let map_complete_key n = let open Map_complete_key in {
    empty=empty n;
    add=add;
    find2=find2;
  }

  let maps n = (
    let map_blocked_key = map_blocked_key in
    let map_complete_key = map_complete_key in
    let map_sym_sym_int_int = map_sym_sym_int_int in
    let map_tm_int = Sets_maps.map_tm_int in
    { map_blocked_key=(map_blocked_key n); map_complete_key=(map_complete_key n); map_sym_sym_int_int=(map_sym_sym_int_int n); map_tm_int })


end)




let mk_init_loop2 ctxt init_items = (
  let sets = ctxt.sets in
  let maps = ctxt.maps in
  let s0 = {
    todo_done5=(
      List.fold_left (fun s itm -> sets.set_todo_done.add itm s) sets.set_todo_done.empty init_items);
    todo5=(init_items);
    oracle5=maps.map_sym_sym_int_int.empty;
    tmoracle5=maps.map_tm_int.empty;
    blocked5=maps.map_blocked_key.empty;
    complete5=maps.map_complete_key.empty
  } in
  s0)

(*
let memo tbl f k = (
  if (Hashtbl.mem tbl k) then 
    (Hashtbl.find tbl k) 
  else
    let v = f k in
    let _ = Hashtbl.add tbl k v in
    v)
*)

let mk_nt_items_for_nt g = (
  let tbl = Hashtbl.create 100 in
  let f0 = (fun (nt,rhs) -> (nt,[],rhs,0,0)) in
  let f1 tbl (nt,rhs) = (
    let itms = try Hashtbl.find tbl nt with Not_found -> [] in
    let _ = Hashtbl.add tbl nt ((f0 (nt,rhs))::itms) in
    tbl)
  in
  let tbl = List.fold_left f1 tbl g in
  let f2 nt = try Hashtbl.find tbl nt with Not_found -> [] in
  fun nt i -> List.map (fun (nt,_as,bs,_,_) -> (nt,_as,bs,i,i)) (f2 nt))

(* FIXME duplicate defn *)
let memo tbl f i = (
  if (Hashtbl.mem tbl i) then
    (Hashtbl.find tbl i)
  else (
    let v = f i in
    let _ = Hashtbl.add tbl i v in
    v))

let post_process (s,ctxt) = (
  let open E3_core in
  (* oracle *)
  let o = s.oracle5 in
  let tbl = Hashtbl.create 100 in
  let f1 (sym1,sym2,i,j) = (
    ctxt.maps.map_sym_sym_int_int.find2 (sym1,sym2,i,j) o 
    |> ctxt.sets.set_int.elements)
  in 
  let o = fun (sym1,sym2) (i,j) -> memo tbl f1 (sym1,sym2,i,j) in
  (* tmoracle *)
  let tmo = s.tmoracle5 in
  let tmo tm (i,j) = ( (* FIXME are we sure this is as efficient as it can be? *)
    (tm,i) 
    |> (fun x -> ctxt.maps.map_tm_int.find2 x tmo)
    |> (fun x -> ctxt.sets.set_int.mem j x))
  in
  (* now construct the oracles *)
  object
    method oracle=o;
    method tmoracle=tmo;
  end)


let earley_full setup0 = (
  let nt_items_for_nt = (mk_nt_items_for_nt setup0#std_g) in
  let init_items = List.map (fun x -> `NTITM x) (nt_items_for_nt setup0#std_sym 0) in
  let len = setup0#std_length in
  let max_len_for_arrays=10000 in
  let functional_ctxt = { string5=setup0#std_string; length5=len; p_of_tm5=setup0#std_p_of_tm; item_ops5=mk_ops nt_items_for_nt; sets=Sets_maps.sets; maps=Sets_maps.maps } in
  let imperative_ctxt = { string5=setup0#std_string; length5=len; p_of_tm5=setup0#std_p_of_tm; item_ops5=mk_ops nt_items_for_nt; sets=Sets_maps_imp.sets (len+1); maps=Sets_maps_imp.maps (len+1) } in
  if len < max_len_for_arrays then (
    let ctxt =  imperative_ctxt in
    let init_state = mk_init_loop2 ctxt init_items in
    let s = E3_core.earley ctxt init_state in
    post_process (s,ctxt))
  else (
    let ctxt =  functional_ctxt in
    let init_state = mk_init_loop2 ctxt init_items in
    let s = E3_core.earley ctxt init_state in
    post_process (s,ctxt)))
  
