(* 

    #directory "../_build";;
    #load "p3.cma";;    

*)

(**
{1 earley3.ml}

{2 Earley types}

  * The `loop_2` type is the type of the state used when running the earley loop.
  * The `prod5` field is a set of `(nt_item,int)`. The `nt_item` is a blocked item; the int is the extent that the next symbol in the blocked item covered. For example, suppose we have the following member of `prod5`:
 
        ((Y -> alpha.Xbeta,i,j),k)

    This is supposed to indicate that the item in question was reachable, and that an item `(X -> ...,j,k)` was also reachable, and when these were combined, we got an item `(Y -> alphaX.beta,i,k)`


*)

(* let _ = Gc.set { (Gc.get()) with Gc.allocation_policy=1 } *)

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
  
end

(* FIXME rename to Earley_all_types since it includes public types *)
module Earley_private_types = struct

  include Earley_public_types
  
  (* FIXME move some of these types to private - we have the oracle now as the interface type *)
  type nt_item = { nt2: nonterm; a2: symbol list; (* c2:(int*int) list;*) b2: symbol list; i2:int; j2:int }
  type tm_item = { tm5:term; i5:int } 
  type c_item = { sym6: symbol; i6:int; j6: int }
  type sym_item = NTITM of nt_item | TMITM of tm_item (* these are items that can be further processed *)

  (* FIXME move to core types? *)
  type 'a ty_ctxt = {
    g5:grammar;
    sym5:nonterm; (* nonterm initial symbol *)
    (* caneps5:symbol -> bool; ( * may be dummy value, to be computed later FIXME don't need * ) *)
    string5:'a ;
    length5: int; (* length of string5 *)
    p_of_tm5:term -> 'a substring -> lc_substring list
  }
  
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
      let compare x y = Pervasives.compare x y
    end)

  module Set_sym_item = MySet_Make(
    struct
      type t = sym_item
      let compare x y = Pervasives.compare x y
    end)

  module Set_c_item = MySet_Make(
    struct
      type t = c_item
      let compare x y = Pervasives.compare x y
    end)

  module Set_int = MySet_Make(
    struct
      type t = int
      let compare x y = Pervasives.compare x y
    end)

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
  
  module Map_blocked_key = MyMap(
    struct
      type key = int * symbol
      type value = Set_nt_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)

  module Map_complete_key = MyMap(
    struct
      type key = int * symbol
      type value = Set_c_item.t
      let compare x y = Pervasives.compare x y
      let default = Set_c_item.empty
    end)
  
  (* oracle implementation type *)
  module Map_sym_sym_int_int_old = MyMap(
    struct
      type key = symbol * symbol * int * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

  (* alternative implementation of Map_sym_sym_int_int *)
  module Map_sym_sym = MyMap(
    struct
      type key = symbol * symbol
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

    
  module Map_int_1 = MyMap(
    struct
      type key = int 
      type value = Map_sym_sym.ty_map
      let compare x y = Pervasives.compare x y
      let default = Map_sym_sym.empty
    end)

  module Map_int_2 = MyMap(
    struct
      type key = int 
      type value = Map_int_1.ty_map
      let compare x y = Pervasives.compare x y
      let default = Map_int_1.empty
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

(*
  module Map_sym_sym_int_int = MyMap(
    struct
      type key = symbol * symbol * int * int
      type value = Set_int.t
      let compare x y = (
        let (x1,x2,x3,x4) = x in
        let (y1,y2,y3,y4) = y in
        Pervasives.compare (x3,x4,x1,x2) (y3,y4,y1,y2))
      let default = Set_int.empty
    end)
*)

  (* an alternative version of set_sym_item *)
  (*
  module Set_todo_done_item = MySet_Make(
    struct
      type t = sym_item
      let compare itm1 itm2 = (
        let f1 i = (i.i2,i.j2,i.nt2,i.a2,i.b2) in
        match (itm1,itm2) with
        | (NTITM ntitm1,NTITM ntitm2) -> (
          Pervasives.compare (f1 ntitm1) (f1 ntitm2))
        | (NTITM x, TMITM y) -> -1 (* NTITM < TMITM *)
        | (TMITM x, NTITM y) -> 1
        | (TMITM x, TMITM y) -> (Pervasives.compare x y))
    end)
  *)

  (* alternative imperative version *)

  module Set_todo_done_item = struct

    module Set_todo_done_item = MySet_Make(
      struct
        type t = sym_item
        let compare itm1 itm2 = (
          let f1 i = (i.i2,i.j2,i.nt2,i.a2,i.b2) in
          match (itm1,itm2) with
          | (NTITM ntitm1,NTITM ntitm2) -> (
            Pervasives.compare (f1 ntitm1) (f1 ntitm2))
          | (NTITM x, TMITM y) -> -1 (* NTITM < TMITM *)
          | (TMITM x, NTITM y) -> 1
          | (TMITM x, TMITM y) -> (Pervasives.compare x y))
      end)

    type t = Set_todo_done_item.t array array

    let empty n = (Array.make_matrix n n Set_todo_done_item.empty) 

    let ij_of x = (match x with
      | NTITM x -> (x.i2,x.j2)
      | TMITM x -> (x.i5,x.i5))

    let mem x a2 = (
      let (i,j) = ij_of x in
      let a1 = a2.(i) in
      let ss = a1.(j) in
      Set_todo_done_item.mem x ss)

    let add x a2 = (
      let (i,j) = ij_of x in
      let a1 = a2.(i) in
      let ss = a1.(j) in
      let ss = Set_todo_done_item.add x ss in
      let _ = a1.(j) <- ss in
      a2)

    let elements xs = [] (* FIXME *)


  end


(*
  module Set_todo_done_item = MySet_Make(
    struct
      type t = sym_item
      let compare itm1 itm2 = (
        let f1 i = (i.i2,i.j2,i.nt2,i.a2,i.b2) in
        match (itm1,itm2) with
        | (NTITM ntitm1,NTITM ntitm2) -> (
          Pervasives.compare (f1 ntitm1) (f1 ntitm2))
        | (NTITM x, TMITM y) -> -1 (* NTITM < TMITM *)
        | (TMITM x, NTITM y) -> 1
        | (TMITM x, TMITM y) -> (Pervasives.compare x y))
    end)
*)

  (* an alternative version of Set_todo_done_item using maps; performance is worse! *)
  (*
  module Set_todo_done_item = struct 

    module Set_todo_done_item = MySet_Make(
      struct
        type t = sym_item
        let compare itm1 itm2 = (
          let f1 i = (i.nt2,i.a2,i.b2) in (* NB i and j are always fixed, for each set *)
          match (itm1,itm2) with
          | (NTITM ntitm1,NTITM ntitm2) -> (
            Pervasives.compare (f1 ntitm1) (f1 ntitm2))
          | (NTITM x, TMITM y) -> -1 (* NTITM < TMITM *)
          | (TMITM x, NTITM y) -> 1
          | (TMITM x, TMITM y) -> (Pervasives.compare x y))
      end)
  
    module Map_int_1 = MyMap(
      struct
        type key = int 
        type value = Set_todo_done_item.t
        let compare x y = Pervasives.compare x y
        let default = Set_todo_done_item.empty
      end)
  
    module Map_int_2 = MyMap(
      struct
        type key = int 
        type value = Map_int_1.ty_map
        let compare x y = Pervasives.compare x y
        let default = Map_int_1.empty
      end)

    type t = Map_int_2.ty_map

    let empty = Map_int_2.empty

    let ij_of x = (match x with
      | NTITM x -> (x.i2,x.j2)
      | TMITM x -> (x.i5,x.i5))

    let mem x xs = (
      let (i,j) = ij_of x in
      let mi1 = Map_int_2.find2 i xs in
      let ss = Map_int_1.find2 j mi1 in
      Set_todo_done_item.mem x ss)

    let add x mi2 = (
      let (i,j) = ij_of x in
      let mi1 = Map_int_2.find2 i mi2 in
      let ss = Map_int_1.find2 j mi1 in
      let ss = Set_todo_done_item.add x ss in
      let mi1 = Map_int_1.add j ss mi1 in
      let mi2 = Map_int_2.add i mi1 mi2 in
      mi2)

    let elements xs = [] (* FIXME *)

  end
  *)



  (* tmoracle impl type *)
  module Map_term_int = MyMap(
    struct
      type key = term * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)

  type ty_loop2 = {
    todo_done5: Set_todo_done_item.t;
    todo5: sym_item list;
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

  (* used to export results *)
  type full_earley_data = [ `Loop2 of
    [`Done of Set_todo_done_item.t] *
    [`Oracle of ty_oracle * ty_tmoracle]]

end(* *)

(**
{2 Earley core}

See Earley types for an explanation of the `prod` field.

*)

module EarleyCore = struct
  
(*  open Types *)
  open Earley_private_types

  let rules_for_nt ctxt nt = (
    let rs = List.filter (fun (nt',rhs) -> nt' = nt) ctxt.g5 in
    rs)         
  
  let shift_a2_b2_c2 itm = {itm with
    a2=(List.hd itm.b2)::itm.a2;
    b2=(List.tl itm.b2)
  }


  let update_oracle m (itm,l) = (
    if not ((List.length itm.a2=1) && (List.length itm.b2=1)) then 
      (* FIXME should we fail? or process if length itm.b2 >= 1? *)
      m
    else (
      let (sym1,sym2) = (List.hd itm.a2,List.hd itm.b2) in
      let (i,k,j) = (itm.i2,itm.j2,l) in
(*      let _ = assert (i <=k && k <=j) in *)
      let key = (sym1,sym2,i,j) in
      let ks = Map_sym_sym_int_int.find2 key m in
      let ks = Set_int.add k ks in (* FIXME performance *)
      let m = Map_sym_sym_int_int.add key ks m in
      m))


(*  let update_oracle m (itm,l) = m *)


  let update_tmoracle m (tm,i,j) = (
    let key = (tm,i) in
    let js = Map_term_int.find2 key m in
    let js = Set_int.add j js in
    let m = Map_term_int.add key js m in
    m)

(*  let update_tmoracle m (tm,i,j) = m *)
        


  let todo_is_empty s0 = (s0.todo5=[])

  let add_todo s0 itm = { s0 with 
    todo5=(itm::s0.todo5); 
    todo_done5=(Set_todo_done_item.add itm s0.todo_done5) }

  let pop_todo s0 = (
    match s0.todo5 with
    | [] -> (failwith "pop_todo")
    | x::xs -> ({s0 with todo5=xs},x))

  (* bitm is an nt_item *)
  (* O(ln n) *)
  let cut bitm citm s0 = (
    let nitm = NTITM {(shift_a2_b2_c2 bitm) with j2=(citm.j6) } in
    let s0 = (
      (* if this could be made O(1) our implementation would be O(n^3) overall *)
      (* FIXME optimization: mem and add could be collapsed into single add if this returned whether the item was already present *)
      if (Set_todo_done_item.mem nitm s0.todo_done5) then
        s0
      else
        add_todo s0 nitm)
    in
    s0)

  let loop2 ctxt s0 = (
    let (s0,itm0) = pop_todo s0 in
    match itm0 with
    | NTITM nitm -> (
      let complete = (nitm.b2 = []) in
      match complete with
      | true -> (
        let citm = {sym6=(`NT nitm.nt2); i6=nitm.i2; j6=nitm.j2} in
        let k = (citm.i6,citm.sym6) in
        (* FIXME check whether citm has already been done? *)
        let citms = Map_complete_key.find2 k s0.complete5 in
        match Set_c_item.mem citm citms with
        | true -> s0
        | false -> (
          let bitms = Map_blocked_key.find2 k s0.blocked5 in
          let f1 bitm s1 = cut bitm citm s1 in
          (* O(n. ln n) *)
          let s0 = Set_nt_item.fold f1 bitms s0 in
          let s0 = {s0 with 
            complete5=(Map_complete_key.add k (Set_c_item.add citm citms) s0.complete5)} 
          in
          (* we also update the oracle at this point; FIXME this appears very costly *)
          let f1 bitm o5 = update_oracle o5 (bitm,citm.j6) in
          (* O(n. ln n) *)
          let s0 = {s0 with oracle5=(Set_nt_item.fold f1 bitms s0.oracle5) } in
          s0))
      | false -> (
        let bitm = nitm in
        let sym = List.hd bitm.b2 in
        let k = (bitm.j2,sym) in
        let bitms = Map_blocked_key.find2 k s0.blocked5 in
        let s0 = {s0 with
          blocked5=(Map_blocked_key.add k (Set_nt_item.add bitm bitms) s0.blocked5)}
        in
        (* we should also process the blocked item against the relevant complete items *)
        let citms = Map_complete_key.find2 k s0.complete5 in
        let f1 citm s1 = cut bitm citm s1 in
        (* O(n. ln n) *)
        let s0 = Set_c_item.fold f1 citms s0 in
        (* we also update the oracle at this point; FIXME this appears very costly *)
        let f1 citm o5 = update_oracle o5 (bitm,citm.j6) in
        (* O(n. ln n) *)
        let s0 = {s0 with oracle5=(Set_c_item.fold f1 citms s0.oracle5) } in
        match sym with
        | `NT nt -> (
          let rs = rules_for_nt ctxt nt in
          let f1 s1 (_,alt) = (
            let nitm = NTITM {nt2=nt; a2=[]; b2=alt; i2=nitm.j2; j2=nitm.j2} in
            if (Set_todo_done_item.mem nitm s1.todo_done5) then s1 else
              add_todo s1 nitm)
          in
          let s0 = List.fold_left f1 s0 rs in
          s0)
        | `TM tm -> (
          let titm = TMITM({tm5=tm;i5=nitm.j2}) in
          if (Set_todo_done_item.mem titm s0.todo_done5) then s0 else 
            add_todo s0 titm)))
    | TMITM titm -> (
      let tm = titm.tm5 in
      let p = ctxt.p_of_tm5 tm in
      let rs = p (ctxt.string5,titm.i5,ctxt.length5) in 
      let sym = `TM tm in
      let k = (titm.i5,sym) in
      (* lots of new complete items, so complete5 must be updated, but we must also process blocked *)
      let bitms = Map_blocked_key.find2 k s0.blocked5 in
      let f2 (cs,s1) (i,j) = (
        let citm = { sym6=sym; i6=i; j6=j } in
        let cs = Set_c_item.add citm cs in
        let f1 bitm s1 = cut bitm citm s1 in
        (* O(n. ln n) *)
        let s1 = Set_nt_item.fold f1 bitms s1 in
        (* we also update the oracle at this point *)
        let f1 bitm o5 = update_oracle o5 (bitm,citm.j6) in
        (* O(n. ln n) *)
        let s1 = {s1 with oracle5=(Set_nt_item.fold f1 bitms s1.oracle5) } in
        (* and the tmoracle *)
        let s1 = {s1 with tmoracle5=(update_tmoracle s1.tmoracle5 (tm,i,j)) } in
        (cs,s1))
      in
      let cs = Map_complete_key.find2 k s0.complete5 in
      let (cs,s0) = List.fold_left f2 (cs,s0) rs in
      let s0 = {s0 with complete5=(Map_complete_key.add k cs s0.complete5)} in
      s0))

  
  let rec earley ctxt s0 = (if todo_is_empty s0 then s0 else (earley ctxt (loop2 ctxt s0)))

  let earley_initial_state ctxt0 = ({
      todo_done5=(Set_todo_done_item.empty (ctxt0.length5+1) );
      todo5=(
        let initial_itm = (NTITM{nt2=ctxt0.sym5;a2=[];b2=[`NT ctxt0.sym5];i2=0;j2=0}) in
        [initial_itm]);
      oracle5=Map_sym_sym_int_int.empty (ctxt0.length5+1);
      tmoracle5=Map_term_int.empty;
      blocked5=Map_blocked_key.empty;
      complete5=Map_complete_key.empty
    })

(*

let g = [
   ("E",[`NT "E";`NT "E";`NT "E"]);
   ("E",[`TM "\"1\""]);
   ("E",[`TM "\"\""])]

let txt = "111"

let start_nt = "E"

let setup0 = `Setup(
   `G7(g),
   `Sym7(start_nt),
   `P_of_tm7(P1_lib.P1_terminal_parsers.RawParsers.term_to_parser),
   `String7(txt,String.length txt))

let ctxt = (match setup0 with
    | `Setup(`G7(g),`Sym7(sym),`P_of_tm7(p_of_tm),`String7(s,len)) -> (
      let ctxt0 = {
        g5=g;
        sym5=sym;
        string5=s;
        length5=len;
        p_of_tm5=(earley_term_to_parser p_of_tm);
      } 
      in
      ctxt0))

let s0 = earley_initial_state ctxt
let s0 = loop2 ctxt s0
let _ = Set_sym_item.elements s0.todo5

*)

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

  (* FIXME memoize exported oracles *)
  let export_oracle oracle = (
    let tbl = Hashtbl.create 100 in
    let f1 = (fun (sym1,sym2,i,j) -> 
      let rs = Map_sym_sym_int_int.find2 (sym1,sym2,i,j) oracle in
      Set_int.elements rs)
    in
    (fun (sym1,sym2) -> fun (i,j) -> memo tbl f1 (sym1,sym2,i,j)))

  let export_tmoracle tmoracle = (
    let tbl = Hashtbl.create 100 in 
    let f1 (tm,i,j) = Set_int.mem j (Map_term_int.find2 (tm,i) tmoracle) in
    (fun tm -> fun (i,j) -> memo tbl f1 (tm,i,j)))
  
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
      `Loop2(`Done(s1.todo_done5),`Oracle(export_oracle s1.oracle5,export_tmoracle s1.tmoracle5))))
  let (_:'a ty_setup -> full_earley_data) = earley_full

  let earley_main setup0 = (
    let r = earley_full setup0 in 
    match r with
    | `Loop2(`Done(itms),_) -> itms)
  let (_:'a ty_setup -> Set_todo_done_item.t) = earley_main
  
end


(**
{2 Earley debug}

Some debugging functions. Work with the last earley state that was found.

*)
module Earley_debug = struct

(*  open Prelude
  open Types *)
  open Earley_private_types
  let j_of_itm itm = (
    match itm with
    | NTITM itm -> itm.j2
    | TMITM itm -> itm.i5) (* FIXME? only used in debugging but now we don't have this info *)

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
    let f1 acc itms = (Set_c_item.elements itms)@acc in
    let cs = List.fold_left f1 [] cs in
    let f1 m itm = if itm.j6 > m then itm.j6 else m in
    let m = List.fold_left f1 0 cs in
    m)

end

