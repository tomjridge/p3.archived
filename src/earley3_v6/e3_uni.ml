(*
#mod_use "e3_core.ml";;
*)

(*

Description of various "universes":

  * U0: nts and tms are strings

  * U1: nts and tms are ints (enumerate U0 nts and tms)

  * U2: pre_nt_items are ints (enumerate all possible X->alpha.beta)

  * U3: as U2, no datatype wrappers

  * U4: function-optimized U3, no set and map optimization

  * U4_imp: as U4, some imperative sets and maps

  * U5: items represented using ints (64 bits required, limits on length of input, and number of pre_nt_items)

  * U6: function-optimized U5, no set and map optimization

  * U7: as U6, sets and maps optimized using imperative arrays

  * U8: for timing purposes, partially omits oracle construction

*)


open E3_core (* for record fields add etc *)

open E3_mods


let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b);;

(* sets are not ordered - so can't use naive equality on sets - but are without duplicates *)
let finset_insert x l =
  if List.mem x l then l else x::l;;

let finset_union l1 l2 = itlist finset_insert l1 l2;;

let finset_unions l = itlist finset_union l [];;



module YY1 : EARLEY_X_TYPES = (val (Obj.magic 0) : EARLEY_X_TYPES)
module YY2 : EARLEY_X_TYPES = (val (Obj.magic 0) : EARLEY_X_TYPES)


type 'a full_methods = 'a E3_core.ty_ops

(*
type 'a base_methods = {
  sym_nt: 'u0_nt -> 'u0_sym
  ;sym_tm: 'u0_tm -> 'u0_sym
  ;sym_case: 'u0_sym -> [ `NT of 'u0_nt | `TM of 'u0_tm ]
}
constraint 'a = <
  nt         :'u0_nt         ;
  tm         :'u0_tm         ;
  sym        :'u0_sym        ;
  tm_item    :'u0_tm_item    ;
  sym_item   :'u0_sym_item   ;
  nt_item    :'u0_nt_item    ;
  item       :'u0_item       ;
>
*)

(* basic isomorphisms between types *)
type ('a,'b) iso = {
  nt1_of_2       : 'u2_nt -> 'u1_nt            
  ;nt2_of_1      : 'u1_nt -> 'u2_nt            
  ;tm1_of_2      : 'u2_tm -> 'u1_tm            
  ;tm2_of_1      : 'u1_tm -> 'u2_tm            
  ;sym1_of_2     : 'u2_sym -> 'u1_sym          
  ;sym2_of_1     : 'u1_sym -> 'u2_sym          
  ;tm_item1_of_2 : 'u2_tm_item -> 'u1_tm_item  
  ;tm_item2_of_1 : 'u1_tm_item -> 'u2_tm_item  
  ;sym_item1_of_2: 'u2_sym_item -> 'u1_sym_item
  ;sym_item2_of_1: 'u1_sym_item -> 'u2_sym_item
  ;nt_item1_of_2 : 'u2_nt_item -> 'u1_nt_item  
  ;nt_item2_of_1 : 'u1_nt_item -> 'u2_nt_item  
  ;item1_of_2    : 'u2_item -> 'u1_item  
  ;item2_of_1    : 'u1_item -> 'u2_item  
}
constraint 'a = <
  nt         :'u1_nt         ;
  tm         :'u1_tm         ;
  sym        :'u1_sym        ;
  tm_item    :'u1_tm_item    ;
  sym_item   :'u1_sym_item   ;
  nt_item    :'u1_nt_item    ;
  item       :'u1_item       ;
>
constraint 'b = <
  nt         :'u2_nt         ;
  tm         :'u2_tm         ;
  sym        :'u2_sym        ;
  tm_item    :'u2_tm_item    ;
  sym_item   :'u2_sym_item   ;
  nt_item    :'u2_nt_item    ;
  item       :'u2_item       ;
>


let mk_ops2 (ops1: 'a full_methods) (map12: ('a,'b)iso) = (
  let x_iso = map12 in
  let sym_case = (fun sym -> sym |> map12.sym1_of_2 |> ops1.sym_case |> (fun x -> match x with
      | `NT x -> `NT (map12.nt2_of_1 x)
      | `TM x -> `TM (map12.tm2_of_1 x)))
  in
  let sym_of_tm = (fun tm -> tm |> map12.tm1_of_2 |> ops1.sym_of_tm |> map12.sym2_of_1) in
  let mk_tm_coord = (fun (tm,i) -> (map12.tm1_of_2 tm,i) |> ops1.mk_tm_coord |> map12.tm_item2_of_1) in  
  let tm5=(fun x -> x |> x_iso.tm_item1_of_2 |> ops1.tm5 |> x_iso.tm2_of_1) in
  let mk_sym_coord = (fun (sym,i,j) -> (x_iso.sym1_of_2 sym,i,j) |> ops1.mk_sym_coord |> x_iso.sym_item2_of_1) in
  let sym6=(fun x -> x |> x_iso.sym_item1_of_2 |> ops1.sym6 |> x_iso.sym2_of_1) in
  let nt2=(fun x -> x |> x_iso.nt_item1_of_2 |> ops1.nt2 |> x_iso.sym2_of_1) in
  let shift_a2_b2_c2=(fun x -> x |> x_iso.nt_item1_of_2 |> ops1.shift_a2_b2_c2 |> x_iso.nt_item2_of_1) in
  let a2_length_1=(fun x-> x |> x_iso.nt_item1_of_2 |> ops1.a2_length_1) in
  let b2_nil=(fun x -> x |> x_iso.nt_item1_of_2 |> ops1.b2_nil) in
  let hd_a2=(fun x ->  x |> x_iso.nt_item1_of_2 |> ops1.hd_a2 |> x_iso.sym2_of_1) in
  let hd_b2=(fun x ->  x |> x_iso.nt_item1_of_2 |> ops1.hd_b2 |> x_iso.sym2_of_1) in
  let nt_items_for_nt=(fun x i -> x |> x_iso.nt1_of_2 |> (fun x -> ops1.nt_items_for_nt x i) |> List.map x_iso.nt_item2_of_1) in
  let mk_item = (fun x -> match x with
    | `NTITM x -> (x |> x_iso.nt_item1_of_2 |> (fun x -> ops1.mk_item (`NTITM x)) |> x_iso.item2_of_1)
    | `TMITM x -> (x |> x_iso.tm_item1_of_2 |> (fun x -> ops1.mk_item (`TMITM x)) |> x_iso.item2_of_1))
  in
  let dest_item = (fun x -> x |> x_iso.item1_of_2 |> ops1.dest_item |> (fun x -> match x with
    | `NTITM x -> (x |> x_iso.nt_item2_of_1 |> (fun x -> `NTITM x))
    | `TMITM x -> (x |> x_iso.tm_item2_of_1 |> (fun x -> `TMITM x))))
  in
  let tm_dot_i9 = (fun x -> x |> x_iso.tm_item1_of_2 |> ops1.tm_dot_i9) in
  let sym_dot_i9 = (fun x -> x |> x_iso.sym_item1_of_2 |> ops1.sym_dot_i9) in
  let sym_dot_j9 = (fun x -> x |> x_iso.sym_item1_of_2 |> ops1.sym_dot_j9) in
  let nt_dot_i9 = (fun x -> x |> x_iso.nt_item1_of_2 |> ops1.nt_dot_i9) in
  let nt_dot_j9 = (fun x -> x |> x_iso.nt_item1_of_2 |> ops1.nt_dot_j9) in
  let with_j9 = (fun x j -> x |> x_iso.nt_item1_of_2 |> (fun x -> ops1.with_j9 x j) |> x_iso.nt_item2_of_1) in
  {
    sym_case       =sym_case       ;
    sym_of_tm      =sym_of_tm      ;
    mk_tm_coord    =mk_tm_coord    ;
    tm5            =tm5            ;
    mk_sym_coord   =mk_sym_coord   ;
    sym6           =sym6           ;
    nt2            =nt2            ;
    shift_a2_b2_c2 =shift_a2_b2_c2 ;
    a2_length_1    =a2_length_1    ;
    b2_nil         =b2_nil         ;
    hd_a2          =hd_a2          ;
    hd_b2          =hd_b2          ;
    nt_items_for_nt=nt_items_for_nt;
    mk_item        =mk_item        ;
    dest_item      =dest_item      ;
    tm_dot_i9      =tm_dot_i9      ;
    sym_dot_i9     =sym_dot_i9     ;
    sym_dot_j9     =sym_dot_j9     ;
    nt_dot_i9      =nt_dot_i9      ;
    nt_dot_j9      =nt_dot_j9      ;
    with_j9        =with_j9        ;
  })

let _ = fun () -> mk_ops2 (YY1.x_ctxt.item_ops5)

(* these are some extra operations necessary for initialization and mapping of results *)
let mk_ops2x ops1x map12 = (
  let init_items=(fun () -> () |> ops1x#init_items |> List.map map12.nt_item2_of_1) in
  let u0_tm=(fun x -> x |> map12.tm1_of_2 |> ops1x#u0_tm) in
  let sym_of_0=(fun x -> x |> ops1x#sym_of_0 |> map12.sym2_of_1) in
  let tm_of_0=(fun x -> x |> ops1x#tm_of_0 |> map12.tm2_of_1) in
  object
    method init_items=init_items;
    method u0_tm=u0_tm;
    method sym_of_0=sym_of_0;
    method tm_of_0=tm_of_0;
  end)


type 'a coord = { v8:'a; i8:int; j8:int }


(* nts and tms are strings *)
module U0 = struct

  type nt = U0_nt of string
  type tm = U0_tm of string
  type sym = U0_sym_nt of nt | U0_sym_tm of tm

  type tm_coord = tm coord
  type sym_coord = sym coord

  type pre_nt_item = U0_pre_nt_item of (nt * sym list * sym list)
  type nt_item = pre_nt_item coord

  (* type pre_item = U0_pre_item_nt of pre_nt_item | U0_pre_item_tm of tm *)

  type item  = U0_item_nt of nt_item | U0_item_tm of tm_coord

  type grammar = (nt*(sym list)) list

  let ops g = (
    let sym_case sym = (
      match sym with
      | U0_sym_nt nt -> `NT nt
      | U0_sym_tm tm -> `TM tm)
    in
    let sym_of_tm x = U0_sym_tm x in
    let sym_nt x = U0_sym_nt x in
    let mk_tm_coord (x,i) = {v8=x;i8=i;j8=i} in
    let tm5 x = x.v8 in
    let mk_sym_coord (x,i,j) = {v8=x;i8=i;j8=j} in
    let sym6 x = x.v8 in
    let nt2 x = (let U0_pre_nt_item(nt,_,_) = x.v8 in U0_sym_nt nt) in
    let shift_a2_b2_c2 x = {x with v8=(
      match x.v8 with U0_pre_nt_item(nt,xs,y::ys) -> U0_pre_nt_item(nt,y::xs,ys))}
    in
    let a2_length_1 x = (match x.v8 with
        U0_pre_nt_item(nt,xs,ys) -> List.length xs = 1)
    in
    let b2_nil x = (match x.v8 with
        U0_pre_nt_item(nt,xs,ys) -> ys=[])
    in
    let hd_a2 x = (match x.v8 with
        U0_pre_nt_item(nt,xs,ys) -> List.hd xs)
    in
    let hd_b2 x = (match x.v8 with
        U0_pre_nt_item(nt,xs,ys) -> List.hd ys)
    in
    let nt_items_for_nt = (
      let f1 m (U0_nt nt,rhs) = (
        let rs = try Std_map_string.find nt m with Not_found -> [] in
        let m = Std_map_string.add nt ((U0_nt nt,rhs)::rs) m in
        m)
      in
      let m = List.fold_left f1 Std_map_string.empty g in
      fun (U0_nt nt) i -> (
        let rs = try Std_map_string.find nt m with Not_found -> [] in
        List.map (fun (nt,rhs) -> {v8=(U0_pre_nt_item(nt,[],rhs)); i8=i; j8=i}) rs))
    in
    let (_:nt->int->nt_item list) = nt_items_for_nt in
    let mk_item = (fun x -> match x with 
      | `NTITM x -> U0_item_nt x
      | `TMITM x -> U0_item_tm x)
    in
    let dest_item = (fun x -> match x  with
      | U0_item_nt y -> `NTITM y
      | U0_item_tm y -> `TMITM y)
    in
    let tm_dot_i9 = (fun x -> x.i8) in
    let sym_dot_i9 = (fun x -> x.i8) in
    let sym_dot_j9 = (fun x -> x.j8) in
    let nt_dot_i9 = (fun x -> x.i8) in
    let nt_dot_j9 = (fun x -> x.j8) in
    let with_j9 = (fun x j -> {x with j8=j}) in
      {
        sym_case       =sym_case       ;
        sym_of_tm      =sym_of_tm      ;
        mk_tm_coord    =mk_tm_coord    ;
        tm5            =tm5            ;
        mk_sym_coord   =mk_sym_coord   ;
        sym6           =sym6           ;
        nt2            =nt2            ;
        shift_a2_b2_c2 =shift_a2_b2_c2 ;
        a2_length_1    =a2_length_1    ;
        b2_nil         =b2_nil         ;
        hd_a2          =hd_a2          ;
        hd_b2          =hd_b2          ;
        nt_items_for_nt=nt_items_for_nt;
        mk_item        =mk_item        ;
        dest_item      =dest_item      ;
        tm_dot_i9      =tm_dot_i9      ;
        sym_dot_i9     =sym_dot_i9     ;
        sym_dot_j9     =sym_dot_j9     ;
        nt_dot_i9      =nt_dot_i9      ;
        nt_dot_j9      =nt_dot_j9      ;
        with_j9        =with_j9        ;
      })

  let opsx g nt0 ops = (
    let init_items=(fun () -> ops.nt_items_for_nt nt0 0) in
    let u0_tm=(fun x -> x) in
    let sym_of_0=(fun x -> x) in
    let tm_of_0=(fun x -> x) in
    object
      method init_items=init_items;
      method u0_tm=u0_tm;
      method sym_of_0=sym_of_0;
      method tm_of_0=tm_of_0;
    end)

  let get_symbols g = (
      let syms_of_rhs rhs = rhs in
      let syms_of_parse_rule (nt,rhs) = finset_insert (U0_sym_nt(nt)) (syms_of_rhs rhs) in
      let syms_of_grammar g = finset_unions (List.map syms_of_parse_rule g) in
      syms_of_grammar g)

  let u0_info ((g:grammar),nt0) = (
    let ops = ops g in
    let opsx = opsx g nt0 ops in
    let symbols = get_symbols g in
    object
      method grammar=g;
      method symbols=symbols;
      method ops=ops;
      method opsx=opsx;
    end)


  module U0_sets_maps = (struct
   
    (* FIXME for efficiency EARLEY_TYPES should expose these orderings *)  
    module Set_nt_item = MySet_Make(
      struct
        type t = nt_item
        let compare x y = Pervasives.compare x y
      end)
    
    
    module Set_item = MySet_Make(
      struct
        type t = item
        let compare x y = Pervasives.compare x y 
      end)
    
    module Set_sym_coord = MySet_Make(
      struct
        type t = sym_coord
        let compare x y = Pervasives.compare x y 
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
        let compare x y = Pervasives.compare x y
        let default = Set_nt_item.empty
      end)
    
    module Map_complete_key = MyMap(
      struct
        type key = int * sym
        type value = Set_sym_coord.t
        let compare x y = Pervasives.compare x y
        let default = Set_sym_coord.empty
      end)
    
    (* oracle implementation type *)
    module Map_sym_sym_int_int = MyMap(
      struct
        type key = sym * sym * int * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
        let default = Set_int.empty
      end)
    
    (* tmoracle impl type *)
    module Map_tm_int = MyMap(
      struct
        type key = tm * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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

end (* U0 *)

(* nts and tms are ints *)
module U1 = struct

  type nt = U1_nt of int
  type tm = U1_tm of int
  type sym = U1_sym_nt of nt | U1_sym_tm of tm

  type tm_coord = tm coord
  type sym_coord = sym coord

  type pre_nt_item = U1_pre_nt_item of (nt * sym list * sym list)
  type nt_item = pre_nt_item coord

  type item  = U1_item_nt of nt_item | U1_item_tm of tm_coord

  let even n = (n mod 2 = 0)
  let odd n = not (even n)

  let private_iso0_1 g = (
    let symbols = U0.get_symbols g in
    let f1 (acc,n) x = (
    (* we want to map nts to even numbers and tms to odd numbers *)
      match x with 
      | U0.U0_sym_nt _ -> (
        let n = if even n then n else n+1 in
        (acc@[(n,x)],n+1))
      | U0.U0_sym_tm _ -> (
        let n = if odd n then n else n+1 in
        (acc@[(n,x)],n+1)))
    in
    let is_NT i = ((i mod 2)=0) in
    let (nsyms,free_int) = (List.fold_left f1 ([],0) symbols) in
    let nts = nsyms |> List.filter (fun (_,x) -> match x with | U0.U0_sym_nt _ -> true | _ -> false) |> List.map fst in
    let tms = nsyms |> List.filter (fun (_,x) -> match x with | U0.U0_sym_tm _ -> true | _ -> false) |> List.map fst in
    let syms=nts@tms in
    let f1 (x,y) = (match y with
      | U0.U0_sym_nt _ -> (U1_sym_nt (U1_nt x),y)
      | U0.U0_sym_tm _ -> (U1_sym_tm (U1_tm x),y))
    in
    let nsyms = List.map f1 nsyms in
    let sym0_of_1 sym1 = List.assoc sym1 nsyms in
    let sym1_of_0 sym0 = List.assoc sym0 (List.map (fun (x,y) -> (y,x)) nsyms) in
    let nt1_of_0 nt0 = (
      let U1_sym_nt nt1 = sym1_of_0 (U0.U0_sym_nt nt0) in
      nt1)
    in
    let nt0_of_1 nt1 = (
      let U0.U0_sym_nt nt0 = sym0_of_1 (U1_sym_nt nt1) in
      nt0)
    in
    let tm0_of_1 tm = (
      let U0.U0_sym_tm x = sym0_of_1 (U1_sym_tm tm) in
      x)
    in
    let tm1_of_0 x = (
      let U1_sym_tm x = sym1_of_0 (U0.U0_sym_tm x) in
      x)
    in
    let tm_item0_of_1 x = {x with v8=(tm0_of_1 x.v8)} in
    let tm_item1_of_0 x = {x with v8=(tm1_of_0 x.v8)} in
    let sym_item0_of_1 x = {x with v8=(sym0_of_1 x.v8)} in
    let sym_item1_of_0 x = {x with v8=(sym1_of_0 x.v8)} in
    let pre_nt_item0_of_1 = (function U1_pre_nt_item (nt,_as,bs) ->
      U0.U0_pre_nt_item(nt0_of_1 nt,List.map sym0_of_1 _as, List.map sym0_of_1 bs))
    in
    let pre_nt_item1_of_0 = (function U0.U0_pre_nt_item (nt,_as,bs) ->
      U1_pre_nt_item(nt1_of_0 nt,List.map sym1_of_0 _as, List.map sym1_of_0 bs))
    in
    let nt_item0_of_1 x = {x with v8=(pre_nt_item0_of_1 x.v8) } in
    let nt_item1_of_0 x = {x with v8=(pre_nt_item1_of_0 x.v8) } in
    let item0_of_1 x = (match x with
      | U1_item_nt x -> U0.U0_item_nt (nt_item0_of_1 x)
      | U1_item_tm x -> U0.U0_item_tm (tm_item0_of_1 x))
    in
    let item1_of_0 x = (match x with
      | U0.U0_item_nt x -> U1_item_nt (nt_item1_of_0 x)
      | U0.U0_item_tm x -> U1_item_tm (tm_item1_of_0 x))
    in
    let iso = {
       nt1_of_2      =nt0_of_1      
      ;nt2_of_1      =nt1_of_0      
      ;tm1_of_2      =tm0_of_1      
      ;tm2_of_1      =tm1_of_0      
      ;sym1_of_2     =sym0_of_1     
      ;sym2_of_1     =sym1_of_0     
      ;tm_item1_of_2 =tm_item0_of_1 
      ;tm_item2_of_1 =tm_item1_of_0 
      ;sym_item1_of_2=sym_item0_of_1
      ;sym_item2_of_1=sym_item1_of_0
      ;nt_item1_of_2 =nt_item0_of_1 
      ;nt_item2_of_1 =nt_item1_of_0 
      ;item1_of_2    =item0_of_1    
      ;item2_of_1    =item1_of_0    
    }
    in 
    object
      method iso=iso;
      method free_int=free_int;
      method is_NT=is_NT;
      method nsyms=nsyms;
      method nts=nts;
      method tms=tms;
      method syms=syms;
      method syms_assoc=(List.map (fun (x,y) -> (y,x)) nsyms);
    end)

  let u1_info (g,nt0) = (
    let u0_info = U0.u0_info (g,nt0) in
    let ops0 = u0_info#ops in
    let opsx0 = u0_info#opsx in
    let private_iso0_1 = private_iso0_1 g in
    let iso = private_iso0_1#iso in
    let ops1 = mk_ops2 ops0 iso in
    let opsx1 = mk_ops2x opsx0 iso in
    let rule1_of_rule0 (nt0,rhs0) = (
      let nt1 = iso.nt2_of_1 nt0 in
      let rhs1 = List.map iso.sym2_of_1 rhs0 in
      (nt1,rhs1))
    in
    let g1=List.map rule1_of_rule0 u0_info#grammar in
    object
      method u0_info=u0_info;
      method ops=ops1;
      method opsx=opsx1;
      method grammar=g1;
      method free_int  =private_iso0_1#free_int;                             
      method is_NT     =private_iso0_1#is_NT;                                
      method nts       =private_iso0_1#nts;                                  
      method tms       =private_iso0_1#tms;                                  
      method syms      =private_iso0_1#syms;                                 
      method syms_assoc=private_iso0_1#syms_assoc;
      method iso0_1    =private_iso0_1#iso;                               
    end)

end (* U1 *)


let get_items g = (
  let get_items nt_item = (
    let rec f1 (nt,alpha,beta) = (
      if beta=[] 
        then [(nt,alpha,beta)] 
        else (nt,alpha,beta)::(f1 (nt,(List.hd beta)::alpha,List.tl beta)))
    in
    let (nt,beta) = nt_item in
    f1 (nt,[],beta))
  in
  List.concat (List.map get_items g))

let rec upto m n = if m<=n then m::(upto (m+1) n) else []

(* nt_items are ints *)
module U2 = struct

  type nt = U2_nt of int
  type tm = U2_tm of int
  type sym = U2_sym_nt of nt | U2_sym_tm of tm (* FIXME just of int *)

  type tm_coord = tm coord
  type sym_coord = sym coord

  type pre_nt_item = U2_pre_nt_item of int
  type nt_item = pre_nt_item coord

  type item = U2_item_nt of nt_item | U2_item_tm of tm_coord

  type grammar = (nt*(sym list)) list

  let private_iso1_2 u1info = (
    let open U1 in
    let free_int = u1info#free_int in
    let itms = get_items u1info#grammar in
    let f1 (acc,n) itm = (acc@[(n,itm)],n+1) in
    let (nitms,free_pre_nt_item) = (List.fold_left f1 ([],free_int) itms) in
    let nitms = List.map (fun (x,itm1) -> (U2_pre_nt_item x,U1.U1_pre_nt_item itm1)) nitms in
    let min_pre_nt_item = free_int in
    let suc_max_pre_nt_item = free_pre_nt_item in
    let pre_nt_items = upto min_pre_nt_item (suc_max_pre_nt_item - 1) in
    let is_pre_item_nt i = i >= free_int in
    let pre_nt_item1_of_2 x2 = List.assoc x2 nitms in
    let pre_nt_item2_of_1 x1 = List.assoc x1 (List.map (fun (x,y) -> (y,x)) nitms) in
    let tm1_of_2 (U2_tm x) = (U1_tm x) in
    let tm2_of_1 (U1_tm x) = (U2_tm x) in
    let nt1_of_2 (U2_nt x) = (U1_nt x) in
    let nt2_of_1 (U1_nt x) = (U2_nt x) in
    let sym1_of_2 x = (match x with
      | U2_sym_nt x -> U1_sym_nt (nt1_of_2 x)
      | U2_sym_tm x -> U1_sym_tm (tm1_of_2 x))
    in
    let sym2_of_1 x = (match x with
      | U1_sym_nt x -> U2_sym_nt (nt2_of_1 x)
      | U1_sym_tm x -> U2_sym_tm (tm2_of_1 x))
    in
    let tm_item1_of_2 x = {x with v8=(tm1_of_2 x.v8)} in
    let tm_item2_of_1 x = {x with v8=(tm2_of_1 x.v8)} in
    let sym_item1_of_2 x = {x with v8=(sym1_of_2 x.v8)} in
    let sym_item2_of_1 x = {x with v8=(sym2_of_1 x.v8)} in
    (*
    let pre_nt_item1_of_2 = (function U2_pre_nt_item (nt,_as,bs) ->
      U1.U1_pre_nt_item(nt1_of_2 nt,List.map sym1_of_2 _as, List.map sym1_of_2 bs))
    in
    let pre_nt_item2_of_1 = (function U1.U1_pre_nt_item (nt,_as,bs) ->
      U2_pre_nt_item(nt2_of_1 nt,List.map sym2_of_1 _as, List.map sym2_of_1 bs))
    in
    *)
    let nt_item1_of_2 x = {x with v8=(pre_nt_item1_of_2 x.v8) } in
    let nt_item2_of_1 x = {x with v8=(pre_nt_item2_of_1 x.v8) } in
    let item1_of_2 x = (match x with
      | U2_item_nt x -> U1.U1_item_nt (nt_item1_of_2 x)
      | U2_item_tm x -> U1.U1_item_tm (tm_item1_of_2 x))
    in
    let item2_of_1 x = (match x with
      | U1.U1_item_nt x -> U2_item_nt (nt_item2_of_1 x)
      | U1.U1_item_tm x -> U2_item_tm (tm_item2_of_1 x))
    in
    let pre_nt_items_a2_length_1 = (
      let f1 pni = (
        let x = pre_nt_item1_of_2 (U2_pre_nt_item pni) in
        match x with
        | U1.U1_pre_nt_item(nt,a2,_) -> List.length a2 = 1)
      in
      List.filter f1 pre_nt_items)
    in
    let pre_nt_items_not_a2_nil = (
      let f1 pni = (
        let x = pre_nt_item1_of_2 (U2_pre_nt_item pni) in
        match x with
        | U1.U1_pre_nt_item(nt,a2,_) -> a2 <> [])
      in
      List.filter f1 pre_nt_items)
    in
    let pre_nt_items_not_b2_nil = (
      let f1 pni = (
        let x = pre_nt_item1_of_2 (U2_pre_nt_item pni) in
        match x with
        | U1.U1_pre_nt_item(nt,_,b2) -> b2 <> [])
      in
      List.filter f1 pre_nt_items)
    in
    let iso = {
       nt1_of_2      =nt1_of_2      
      ;nt2_of_1      =nt2_of_1      
      ;tm1_of_2      =tm1_of_2      
      ;tm2_of_1      =tm2_of_1      
      ;sym1_of_2     =sym1_of_2     
      ;sym2_of_1     =sym2_of_1     
      ;tm_item1_of_2 =tm_item1_of_2 
      ;tm_item2_of_1 =tm_item2_of_1 
      ;sym_item1_of_2=sym_item1_of_2
      ;sym_item2_of_1=sym_item2_of_1
      ;nt_item1_of_2 =nt_item1_of_2 
      ;nt_item2_of_1 =nt_item2_of_1 
      ;item1_of_2    =item1_of_2    
      ;item2_of_1    =item2_of_1    
    }
    in
    object
      method iso=iso;
      method is_pre_item_nt=is_pre_item_nt;
      method min_pre_nt_item=min_pre_nt_item;
      method suc_max_pre_nt_item=suc_max_pre_nt_item;
      method pre_nt_items=pre_nt_items;
      method pre_nt_items_a2_length_1=pre_nt_items_a2_length_1;
      method pre_nt_items_not_a2_nil=pre_nt_items_not_a2_nil;
      method pre_nt_items_not_b2_nil=pre_nt_items_not_b2_nil;
    end)

  let u2_info (g,nt0) = (
    let u1_info = U1.u1_info (g,nt0) in
    let ops1 = u1_info#ops in
    let opsx1 = u1_info#opsx in
    let private_iso1_2 = private_iso1_2 u1_info in
    let iso = private_iso1_2#iso in
    let ops2 = mk_ops2 ops1 iso in
    let opsx2 = mk_ops2x opsx1 iso in
    object
      method u1_info=u1_info;
      method ops=ops2;
      method opsx=opsx2;
      method iso1_2=iso;
      method is_pre_item_nt          =private_iso1_2#is_pre_item_nt;          
      method min_pre_nt_item         =private_iso1_2#min_pre_nt_item;         
      method suc_max_pre_nt_item     =private_iso1_2#suc_max_pre_nt_item;     
      method pre_nt_items            =private_iso1_2#pre_nt_items;            
      method pre_nt_items_a2_length_1=private_iso1_2#pre_nt_items_a2_length_1;
      method pre_nt_items_not_a2_nil =private_iso1_2#pre_nt_items_not_a2_nil; 
      method pre_nt_items_not_b2_nil =private_iso1_2#pre_nt_items_not_b2_nil; 
    end)


  (* here we define some sets and maps *)
  (* need to map between modules and 'a myset, which is a record *)

 
  (* FIXME for efficiency EARLEY_TYPES should expose these orderings *)  
  module Set_nt_item = MySet_Make(
    struct
      type t = nt_item
      let compare x y = Pervasives.compare x y
    end)
  
  
  module Set_item = MySet_Make(
    struct
      type t = item
      let compare x y = Pervasives.compare x y 
    end)
  
  module Set_sym_coord = MySet_Make(
    struct
      type t = sym_coord
      let compare x y = Pervasives.compare x y 
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
      let compare x y = Pervasives.compare x y
      let default = Set_nt_item.empty
    end)
  
  module Map_complete_key = MyMap(
    struct
      type key = int * sym
      type value = Set_sym_coord.t
      let compare x y = Pervasives.compare x y
      let default = Set_sym_coord.empty
    end)
  
  (* oracle implementation type *)
  module Map_sym_sym_int_int = MyMap(
    struct
      type key = sym * sym * int * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
      let default = Set_int.empty
    end)
  
  (* tmoracle impl type *)
  module Map_tm_int = MyMap(
    struct
      type key = tm * int
      type value = Set_int.t
      let compare x y = Pervasives.compare x y
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

end (* U2 *)



(* U3 is as U2, but we blow away the datatype wrappers *)
module U3 = struct

  type nt = int
  type tm = int 
  type sym = int 

  type tm_coord = tm coord
  type sym_coord = sym coord

  type pre_nt_item = int
  type nt_item = pre_nt_item coord

  type pre_item = int
  type item  = pre_item coord

  type grammar = (nt*(sym list)) list

  let private_iso2_3 u2info = (
    let u1info = u2info#u1_info in
    let open U2 in
    let tm2_of_3 (x) = (U2_tm x) in
    let tm3_of_2 (U2_tm x) = (x) in
    let nt2_of_3 (x) = (U2_nt x) in
    let nt3_of_2 (U2_nt x) = (x) in
    let sym2_of_3 sym = (
      if u1info#is_NT sym then U2_sym_nt (U2_nt sym) else U2_sym_tm (U2_tm sym))
    in
    let sym3_of_2 x = (match x with
      | U2_sym_nt x -> (nt3_of_2 x)
      | U2_sym_tm x -> (tm3_of_2 x))
    in
    let tm_item2_of_3 x = {x with v8=(tm2_of_3 x.v8)} in
    let tm_item3_of_2 x = {x with v8=(tm3_of_2 x.v8)} in
    let sym_item2_of_3 x = {x with v8=(sym2_of_3 x.v8)} in
    let sym_item3_of_2 x = {x with v8=(sym3_of_2 x.v8)} in
    let pre_nt_item2_of_3 = (fun x -> U2.U2_pre_nt_item(x)) in
    let pre_nt_item3_of_2 = (function U2.U2_pre_nt_item x -> x) in
    let nt_item2_of_3 x = {x with v8=(pre_nt_item2_of_3 x.v8) } in
    let nt_item3_of_2 x = {x with v8=(pre_nt_item3_of_2 x.v8) } in
    let item2_of_3 x = (
      if u2info#is_pre_item_nt x.v8 then U2_item_nt (nt_item2_of_3 x)
      else U2_item_tm (tm_item2_of_3 x))
    in
    let item3_of_2 x = (match x with
      | U2_item_nt x -> (nt_item3_of_2 x)
      | U2_item_tm x -> (tm_item3_of_2 x))   
    in
    let iso = {
       nt1_of_2      =nt2_of_3      
      ;nt2_of_1      =nt3_of_2      
      ;tm1_of_2      =tm2_of_3      
      ;tm2_of_1      =tm3_of_2      
      ;sym1_of_2     =sym2_of_3     
      ;sym2_of_1     =sym3_of_2     
      ;tm_item1_of_2 =tm_item2_of_3 
      ;tm_item2_of_1 =tm_item3_of_2 
      ;sym_item1_of_2=sym_item2_of_3
      ;sym_item2_of_1=sym_item3_of_2
      ;nt_item1_of_2 =nt_item2_of_3 
      ;nt_item2_of_1 =nt_item3_of_2 
      ;item1_of_2    =item2_of_3    
      ;item2_of_1    =item3_of_2    
    }
    in
    object
      method iso=iso;
    end)

  let u3_info g = (
    let u2_info = U2.u2_info g in
    let ops2 = u2_info#ops in
    let opsx2 = u2_info#opsx in
    let private_iso2_3 = private_iso2_3 u2_info in
    let iso = private_iso2_3#iso in
    let ops3 = mk_ops2 ops2 iso in
    let opsx3 = mk_ops2x opsx2 iso in
    object
      method u2_info=u2_info;
      method ops=ops3;
      method opsx=opsx3;
      method iso2_3=iso;
    end)

end (* U3 *)


let tabulate f default ds = (
  let max = List.fold_left (fun acc i -> if i > acc then i else acc) 0 ds in
  let arr = Array.make (max+1) default in
  let f1 () i = Array.set arr i (f i) in
  let _ = List.fold_left f1 () ds in
  fun i -> Array.get arr i)
let (_:(int -> 'a) -> 'a -> int list -> (int -> 'a)) = tabulate


(* U4 is an optimized version of U3, with functional sets and maps *)
module U4 = struct

  include U3

  let u4_info g = (
    let u3_info = U3.u3_info g in
    let u2_info = u3_info#u2_info in
    let u1_info = u2_info#u1_info in
    (* optimizations! *)
    let orig = u3_info#ops in
    let pre_nt_items = u2_info#pre_nt_items in
    let sym_case=(fun x -> 
      if u1_info#is_NT x then `NT x else `TM x)
    in
    let sym_of_tm=(fun x -> x) in
    let mk_tm_coord=(fun (x,i) -> {v8=x;i8=i;j8=i}) in
    let tm5=(
      let f = orig.tm5 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let f' = tabulate f 0 u2_info#u1_info#tms in
      fun x -> f' x.v8)
    in
    let mk_sym_coord=(fun (x,i,j) -> {v8=x;i8=i;j8=j}) in
    let nt_items_for_nt=(
        let f = (fun x i -> orig.nt_items_for_nt x i) in
        (* but we want to work with pre_nt_items *)
        let f = fun nt -> List.map (fun x -> x.v8) (f nt 0) in
        (* and we need to tabulate over all nts *)
        let nts = u2_info#u1_info#nts in
        let f' = tabulate f [] nts in
        fun x i -> List.map (fun x -> {v8=x; i8=i; j8=i}) (f' x)) (* optimize! *)
    in
    let a2_length_1=(
	    let f = fun x -> orig.a2_length_1 x in
      (* want to work with pre_nt_items *)
      let f = fun x -> orig.a2_length_1 {v8=x;i8=0;j8=0} in
      let f' = tabulate f false pre_nt_items in
      fun x -> f' x.v8)
    in
    let b2_nil = (
      let f = orig.b2_nil in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let f' = tabulate f false pre_nt_items in
      fun x -> f' x.v8)
    in
    let hd_a2 = (
      let f = orig.hd_a2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let pre_nt_items = u2_info#pre_nt_items_not_a2_nil in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' x.v8)
    in
    let hd_b2 = (
      let f = orig.hd_b2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let pre_nt_items = pre_nt_items |> List.map (fun x -> {v8=x; i8=0;j8=0}) |> List.filter (fun x -> not(b2_nil x)) |> List.map (fun x -> x.v8) in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' x.v8)
    in
    let nt2=(
      let f = orig.nt2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' x.v8)
    in
    let sym6=(
      let f = orig.sym6 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f {v8=x; i8=0; j8=0} in
      let f' = tabulate f 0 u2_info#u1_info#syms in
      fun x -> f' x.v8)
    in
    let dest_item=(
      let f = u2_info#is_pre_item_nt in
      fun x -> if f x.v8 then `NTITM x else `TMITM x)
    in
    let mk_item=(fun x -> match x with
      | `NTITM x -> x
      | `TMITM x -> x)
    in
    let dot_i9=(fun x -> x.i8) in
    let dot_j9=(fun x -> x.j8) in
    let with_j9=(fun x i -> {x with j8=i}) in
    let opt = object
      method sym_case       =sym_case;           
      method sym_of_tm      =sym_of_tm;          
      method mk_tm_coord    =mk_tm_coord;        
      method tm5            =tm5;                
      method mk_sym_coord   =mk_sym_coord;       
      method sym6           =sym6;               
      method nt2            =nt2;                
      method shift_a2_b2_c2 =(fun x -> {x with v8=x.v8+1});
      method a2_length_1    =a2_length_1;        
      method b2_nil         =b2_nil;             
      method hd_a2          =hd_a2;              
      method hd_b2          =hd_b2;              
      method nt_items_for_nt=nt_items_for_nt;    
      method mk_item        =mk_item;            
      method dest_item      =dest_item;          
      method dot_i9         =dot_i9;             
      method dot_j9         =dot_j9;             
      method with_j9        =with_j9;            
    end
    in
    let id=(fun x -> x) in
    let ops = {
      sym_case       =opt#sym_case       ;
      sym_of_tm      =opt#sym_of_tm      ;
      mk_tm_coord    =opt#mk_tm_coord    ;
      tm5            =opt#tm5            ;
      mk_sym_coord   =opt#mk_sym_coord   ;
      sym6           =opt#sym6           ;
      nt2            =opt#nt2            ;
      shift_a2_b2_c2 =opt#shift_a2_b2_c2 ;
      a2_length_1    =opt#a2_length_1    ;
      b2_nil         =opt#b2_nil         ;
      hd_a2          =opt#hd_a2          ;
      hd_b2          =opt#hd_b2          ;
      nt_items_for_nt=opt#nt_items_for_nt;
      mk_item        =opt#mk_item        ;
      dest_item      =opt#dest_item      ;
      tm_dot_i9      =opt#dot_i9         ;
      sym_dot_i9     =opt#dot_i9         ;
      sym_dot_j9     =opt#dot_j9         ;
      nt_dot_i9      =opt#dot_i9         ;
      nt_dot_j9      =opt#dot_j9         ;
      with_j9        =opt#with_j9        ;
    }
    in
    object
      method u3_info=u3_info;
      method ops=ops;
      method opsx=u3_info#opsx;
    end)

  module U4_sets_maps = struct

    (* optimiztion for int triples *)
    let xx_compare = (
      let cmp (x:int) (y:int) = (Pervasives.compare: int -> int -> int) x y in
      let cmp (x:int) (y:int) = x-y in
      fun x y -> 
      let i = cmp x.v8 y.v8 in
      if i <> 0 then i else
        let i = cmp x.i8 y.i8 in
        if i <> 0 then i else
          let i = cmp x.j8 y.j8 in
          i)
    
    module Set_nt_item = MySet_Make(
      struct
        type t = nt_item
        let compare x y = xx_compare x y
      end)
    
    
    module Set_item = MySet_Make(
      struct
        type t = item
        let compare x y = xx_compare x y 
      end)
    
    module Set_sym_coord = MySet_Make(
      struct
        type t = sym_coord
        let compare x y = Pervasives.compare x y 
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
        let compare x y = Pervasives.compare x y
        let default = Set_nt_item.empty
      end)
    
    module Map_complete_key = MyMap(
      struct
        type key = int * sym
        type value = Set_sym_coord.t
        let compare x y = Pervasives.compare x y
        let default = Set_sym_coord.empty
      end)
    
    (* oracle implementation type *)
    module Map_sym_sym_int_int = MyMap(
      struct
        type key = sym * sym * int * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
        let default = Set_int.empty
      end)
    
    (* tmoracle impl type *)
    module Map_tm_int = MyMap(
      struct
        type key = tm * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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

  end

end

(* U4_imp is like U4, but some sets and maps use imperative arrays *)
module U4_imp = struct

  include U4

  module U4_imp_sets_maps = struct

    module Set_item = U4_sets_maps.Set_item

    (* alternative imperative version *)
    module Set_todo_done_item = struct
    
      module Set_item = Set_item
    
      type t = Set_item.t array array
    
      let empty n = (
        (* let _ = Printf.printf "Empty %i" n in *)
        Array.make_matrix n n Set_item.empty) 
    
      let ij_of x = (x.i8, x.j8) 
    
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
      let set_nt_item = U4_sets_maps.set_nt_item in
      let set_item = U4_sets_maps.set_item in
      let set_sym_item = U4_sets_maps.set_sym_item in
      let set_todo_done = set_todo_done in
      { set_nt_item; set_item; set_sym_item; set_int; set_todo_done=(set_todo_done len) })

    (* alternative implementation of Map_sym_sym_int_int *)
    module Map_sym_sym = MyMap(
      struct
        type key = sym * sym
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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

    module Set_nt_item = U4_sets_maps.Set_nt_item

    module Map_sym_nt_item = MyMap(
      struct
        type key = sym
        type value = Set_nt_item.t
        let compare x y = Pervasives.compare x y
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

    module Set_sym_coord = U4_sets_maps.Set_sym_coord

    module Map_sym_sym_coord = MyMap(
      struct
        type key = sym
        type value = Set_sym_coord.t
        let compare x y = Pervasives.compare x y
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
      let map_blocked_key = (* U4_sets_maps.map_blocked_key *) map_blocked_key in
      let map_complete_key = (* U4_sets_maps.map_complete_key*) map_complete_key in
      let map_sym_sym_int_int = map_sym_sym_int_int in
      let map_tm_int = U4_sets_maps.map_tm_int in
      { map_blocked_key=(map_blocked_key n); map_complete_key=(map_complete_key n); map_sym_sym_int_int=(map_sym_sym_int_int n); map_tm_int })



  end

end




(* U5 is like U3, but 'a coord is represented using ints *)
module U5 = struct

  type 'a coord0 = 'a coord
  type 'a coord5 = int

  type nt = int
  type tm = int 
  type sym = int 

  type tm_coord = tm coord5
  type sym_coord = sym coord5

  type pre_nt_item = int
  type nt_item = pre_nt_item coord5

  type pre_item = int
  type item  = pre_item coord5

  (* N.B. we can't make the iso uniformly because lift_coord doesn't make sense anymore *)
  let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> 
    let b = pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

  let bits xs = (
    let rec f1 n bs = (
      match bs with 
      | [] -> 0 
      | b::bs -> (
          b*(pow 2 n) + f1 (n+1) bs))
    in
    f1 0 (List.rev xs))
  let _ = bits [1;0;1]        

  (* representation: 
     |b62    b37|b36    b11|b10     b0|
     |i bits(26)|j bits(26)|v bits(11)| 

  *)

  let ten = [1;1;1;1;1;1;1;1;1;1]
  let bits_26 = bits ([1;1;1;1;1;1]@ten@ten) 
  let bits_11 = bits ([1]@ten) 

  let bits_0_10  = bits_11
  let bits_11_36 = bits_26 lsl 11
  let bits_37_62 = bits_26 lsl 37

  let coord3_of_5 : 'a. 'a coord5 -> int coord0 = (
    let f : 'a. 'a coord5 -> int coord0 = (
        fun i0 -> 
          let v = i0 land bits_0_10 in
          let i0 = i0 lsr 11 in
          let j = i0 land bits_26 in
          let i0 = i0 lsr 26 in
          let i = i0 land bits_26 in
          { v8=v; i8=i; j8=j})
    in
    f)

  let coord5_of_3 : 'a. int coord0 -> 'a coord5 = ( 
      let f = (
        fun x -> (
            let (v,i,j) = (x.v8,x.i8,x.j8) in
            (* check that these are all in range *)
            let () = if (v > bits_11 || i > bits_26 || j > bits_26) then failwith "universe: coord5_of_3" else () in
            let i0 = i in
            let i0 = i0 lsl 26 in
            let i0 = i0 lor j in
            let i0 = i0 lsl 11 in
            let i0 = i0 lor v in
            i0))
      in
      f)

  let _ = {v8=123;i8=456;j8=789} |> coord5_of_3 |> coord3_of_5

  let dot_i9 x = (coord3_of_5 x).i8
  let dot_j9 x = (coord3_of_5 x).j8
  let with_i9 : 'a. 'a coord5 -> int -> 'a coord5 = fun x i -> x |> coord3_of_5 |> (fun x -> {x with i8=i}) |> coord5_of_3
  let with_j9 : 'a. 'a coord5 -> int -> 'a coord5 = fun x j -> x |> coord3_of_5 |> (fun x -> {x with j8=j}) |> coord5_of_3

  let not_bits_37_62 = lnot bits_37_62
            
  let dot_i9 x = (x land bits_37_62) lsr 37
  let dot_j9 x = (x land bits_11_36) lsr 11
                                                 
  (* optimized versions for efficiency *)
  let with_i9 : 'a. 'a coord5 -> int -> 'a coord5 = (
    fun x i -> 
    let x' = x land not_bits_37_62 in (* mask out i bits *)
    let i' = i lsl 37 in
    x' lor i')

  let not_bits_11_36 = lnot bits_11_36
                                                             
  (* optimized versions for efficiency *)
  let with_j9 : 'a. 'a coord5 -> int -> 'a coord5 = (
    fun x j -> 
    let x' = x land not_bits_11_36 in (* mask out j bits *)
    let j' = j lsl 11 in
    x' lor j')


  let private_iso3_5 u3info = (
    let u2_info = u3info#u2_info in
    (* optimizations! *)
    let orig = u3info#ops in
    let pre_nt_items = u2_info#pre_nt_items in
    let id = fun x -> x in
    let nt5_of_3 = id in
    let nt3_of_5 = id in
    let tm5_of_3 = id in
    let tm3_of_5 = id in
    let sym3_of_5 = id in
    let sym5_of_3 = id in
    let tm_item3_of_5 x = coord3_of_5 x in
    let tm_item5_of_3 = coord5_of_3 in
    let sym_item3_of_5 x = coord3_of_5 x in
    let sym_item5_of_3 = coord5_of_3 in
    let pre_nt_item3_of_5 x2 = x2 in
    let pre_nt_item5_of_3 x2 = x2 in
    let nt_item3_of_5 (x:nt_item) = coord3_of_5 x in
    let nt_item5_of_3 (x:U3.nt_item) = coord5_of_3 x in
    let item3_of_5 = coord3_of_5 in
    let item5_of_3 = coord5_of_3 in
    let iso = {
       nt1_of_2      =nt3_of_5      
      ;nt2_of_1      =nt5_of_3      
      ;tm1_of_2      =tm3_of_5      
      ;tm2_of_1      =tm5_of_3      
      ;sym1_of_2     =sym3_of_5     
      ;sym2_of_1     =sym5_of_3     
      ;tm_item1_of_2 =tm_item3_of_5 
      ;tm_item2_of_1 =tm_item5_of_3 
      ;sym_item1_of_2=sym_item3_of_5
      ;sym_item2_of_1=sym_item5_of_3
      ;nt_item1_of_2 =nt_item3_of_5 
      ;nt_item2_of_1 =nt_item5_of_3 
      ;item1_of_2    =item3_of_5    
      ;item2_of_1    =item5_of_3    
    }
    in
    object
      method iso=iso;
    end)

  let u5_info g = (
    let u3_info = U3.u3_info g in
    let ops3 = u3_info#ops in
    let opsx3 = u3_info#opsx in
    let private_iso3_5 = private_iso3_5 u3_info in
    let iso = private_iso3_5#iso in
    let ops5 = mk_ops2 ops3 iso in
    let opsx5 = mk_ops2x opsx3 iso in
    let orig = ops5 in
    let opt = object
      method shift_a2_b2_c2=(fun x -> x+1); (* optimization *)
      method dot_i9         =dot_i9;             
      method dot_j9         =dot_j9;             
      method with_j9        =with_j9;            
    end
    in
    let ops = {
      sym_case       =orig.sym_case       ;
      sym_of_tm      =orig.sym_of_tm      ;
      mk_tm_coord    =orig.mk_tm_coord    ;
      tm5            =orig.tm5            ;
      mk_sym_coord   =orig.mk_sym_coord   ;
      sym6           =orig.sym6           ;
      nt2            =orig.nt2            ;
      shift_a2_b2_c2 =opt#shift_a2_b2_c2  ;
      a2_length_1    =orig.a2_length_1    ;
      b2_nil         =orig.b2_nil         ;
      hd_a2          =orig.hd_a2          ;
      hd_b2          =orig.hd_b2          ;
      nt_items_for_nt=orig.nt_items_for_nt;
      mk_item        =orig.mk_item        ;
      dest_item      =orig.dest_item      ;
      tm_dot_i9      =opt#dot_i9          ;
      sym_dot_i9     =opt#dot_i9          ;
      sym_dot_j9     =opt#dot_j9          ;
      nt_dot_i9      =opt#dot_i9          ;
      nt_dot_j9      =opt#dot_j9          ;
      with_j9        =opt#with_j9         ;
    }
    in
    object 
      method u3_info=u3_info;
      method ops=ops5;
      method opsx=opsx5;
      method iso3_5=iso;
    end)

  (* put this in another module so we can avoid name clashes on import *)
  module U5_sets_maps = struct 

    (* optimiztion for ints *)
    let xx_compare = (
      let cmp (x:int) (y:int) = (Pervasives.compare: int -> int -> int) x y in
      let cmp (x:int) (y:int) = x-y in  
      fun x y -> cmp x y
    )

    module Set_nt_item = MySet_Make(
      struct
        type t = nt_item
        let compare x y = xx_compare x y
      end)
    
    
    module Set_item = MySet_Make(
      struct
        type t = item
        let compare x y = xx_compare x y 
      end)
    
    module Set_sym_coord = MySet_Make(
      struct
        type t = sym_coord
        let compare x y = xx_compare x y 
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
        let compare x y = Pervasives.compare x y
        let default = Set_nt_item.empty
      end)
    
    module Map_complete_key = MyMap(
      struct
        type key = int * sym
        type value = Set_sym_coord.t
        let compare x y = Pervasives.compare x y
        let default = Set_sym_coord.empty
      end)
    
    (* oracle implementation type *)
    module Map_sym_sym_int_int = MyMap(
      struct
        type key = sym * sym * int * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
        let default = Set_int.empty
      end)
    
    (* tmoracle impl type *)
    module Map_tm_int = MyMap(
      struct
        type key = tm * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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
  
  end (* U5_sets_maps *)

end


(* U6 is an function-optimized version of U5 *)
module U6 = struct

  include U5 

  let dot_v8 x = U5.((coord3_of_5 x).v8)

  (* optimize *)
  let dot_v8 x = x land bits_0_10

  let u6_info g = (
    let u5_info = U5.u5_info g in
    let orig = u5_info#ops in
    let opsx = u5_info#opsx in
    let u2_info = u5_info#u3_info#u2_info in
    let u1_info = u2_info#u1_info in
    let pre_nt_items = u2_info#pre_nt_items in
    let sym_case=(
      let f = u1_info#is_NT in
      fun x -> if f x then `NT x else `TM x)
    in
    let sym_of_tm=(fun x -> x) in
    let mk_tm_coord=(fun (x,i) -> (i lsl (11+26)) lor x) in (* NB j component is 0 *)
    let tm5=(
      let f = orig.tm5 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let f' = tabulate f 0 u2_info#u1_info#tms in
      fun x -> f' (dot_v8 x))
    in
    let mk_sym_coord=(fun (v,i,j) -> (
      let i0 = i in
      let i0 = i0 lsl 26 in
      let i0 = i0 lor j in
      let i0 = i0 lsl 11 in
      let i0 = i0 lor v in
      i0))
    in
    let sym6=(
      let f = orig.sym6 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let f' = tabulate f 0 u2_info#u1_info#syms in
      fun x -> f' (dot_v8 x))
    in
    let nt2=(
      let f = orig.nt2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' (dot_v8 x))
    in
    let a2_length_1=(
	    let f = fun x -> orig.a2_length_1 x in
     (* want to work with pre_nt_items *)
     let f = fun x -> orig.a2_length_1 (coord5_of_3 {v8=x;i8=0;j8=0}) in
     let f' = tabulate f false pre_nt_items in
     fun x -> f' (dot_v8 x))
    in
    let b2_nil = (
      let f = orig.b2_nil in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let f' = tabulate f false pre_nt_items in
      fun x -> f' (dot_v8 x))
    in
    let hd_a2 = (
      let f = orig.hd_a2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let pre_nt_items = u2_info#pre_nt_items_not_a2_nil in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' (dot_v8 x))
    in
    let hd_b2 = (
      let f = orig.hd_b2 in
      (* want to work with pre_nt_items *)
      let f = fun x -> f (coord5_of_3 {v8=x; i8=0; j8=0}) in
      let pre_nt_items = u2_info#pre_nt_items_not_b2_nil in
      let f' = tabulate f 0 pre_nt_items in
      fun x -> f' (dot_v8 x))
    in
    let nt_items_for_nt=(
        let f = (fun x i -> orig.nt_items_for_nt x i) in
        (* but we want to work with pre_nt_items *)
        let f = fun nt -> List.map (fun x -> dot_v8 x) (f nt 0) in
        (* and we need to tabulate over all nts *)
        let nts = u2_info#u1_info#nts in
        let f' = tabulate f [] nts in
        (* optimize! *)
        fun x i -> List.map (fun x -> x |> (fun x -> with_i9 x i) |> (fun x -> with_j9 x i)) (f' x)) 
    in
    let mk_item=(fun x -> match x with
      | `NTITM x -> x
      | `TMITM x -> x)
    in
    let dest_item=(
      let f = u2_info#is_pre_item_nt in
      fun x -> if f (dot_v8 x) then `NTITM x else `TMITM x)
    in
    let opt = object
      method sym_case=sym_case;
      method sym_of_tm=sym_of_tm;
      method mk_tm_coord=mk_tm_coord;
      method tm5=tm5;
      method mk_sym_coord=mk_sym_coord;
      method sym6=sym6;
      method nt2=nt2;
      method shift_a2_b2_c2=(fun x -> x+1);
      method a2_length_1=a2_length_1;
      method b2_nil=b2_nil;
      method hd_a2=hd_a2;
      method hd_b2=hd_b2;
      method nt_items_for_nt=nt_items_for_nt;
      method mk_item=mk_item;
      method dest_item=dest_item;
      method dot_i9         =dot_i9;             
      method dot_j9         =dot_j9;             
      method with_j9        =with_j9;            
    end
    in
    let ops = {
      sym_case       =opt#sym_case       ;
      sym_of_tm      =opt#sym_of_tm      ;
      mk_tm_coord    =opt#mk_tm_coord    ; (* opt makes slower ? *)
      tm5            =opt#tm5            ;
      mk_sym_coord   =opt#mk_sym_coord   ; (* opt makes slower ? *)
      sym6           =opt#sym6           ;
      nt2            =opt#nt2            ;
      shift_a2_b2_c2 =opt#shift_a2_b2_c2 ;
      a2_length_1    =opt#a2_length_1    ;
      b2_nil         =opt#b2_nil         ;
      hd_a2          =opt#hd_a2          ;
      hd_b2          =opt#hd_b2          ;
      nt_items_for_nt=opt#nt_items_for_nt;
      mk_item        =opt#mk_item        ;
      dest_item      =opt#dest_item      ;
      tm_dot_i9      =opt#dot_i9          ;
      sym_dot_i9     =opt#dot_i9          ;
      sym_dot_j9     =opt#dot_j9          ;
      nt_dot_i9      =opt#dot_i9          ;
      nt_dot_j9      =opt#dot_j9          ;
      with_j9        =opt#with_j9         ;
    }
    in
    object
      method u5_info=u5_info;
      method ops=ops;
      method opsx=opsx;
    end)

  module U6_sets_maps = U5_sets_maps 


end


(* U7 is like U6, but we optimize sets and maps using imperative arrays *)
module U7 = struct

  include U6

  module U7_sets_maps = struct 

    (* optimiztion for ints *)
    let xx_compare = (
      let cmp (x:int) (y:int) = (Pervasives.compare: int -> int -> int) x y in
      let cmp (x:int) (y:int) = x-y in 
      fun x y -> cmp x y
    )
  
    module Set_nt_item = MySet_Make(
      struct
        type t = nt_item
        let compare x y = xx_compare x y
      end)
    
    module Set_item = MySet_Make(
      struct
        type t = item
        let compare x y = xx_compare x y 
      end)
    
    module Set_sym_coord = MySet_Make(
      struct
        type t = sym_coord
        let compare x y = xx_compare x y 
      end)
      
    (* an alternative version of set_item *)
    (* module Set_todo_done_item = Set_item *)
    
    (* alternative imperative version *)
    
    module Set_todo_done_item = struct
    
      module Set_item = Set_item
    
      type t = Set_item.t array array
    
      let empty n = (
        (* let _ = Printf.printf "Empty %i" n in *)
        Array.make_matrix n n Set_item.empty) 
    
      let ij_of x = (dot_i9 x, dot_j9 x) (* FIXME depends on ops implementation *)
    
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
  
    let set_todo_done n = let open Set_todo_done_item in {
      add=add;
      empty=empty n;
      fold=fold;
      is_empty=is_empty;
      mem=mem;
      elements=elements;
    }
  
  
    let sets len = {
      set_nt_item; set_item; set_sym_item; set_int; set_todo_done=(set_todo_done len);
    }  
      
    module Map_blocked_key = MyMap(
      struct
        type key = int * sym
        type value = Set_nt_item.t
        let compare x y = Pervasives.compare x y
        let default = Set_nt_item.empty
      end)
    
    module Map_complete_key = MyMap(
      struct
        type key = int * sym
        type value = Set_sym_coord.t
        let compare x y = Pervasives.compare x y
        let default = Set_sym_coord.empty
      end)
    
    (* oracle implementation type *)
    (*
    module Map_sym_sym_int_int = MyMap(
      struct
        type key = sym * sym * int * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
        let default = Set_int.empty
      end)
    *)
    
    (* alternative implementation of Map_sym_sym_int_int *)
    module Map_sym_sym = MyMap(
      struct
        type key = sym * sym
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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
    
      
    (* tmoracle impl type *)
    module Map_tm_int = MyMap(
      struct
        type key = tm * int
        type value = Set_int.t
        let compare x y = Pervasives.compare x y
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
  
    let map_sym_sym_int_int n = let open Map_sym_sym_int_int in {
      empty=empty n;
      add=add;
      find2=find2;
    }
  
    let map_tm_int = let open Map_tm_int in {
      empty=empty;
      add=add;
      find2=find2;
    }
  
  
    let maps n = {
      map_blocked_key; map_complete_key; map_sym_sym_int_int=(map_sym_sym_int_int n); map_tm_int;
    }

  end (* U6_sets_maps *)


end


(* U8 is like U7, but for timing purposes we omit oracle construction (partially) *)
module U8 = struct

  include U7

  module U8_sets_maps = struct 

    include U7_sets_maps

    let dummy_map_sym_sym_int_int len = (
      let m = map_sym_sym_int_int len in
      {m with add=(fun k v m -> m)})
  
    let maps n = {
      map_blocked_key; map_complete_key; map_sym_sym_int_int=(dummy_map_sym_sym_int_int n); map_tm_int;
    }

  end (* U6_sets_maps *)


end
