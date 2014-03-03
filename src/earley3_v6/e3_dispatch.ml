(* 
#mod_use "e3_core.ml";;
#mod_use "e3_uni.ml";;
*)

open E3_core
open E3_uni


(* FIXME duplicate defn *)
let memo tbl f i = (
  if (Hashtbl.mem tbl i) then
    (Hashtbl.find tbl i)
  else (
    let v = f i in
    let _ = Hashtbl.add tbl i v in
    v))

let mk_init_loop2 ctxt ops opsx = (
  let sets = ctxt.sets in
  let maps = ctxt.maps in
  let init_items=opsx#init_items () in
  let init_items=List.map (fun x -> ops.mk_item (`NTITM x)) init_items in
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

let do_earley g nt s len p_of_tm alg = (match alg with
  | `U0 -> (
    let info=U0.u0_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops; sets=U0.U0_sets_maps.sets; maps=U0.U0_sets_maps.maps } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U2 -> (
    let info=U2.u2_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops; sets=U2.sets; maps=U2.maps } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U4 -> (
    let info=U4.u4_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U4.U4_sets_maps.sets; maps=U4.U4_sets_maps.maps } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U4_imp -> (
    let info=U4_imp.u4_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U4_imp.U4_imp_sets_maps.sets (len+1); maps=U4_imp.U4_imp_sets_maps.maps (len+1); } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U5 -> (
    let info=U5.u5_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U5.U5_sets_maps.sets; maps=U5.U5_sets_maps.maps } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U6 -> (
    let info=U6.u6_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U6.U6_sets_maps.sets; maps=U6.U6_sets_maps.maps } in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U7 -> (
    let info=U7.u6_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U7.U7_sets_maps.sets (len+1); maps=U7.U7_sets_maps.maps (len+1)} in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
  | `U8 -> (
    let info=U8.u6_info (g,nt) in
    let ops=info#ops in
    let opsx=info#opsx in
    let p_of_tm' = (fun tm ->
      let (U0.U0_tm tm) = opsx#u0_tm tm in
      p_of_tm tm)
    in
    let ctxt = { string5=s; length5=len; p_of_tm5=p_of_tm'; item_ops5=ops;sets=U8.U8_sets_maps.sets (len+1); maps=U8.U8_sets_maps.maps (len+1)} in
    let init_state = mk_init_loop2 ctxt ops opsx in
    let s = E3_core.earley ctxt init_state in
    let sym_of_0 = opsx#sym_of_0 in
    let tm_of_0 = opsx#tm_of_0 in
    let f1 (sym1,sym2,i,k) = (
      (sym1,sym2,i,k) 
      |> (fun (sym1,sym2,i,k) -> 
          (sym_of_0 sym1, sym_of_0 sym2,i,k))
      |> (fun x -> ctxt.maps.map_sym_sym_int_int.find2 x s.oracle5)
      |> ctxt.sets.set_int.elements)
    in
    let tbl = Hashtbl.create 100 in
    let f1 = memo tbl f1 in
    let f2 tm (i,j) = (
      (tm,i)
      |> (fun (tm,i) -> (tm_of_0 tm,i))
      |> (fun x -> ctxt.maps.map_tm_int.find2 x s.tmoracle5)
      |> (fun x -> ctxt.sets.set_int.mem j x))
    in
    object
      method oracle5=f1;
      method tmoracle5=f2;
    end)
(*  | _ -> failwith "uni_earley: unsupported universe FIXME" *)
)



(*
let _E = U0.U0_nt "E"

let g0 = 
  let open U0 in
  let _E = U0_nt "E" in
  [(_E,[U0_sym_nt _E;U0_sym_nt _E; U0_sym_nt _E]);
   (_E,[U0_sym_tm (U0_tm "1")]);
   (_E,[U0_sym_tm (U0_tm "eps")])]

let _u2_info = U2.u2_info (g0,_E)

(* FIXME this type annotation is getting ridiculous! *)
let (ops:<
       sym_case: 'a. ('nt -> 'a) -> ('tm -> 'a) -> 'sym -> 'a;
	 ..
    >) = Universe.mk_ops _u2_info (* FIXME sym_case is not correct *)

let ops = 
 let ops = Universe.mk_ops _u2_info in
  X.mk_ops ops
(* success! *)

*)
