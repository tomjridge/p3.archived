(* this is now a layer on earley3_int *)

(*

#mod_use "earley3_utils.ml";;
#mod_use "universe.ml";;
#mod_use "earley3_int.ml";;
(* #mod_use "int_utils.ml";; *)

*)

module Earley_public_types = struct

  (* input types *)
  type term = string
  type nonterm = string
  type 'a substring = 'a * int * int
  type symbol = [`NT of nonterm | `TM of term]

  type rhs = symbol list
  type parse_rule = nonterm * rhs
  type grammar = parse_rule list

  type uni = [`U4 | `U4_imp | `U6 | `U7 | `U8]

  type 'a ty_setup = <
    g7      : grammar;
    sym7    : nonterm;
    p_of_tm7: term -> 'a substring -> int list;
    string7 : 'a; (* int arg is length of 'a; assumed to start from 0 *)
    length7 : int;
    uni7    : uni option (* suggested algorithm *)
  >

  (* output types *)
  (* oracle for a parsed 'a substring *)
  type ty_oracle = (symbol*symbol) -> (int*int) -> int list

  (* we also cache the results of terminal parsing; FIXME we really only need to check whether tm,int,int is accepted, so result type could be bool  *)
  type ty_tmoracle = term -> int * int -> bool 

  type ty_result = <
    oracle: ty_oracle;
    tmoracle: ty_tmoracle
  >

end


module Earley_interface = struct

  open Earley_public_types
  open E3_uni 

  let earley_full = (
    let u0_nt_of nt = U0.U0_nt nt in
    let u0_tm_of tm = U0.U0_tm tm in
    let u0_sym_of sym = (
      match sym with
      | `NT nt -> U0.U0_sym_nt (u0_nt_of nt)
      | `TM tm -> U0.U0_sym_tm (u0_tm_of tm))
    in
    let grammar0_of_grammar g = (
      let f1 (nt,rhs) = (u0_nt_of nt,List.map u0_sym_of rhs) in
      let g0 = List.map f1 g in
      g0)
    in
    fun (setup0:'a ty_setup) -> (
    let open E3_dispatch in 
    let x86_64 = ((1 lsl 31) <> 0) in (* note lsl 32 seems to be equivalent to lsl 1 etc - unspecified if >=32! *)
    let len = setup0#length7 in
    let g0 = (grammar0_of_grammar setup0#g7) in
    let nt0 = (E3_uni.U0.U0_nt setup0#sym7) in
    let max_length_for_arrays = 10000 in
(*    let alg = (
      let u2_info = U2.u2_info(g0,nt0) in
      let suc_max_pre_nt_item = u2_info#suc_max_pre_nt_item in
      match setup0#uni7 with
      | None -> (
        (* FIXME we also need to check the length of the binarized grammar *)
        if x86_64 && len < max_length_for_arrays && suc_max_pre_nt_item <= U5.bits_11 then 
          (`U7)
        else if x86_64 && suc_max_pre_nt_item <= U5.bits_11 then 
          (`U6)
        else if len < max_length_for_arrays then
          (`U4_imp)
        else
          (`U4))
      | Some(s) -> (s:uni))
    in*)
    let alg = (
      match setup0#uni7 with
      | None -> (
        if len < max_length_for_arrays then
          (`U4_imp)
        else
          (`U4))
      | Some(s) -> (s:uni))
    in
    let alg = `U0 in 
(*    let _ = (match alg with 
      | `U7 -> print_endline "U7"
      | `U6 -> print_endline "U6"
      | `U4_imp -> print_endline "U4_imp"
      | `U4 -> print_endline "U4")
    in *)
    let r = do_earley g0 nt0 setup0#string7 setup0#length7 setup0#p_of_tm7 alg in
    let tmoracle = fun tm -> fun (i,j) -> r#tmoracle5 (u0_tm_of tm) (i,j) in
    let oracle = fun (sym1,sym2) -> fun (i,j) -> r#oracle5 (u0_sym_of sym1,u0_sym_of sym2,i,j) in
    object
      method oracle=oracle;
      method tmoracle=tmoracle;
    end))

end

