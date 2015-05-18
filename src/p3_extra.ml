(**
{1 P3_extra: extra definitions eg memoization, basic parsers}
{2 P3_memo: various ways to memoize the parser combinators}

*)

module P3_memo = struct

  open P3_core

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
      let lc4 = normalize_context i i.lc4 ss in
      let SS(s,l,h) = ss in
      let k = (lc4,SS(P3_box.box_get_key i.box4,l,h)) in (* about 10% slowdown if we include box key *)
      Some k)
    | _ -> None)
  
  let memo_p3 tbl p i = (
    memo tbl key_of_input p i)

(*  let (_:(int local_context * int ty_substring,(string,'a)output) Hashtbl.t -> ('string,'a)parser3 -> ('string,'a)parser3) = memo_p3*)

  (* version that creates table implicitly *)
  (*
  let memo_check_and_upd_lc4' p = (
    let tbl = Hashtbl.create 100 in
    fun i -> (memo_check_and_upd_lc4 tbl p i))
  *)

  (* see https://groups.google.com/forum/?fromgroups=#!topic/fa.caml/wrCpQFm5Keg 
    let memo_rec f =
      let m = ref [] in
      let rec g x =
        try
          List.assoc x !m
        with
            Not_found ->
              let y = f g x in
                m := (x, y) :: !m ;
                y
      in
        g                                                
  *)

  (* p takes an extra initial argument, its "completion" *)
  
  let memo_rec2 p = (
    let tbl = Hashtbl.create 100 in
    let rec g = (fun i -> 
      let nt = dest_NT (sym_of_parser (p g)) in
      memo tbl key_of_input (p g) i)
    in
    g)
  

  (* some additional functions used by the code generator - slight abbreviations of the above *)
  let mcu4 tbl nt p = memo_p3 tbl (mkntparser nt p)

  (* FIXME not sure about the use of unique everywhere *)
  (*
  let unique4 i = (match i with 
    | OutThree rs -> (OutThree (P1_lib.P1_core.Prelude.unique rs))
    | _ -> i)
  *)

end


(**
{2 P3 basic parsers}

P3 basic parsers (parsers for terminals) need names. We could just use gensym to generate the names, but this makes debugging somewhat harder (because the names of terminal parsers are meaningless). To aid debugging, we here provide basic parsers with somewhat meaningful names.

N.B. clearly two non-identical parsers need different names.

FIXME probably we should use the named versions as the FunctorBasicParsers, and just discard the names if we don't need them.

*)

module P3_basic_parsers = (struct

  open P3_core

  type 'a parser123 = 'a P3_core.ty_substring P3_core.parser3'
  type parser3'' = string ty_substring parser3'

  (* string -> substring parser; FIXME no memo *)
  let a =
    let rp lit = (fun (SS(s,i,j)) -> 
        let n = String.length lit in
        if
          (n <= j-i)
          && (String.sub s i n = lit)
        then
          [SS(s,i,i+n)]
        else
          [])
    in
    let f lit = mktmparser lit (rp lit) in
    f

  let (_:string -> parser3'') = a  

end)


;;
