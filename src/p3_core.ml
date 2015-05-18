(**
{1 P3_core: core P3 definitions}

{2 Prelude}
*)

module GG = P3_gensym
module BB = P3_box
module SP = P3_span

type term = GG.t  (* odd *)
type nonterm = GG.t  (* even *)
type symbol = [ `NT of nonterm | `TM of term ]

let mk_NT () = (GG.gen_even ())
let mk_TM () = (GG.gen_odd ())

let dest_NT sym = (match sym with `NT x -> x | _ -> failwith "dest_NT")

type 'a raw_parser = 'a SP.ty_span -> int list
type ('string,'a) raw_act = 'string SP.ty_span -> 'a list


type 'string tm_plus = {
  tp_tm: term;
  tp_raw_parser: 'string raw_parser }

and 'string nt_plus = {
  np_nt: nonterm;
  np_rhss: unit -> 'string sym_plus list }

and 'string sym_plus = 
  | NP of 'string nt_plus 
  | TP of 'string tm_plus



type 'string ty_oracle = ('string sym_plus * 'string sym_plus) -> (int * int) -> int list

type 'string ty_tmoracle = 'string tm_plus -> int * int -> bool

type 'string local_context = LC of (nonterm * 'string SP.ty_span) list 
let empty_context = (LC [])

type 'string inr = {
  ss4: 'string SP.ty_span;
  box4: 'string BB.box; (* the underlying value is a boxed version of the value in .ss4 *) 
  lc4: GG.t local_context; (* contexts work with box keys, not 'string *)
  oracle4: 'string ty_oracle;
  tmoracle4: 'string ty_tmoracle;
}
type 'a outr = 'a list

type ('string,'a) pre_clause = {
  sp: 'string sym_plus;
  act: 'string inr -> 'a outr }

type ('string,'a) clause = ('string,'a) pre_clause Lazy.t

let rec itlist f l b =
  match l with
    [] -> b
  | (h::t) -> f h (itlist f t b)

let rec allpairs f l1 l2 =
  match l1 with
   h1::t1 ->  itlist (fun x a -> f h1 x :: a) l2 (allpairs f t1 l2)
   | [] -> []

let list_product l1 l2 = allpairs (fun x -> fun y -> (x,y)) l1 l2


let seq: ('s,'a)clause -> ('s,'b) clause -> ('s,'a*'b) clause = (fun p1 p2 ->
    lazy {
      sp=(NP{ 
          np_nt=(mk_NT()); 
          np_rhss=(fun () -> 
              let p1 = Lazy.force p1 in
              let p2 = Lazy.force p2 in
              [p1.sp;p2.sp]) });
      act=(fun i0 ->
          let p1 = Lazy.force p1 in
          let p2 = Lazy.force p2 in
          let `SS(s,i,j) = i0.ss4 in
          let ks = i0.oracle4 (p1.sp,p2.sp) (i,j) in
          let f1 k = (
            let rs1 = p1.act { i0 with ss4=(`SS(s,i,k)) } in
            let rs2 = p2.act { i0 with ss4=(`SS(s,k,j)) } in
            list_product rs1 rs2)
          in
          List.concat (List.map f1 ks)) })

let ( >- ) = seq


let set_act: ('s,'a)clause -> ('a -> 'b) -> ('s,'b)clause = (fun p act -> 
    lazy 
      let p = Lazy.force p in 
      { p with act=(fun x -> x |> p.act |> List.map act) })

let ( >> ) = set_act

let rec _E = lazy(Lazy.force( (seq _E _E) >> (fun _ -> ())))

let _E' = Lazy.force _E

let _ = (match _E'.sp with NP x -> x).np_rhss()

(* version which reuses results from earley parse *)
let mktmparser: 'string raw_parser -> ('string,'a) raw_act -> ('string,'a)clause = (fun rp ract ->
    lazy 
      let tp = {
        tp_tm=(mk_TM());
        tp_raw_parser=rp }
      in
      {
        sp=TP tp;
        act=(fun i0 -> 
            let `SS(s,i,j) = i0.ss4 in
            if i0.tmoracle4 tp (i,j) then
              ract (`SS(s,i,j))
            else
              []) })

      

let run_parser' p txt len (o,tmo) = (
  let ss = (`SS(txt,0,len)) in
  let inr = {
    ss4=ss;
    box4=BB.box txt;
    lc4=LC[];
    oracle4=o;
    tmoracle4=tmo }
  in
  (Lazy.force p).act inr)

let (_:('a,'b)clause -> 'a -> int -> ('a ty_oracle * 'a ty_tmoracle) -> 'b outr) = run_parser'

