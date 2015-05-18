module GG = P3_gensym

type 'a box = [ `Box of GG.t * 'a ] (* FIXME change `Box to Box *)

let box x = (`Box(GG.gen_int(),x))

let box_even x = (`Box(GG.gen_even(),x))

let box_odd x =  (`Box(GG.gen_odd(),x))

let unbox x = (match x with
  | `Box((c:GG.t),x) -> x)

let box_get_key x = (match x with
  | `Box((c:GG.t),x) -> c)
