type 'a box

val box: 'a -> 'a box

val box_even: 'a -> 'a box

val box_odd: 'a -> 'a box

val unbox: 'a box -> 'a

val box_get_key: 'a box -> P3_gensym.t
