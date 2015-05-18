type 'a ty_span = [ `SS of 'a * int * int ]

val dest_SS: 'a ty_span -> 'a * int * int

val content: string ty_span -> string

val concatenate_two: 'a ty_span -> 'a ty_span -> 'a ty_span option

val concatenate_list: 'a ty_span list -> 'a ty_span option
