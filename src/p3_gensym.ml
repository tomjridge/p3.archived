type t = int

let counter = ref 0

let gen_int () = (
  let c = !counter in
  let _ = counter := c+1 in 
  c)

let gen_even () = (
  let c = !counter in
  let c = (if c mod 2=0 then c else c+1) in
  let _ = counter := c+1 in
  c)

let gen_odd () = (
  let c = !counter in
  let c = (if c mod 2=1 then c else c+1) in
  let _ = counter := c+1 in
  c)

let t_to_int i = i
