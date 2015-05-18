(* invariant: SS(s,i,j): i<=j<=(String.length s) *)
type 'a ty_span = [ `SS of 'a * int * int ]
type ty_span' = string ty_span

let dest_SS (`SS(s,i,j)) = (s,i,j)

let content (`SS(s,i,j)) = String.sub s i (j-i)

let concatenate_two (`SS(s1,i1,j1)) (`SS(s2,i2,j2)) = (
  if (s1=s2) && (j1=i2) then
    Some (`SS(s1,i1,j2))
  else
    None)

let rec concatenate_list ss = (match ss with
|  [] -> None
| s1::ss -> (match ss with
  | [] -> Some s1
  | _ -> (match concatenate_list ss with
      None -> None
  |   Some s2 -> concatenate_two s1 s2)))
