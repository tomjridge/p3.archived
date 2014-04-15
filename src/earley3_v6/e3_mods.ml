open E3_core (* for sets and maps interface *)

module Std_map_int = Map.Make(struct type t = int let compare : int -> int -> int = compare end)

module Std_map_string = Map.Make(String)

module Std_set_int = Set.Make(struct type t = int let compare : int -> int -> int = compare end)

(* restrict operations *)  
module type MYSET = sig
  type elt 
  type t
  val add : elt -> t -> t
  val empty : t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val is_empty : t -> bool
  val mem : elt -> t -> bool
  val elements: t -> elt list
end
  
module MySet_Make = functor (Ord:Set.OrderedType) -> (struct
  include Set.Make(Ord)
end : MYSET with type elt = Ord.t)  

module Set_int = MySet_Make(
  struct
    type t = int
    let compare : int -> int -> int = Pervasives.compare
  end)

let set_int = let open Set_int in {
  add=add;
  empty=empty;
  fold=fold;
  is_empty=is_empty;
  mem=mem;
  elements=elements;
}


module type MYMAP = sig
  type key
  type value
  type ty_map
  val empty : ty_map
  val add : key -> value -> ty_map -> ty_map
  val find2 : key -> ty_map -> value
  val bindings : ty_map -> (key * value) list
end

(* argument to Map functor *)
module type MAPINPUT = sig
    type key
    type value
    val compare : key -> key -> int
    val default: value
end

module MyMap = functor (MapInput:MAPINPUT) -> (struct
  module Ord = struct
      type t = MapInput.key
      let compare = MapInput.compare
  end
  include Map.Make(Ord)
  type value=MapInput.value
  type ty_map=MapInput.value t
  let find2 k m =
    if (mem k m) then (find k m) else MapInput.default
end : (MYMAP with type key = MapInput.key and type value = MapInput.value))

