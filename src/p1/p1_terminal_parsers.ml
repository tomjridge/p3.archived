(**
Raw parsers, of type substring -> (substring * substring) list

*)

(**
{2 RawParsers}

Raw parsers are typically those parsers corresponding to terminals.

*)

module RawParsers = struct

  (* begin import *)
  (* prelude *)
  let ($) f g x = f(g x)

  let dest_Some x = match x with `Some y -> y | _ -> failwith "dest_Some"

  (* types *)
  type substring = string * int * int

  type term = string

  (* substring *)
  let string (s,l,h) = s

  let (low,high,len) = (
    (fun (s,l,h) -> l),
    (fun (s,l,h) -> h),
    (fun (s,l,h) -> h-l))

  let content s =
    String.sub (string s) (low s) (len s)

  let concatenate_two s1 s2 =
    if (string s1 = string s2) && (high s1 = low s2) then
      `Some (string s1, low s1, high s2)
    else
      `None

  let rec concatenate_list ss = match ss with
    [] -> `None
  | s1::ss -> (match ss with
      [] -> `Some s1
    | _ -> (match concatenate_list ss with
        `None -> `None
    |   `Some s2 -> concatenate_two s1 s2))
  (* end import *)

  let wf_substring (s,i,j) = (
    let wf = i <= String.length s && j <= String.length s && 0 <= i && i <= j in
    if not wf then failwith ("wf_substring!: "^s^" "^(string_of_int i)^" "^(string_of_int j)^"!") else ())

  type raw_parser = substring -> (substring * substring) list

  (* these combinators are only for raw parsers *)
  let ( **>@ ) p1 p2 = (fun s ->
    let f (e1,s1) =
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 s1)
    in
    ((List.concat $ (List.map f) $ p1) s))

  let ( |||@ ) p1 p2 = (fun s -> List.append (p1 s) (p2 s))

  let (>>@) p f = (List.map (fun (e,s) -> (f e, s))) $ p

  let never = fun i -> []

  let noteps p = (fun s ->
    List.filter (fun (_,srem) -> srem <> s) (p s))


  let a lit = fun s ->
    let n = String.length lit in
    if
      (n <= len s)
      && (String.sub (string s) (low s) n = lit)
    then
      let (s1,l,h) = s in
      [((s1,l,l+n),(s1,l+n,h))]
    else
      []

  (* this appears no faster than the naive approach above...
  (* s is matched by m from posn i *)
  let string_matches s m i = (
    let rec smatch j = (
      j >= String.length m || ((String.get s (i+j) = String.get m j) && smatch (j+1)))
    in
    (i + String.length m <= String.length s) && smatch 0)

  let a lit = (
    let n = String.length lit in
    fun s ->
      if
        (n <= len s)
        && (string_matches (string s) lit (low s))
      then
        let (s1,l,h) = s in
        let s2 = (s1,l,l+n) in
        [(s2,inc_low n s)]
      else
        [])
  *)

  let (_:raw_parser) = (a "1")

  (* FIXME change this to take an underlying parser *)
  let until_a lit = fun s ->
    let llit = String.length lit in
    let rec f1 n =
      if
        n+llit <= len s
        && (String.sub (string s) ((low s)+n) llit) = lit
      then
        let (s1,l,h) = s in
        (* let s2 = (s1,l,l+n) in *)
        [((s1,l,l+n),(s1,l+n,h))]
      else if
          n+llit <= len s
      then
        f1 (n+1)
      else
        let (s1,l,h) = s in
        [(s,(s1,h,h))]
    in
    f1 0

  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = fun s ->
    if (1 <= len s && pred (String.sub (string s) (low s) 1)) then
      let (s,i,j) = s in
      [((s,i,i+1),(s,i+1,j))]
    else
      []

  let parse_EOF = fun s -> (
    if (low s = high s) && (high s = String.length (string s)) then
      (a "") s
    else
      never s)

  (* can return eps; FIXME this is incredibly dangerous, and breaks wf of terminal parsers *)
  let parse_while pred = fun s ->
    let rec f = fun n ->
      if n = len s then len s else
      let c = String.sub (string s) ((low s)+n) 1 in
      if pred c then f (n+1) else n
    in
    let n = f 0 in
    let (s,i,j) = s in
    [((s,i,i+n),(s,i+n,j))]

  let (_:(string -> bool) -> raw_parser) = parse_while

  (* FIXME could tidy up the following *)

  let parse_azAZ =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    parse1 pred

  let (_:raw_parser) = parse_azAZ

  let parse_AZ =
    let pred c =
      (String.compare "A" c <= 0)
      && (String.compare c "Z" <= 0)
    in
    parse1 pred

  let parse_az =
    let pred c =
      (String.compare "a" c <= 0)
      && (String.compare c "z" <= 0)
    in
    parse1 pred

  let parse_azs =
    let pred c =
      (String.compare "a" c <= 0)
      && (String.compare c "z" <= 0)
    in
    parse_while pred

  let parse_AZS =
    let pred c =
      (String.compare "A" c <= 0)
      && (String.compare c "Z" <= 0)
    in
    noteps (parse_while pred)

  let parse_ws = noteps (parse_while (fun s -> s = " " || s = "\n"))

  let parse_epsws = (parse_while (fun s -> s = " " || s = "\n"))

  let parse_newline = a "\n"

  let parse_azAZs =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
    in
    noteps (parse_while pred)

  let parse_notdquote =
    parse_while (fun c -> not (c = "\""))

  let parse_notsquote =
    parse_while (fun c -> not (c = "'"))

  let parse_notlt =
    parse_while (fun c -> not (c = "<"))

  let parse_notgt =
    parse_while (fun c -> not (c = ">"))

  let parse_notltgt =
    parse_while (fun c -> not ((c = "<") || (c = ">")))

  let parse_notbracket =
    parse_while (fun c -> not ((c = "(") || (c = ")")))

  (* FIXME if parse_ws includes \n, then notws should also *)
  let parse_notws =
    parse_while (fun c -> not (c = " "))

  let parse_notcurlyr = parse_while (fun c -> not (c = "}"))

  let parse_all =
    parse_while (fun c -> true)

  let parse_num =
    let pred = fun c ->
      (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
    in
    noteps (parse_while pred)

  (* the following is hopeless, obviously; nums are non-empty  *)
  let parse_float = ((parse_num **>@ (a ".") **>@ parse_num) >>@ (fun (x,(y,z)) -> dest_Some (concatenate_list [x;y;z])))

  let parse_ident =
    let pred = fun c ->
      ((String.compare "A" c <= 0) && (String.compare c "Z" <= 0))
      || ((String.compare "a" c <= 0) && (String.compare c "z" <= 0))
      || (String.compare "0" c <= 0) && (String.compare c "9" <= 0)
      || (c = "_") || (c = "'")
    in
    noteps (parse_while pred)

  let term_to_parser s = (match s with
    | "?all?"   -> parse_all
    | "?AZS?" -> parse_AZS
    | "?AZ?" -> parse_AZ
    | "?az?" -> parse_az
    | "?azs?" -> parse_azs
    | "?azAZ?" -> parse_azAZ
    | "?azAZs?" -> parse_azAZs
    | "?EOF?" -> parse_EOF
    | "?epsws?" -> parse_epsws
    | "?ident?" -> parse_ident
    | "?newline?" -> parse_newline
    | "?notbracket?" -> parse_notbracket
    | "?notcurlyr?" -> parse_notcurlyr
    | "?notdquote?" -> parse_notdquote
    | "?notgt?" -> parse_notgt
    | "?notlt?" -> parse_notlt
    | "?notltgt?" -> parse_notltgt
    | "?notsquote?" -> parse_notsquote
    | "?num?" -> parse_num
    | "?float?" -> parse_float
    | "?ws?" -> parse_ws
    | "\"\"" -> a ""
    | _ -> ( (* interpret as a literal *)
        if String.length s < 2 then failwith ("term_to_parser: "^s)
        else
    let _ = () (* print_string ("term_to_parser: treating "^s^" as a literal\n") *) in
    (a (String.sub s 1 (String.length s - 2)))))

  let (_:term -> raw_parser) = term_to_parser

end


(**
{2 Functorized raw parsers}

We lift the raw parsers to operate over an arbitrary input type. In BasicParsers We then instantiate the input type to `ty_input1`.

*)

module type SubstringPlus = sig
  type ty_input
  val substr:ty_input -> (string*int*int)
  val with_substr:ty_input -> (string*int*int) -> ty_input
end

module FunctorBasicParsers = functor(A:SubstringPlus) -> struct

  open RawParsers

  type ty_input = A.ty_input

  type 'a myparser = A.ty_input -> ('a * substring) list

  let seq p1 p2 = (fun i ->
    let f (e1,s1) =
      List.map (fun (e2,s2) -> ((e1,e2),s2)) (p2 (A.with_substr i s1))
    in
    ((List.concat $ (List.map f) $ p1) i))

  let alt p1 p2 = (fun i -> List.append (p1 i) (p2 i))

  let with_action p f = (fun i -> List.map (fun (e,s) -> (f e, s)) (p i))

  let wrap p i = (p (A.substr i))

  let (_:raw_parser -> substring myparser) = wrap

  (* these combinators are only for raw_parsers *)
  let ( **>@ ) p1 p2 = seq p1 p2

  let ( |||@ ) p1 p2 = alt p1 p2

  let (>>@) p f = with_action p f

  (* string -> substring ty_parser *)
  let a lit = wrap (a lit)

  let (_:substring myparser) = (a "1")

  (* FIXME change this to take an underlying parser *)
  let until_a lit = wrap (until_a lit)

  (* pred is a function from a string of length 1 to a bool *)
  let parse1 pred = wrap (parse1 pred)

  let parse_EOF = wrap (parse_EOF)

  let parse_while pred = wrap (parse_while pred)

  let (_:(string -> bool) -> substring myparser) = parse_while

  (* FIXME could tidy up the following *)

  let parse_azAZ = wrap parse_azAZ

  let (_:substring myparser) = parse_azAZ

  let parse_AZ = wrap parse_AZ

  let parse_az = wrap parse_az

  let parse_azs = wrap parse_azs

  let parse_AZS = wrap parse_AZS

  let parse_ws = wrap parse_ws

  let parse_epsws = wrap parse_epsws

  let parse_newline = wrap parse_newline

  let parse_azAZs = wrap parse_azAZs

  let parse_notdquote = wrap parse_notdquote

  let parse_notsquote = wrap parse_notsquote

  let parse_notlt = wrap parse_notlt

  let parse_notgt = wrap parse_notgt

  let parse_notltgt = wrap parse_notltgt

  let parse_notbracket = wrap parse_notbracket

  let parse_notws = wrap parse_notws

  let parse_notcurlyr = wrap parse_notcurlyr

  let parse_all = wrap parse_all

  let parse_num = wrap parse_num

  let parse_float = wrap parse_float

  let parse_ident = wrap parse_ident

  let term_to_parser s = wrap (term_to_parser s)

  let (_:term -> substring myparser) = term_to_parser

end


