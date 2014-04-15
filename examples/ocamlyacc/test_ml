(* 

    #directory "../../src//p1";;
    #mod_use "p1_terminal_parsers.ml";;
    #mod_use "p1_core.ml";;
    #mod_use "p1_everything.ml";;
    #mod_use "p1_lib.ml";;  

    #directory "../../src//earley2";;
    #mod_use "earley2.ml";;

    #directory "../../src//p3_v2";;
    #mod_use "p3_core.ml";;
    #mod_use "p3_extra.ml";;
    #mod_use "p3_everything.ml";;
    #mod_use "p3_lib.ml";;

*)

open P3_lib
open P3_everything
open P3_basic_parsers

let p3_run_parser = p3_run_parser_string

(* example arith *)

let never = (parse_never >>>> (fun _ -> failwith "never"))

let w = parse_epsws

let rec parse_A h = (fun i -> mkntparser "arithexp" (
  (((parse_A h) ***> w ***> (a "+") ***> w ***> (parse_A h))
   >>>> (fun (e1,(_,(_,(_,e2)))) -> `Plus(e1,e2)))
  |||| (((parse_A h) ***> w ***> (a "-") ***> w ***> (parse_A h))
        >>>> (fun (e1,(_,(_,(_,e2)))) -> `Minus(e1,e2)))
  |||| (parse_num 
        >>>> (fun s -> `Num(int_of_string (content s))))
  |||| (((a "(") ***> w ***> (parse_A h) ***> w ***> (a ")"))  (* brackets *)
        >>>> (fun (_,(_,(e,(_,_)))) -> e))
  |||| h)
  i)

let txt = "1+2-3"

let _ = (p3_run_parser (parse_A never) txt)


(* example boolean expressions *)

(* we include "if then else here", even though it is not a boolean expression *)

let rec parse_B h = (fun i -> mkntparser "boolexp" (
  ((a "true") >>>> (fun _ -> `Bool true))
  |||| ((a "false") >>>> (fun _ -> `Bool false))
  |||| (((a "if") ***> w ***> (parse_B h) ***> w ***> 
            (a "then") ***> w ***> (parse_B h) ***> w ***> 
            (a "else") ***> w ***> (parse_B h))
        >>>> (fun (_,(_,(b,(_,(_,(_,(e1,(_,(_,(_,e2)))))))))) -> `Ite(b,e1,e2)))
  |||| (((parse_B h) ***> w ***> (a "<") ***> w ***> (parse_B h)) 
        >>>> (fun (e1,(_,(_,(_,e2)))) -> `Lt(e1,e2)))
  |||| h)
  i)

let txt = "if true < false then false else true"
let _ = (p3_run_parser (parse_B never) txt)


(* example lambda calculus *)

let c = content

let parse_var = ((a "f") |||| (a "g") |||| (a "x") |||| (a "y") |||| (a "z")) >>>> c

let rec parse_L h = (fun i -> mkntparser "lambdaexp" (
  (((a "\\") ***> w ***> parse_var ***> w ***> (parse_L h))  (* lam *)
   >>>> (fun (_,(_,(x,(_,body)))) -> `Lam(x,body)))
  |||| (((parse_L h) ***> w ***> (parse_L h))  (* app *)
        >>>> (fun (e1,(_,e2)) -> `App(e1,e2)))
  |||| (parse_var >>>> (fun s -> `Var s))  (* var *)
  |||| (((a "(") ***> w ***> (parse_L h) ***> w ***> (a ")"))  (* brackets *)
        >>>> (fun (_,(_,(e,(_,_)))) -> e))
  |||| h)
  i)

let txt = "\\ x x"

let _ = p3_run_parser (parse_L never) txt




(* example arith and lambda and bool mutually recursive *)

(*
let parse_U = (
  let rec l i = parse_L (ar |||| bl) i 
  and ar i = parse_A (l |||| bl) i 
  and bl i = parse_B (l |||| ar) i
  in
  l)
*)

(* Are we sure that parsing terminates with this defn of h? Yes! *)
let parse_U = (
  let rec l i = parse_L h i 
  and a i = parse_A h i 
  and b i = parse_B h i
  and h i = (l |||| a |||| b) i
  in
  l)

(* version with memoization *)
let parse_memo_U () = (
  let tbl = MyHashtbl.create 100 in
  let rec l i = memo_p3 tbl (parse_L h) i 
  and a i = memo_p3 tbl (parse_A h) i 
  and b i = memo_p3 tbl (parse_B h) i
  and h i = memo_p3 tbl (l |||| a |||| b) i
  in
  l)

let parse_memo_U () = (
  let tbl1 = MyHashtbl.create 100 in
  let tbl2 = MyHashtbl.create 100 in
  let tbl3 = MyHashtbl.create 100 in
  let tbl4 = MyHashtbl.create 100 in
  let rec l i = memo_p3 tbl1 (parse_L h) i 
  and a i = memo_p3 tbl2 (parse_A h) i 
  and b i = memo_p3 tbl3 (parse_B h) i
  and h i = memo_p3 tbl4 (l |||| a |||| b) i
  in
  h)

let txt = "\\ x if (1+((\\ y y) 2)) < 4 then true else false"
let _ = (p3_run_parser parse_U txt)
(* 
[`Lam
   ("x",
    `Ite
      (`Lt (`Plus (`Num 1, `App (`Lam ("y", `Var "y"), `Num 2)), `Num 4),
       `Bool true, `Bool false))]
*)

let txt = "if (\\ x x + 2) 1 < 4 then (\\ x x) true else false"
let _ = (p3_run_parser (parse_memo_U ()) txt)

(* key point: don't worry about termination of the parser! the library takes care of that *)


(* example arith calculator *)

let rec eval_A h x = (match x with
  | `Plus(e1,e2) -> (match (eval_A h e1,eval_A h e2) with
    | (`Num x, `Num y) -> `Num(x+y)
    | (x,y) -> `Err("eval_A 1",`Plus(x,y)))
  | `Minus(e1,e2) -> (match (eval_A h e1,eval_A h e2) with
    | (`Num x, `Num y) -> `Num(x-y)
    | (x,y) -> `Err("eval_A 2",`Minus(x,y)))
  | `Num x -> (`Num x)
  | _ -> h x)

let parse_and_eval txt = (
  p3_run_parser
    (parse_A never >>>> (fun e -> eval_A (fun x -> `Err("eval_A 3",x)) e))
    txt)

let _ = parse_and_eval "1+2-3"  



(* example bool evaluator *)
let rec eval_B h x = (match x with
  | `Bool b -> x
  | `Ite(b1,e1,e2) -> (
    let b1 = eval_B h b1 in
    match b1 with
    | `Bool true -> (
      let v1 = eval_B h e1 in
      v1
    )
    | `Bool false -> (
      let v2 = eval_B h e2 in
      v2)
    | _ -> `Err("eval_B 1",b1))
  | `Lt(e1,e2) -> (
    let v1 = eval_B h e1 in
    let v2 = eval_B h e2 in
    match (v1,v2) with
    | (`Num x, `Num y) -> `Bool(x<y)
    | _ -> `Err("eval_B 2",`Lt(v1,v2)))
  | _ -> h x)

let parse_and_eval txt = (
  p3_run_parser 
    (parse_B never >>>> (eval_B (fun x -> `Err("eval_B 3",x))))
    txt)

let _ = parse_and_eval "if true then false else true"    



(* example lambda calc evaluator *)

let fupdate f (x,v) = (fun z -> if z=x then v else f z)

let empty_env = fun x -> `Err("eval_l 1",`Var(x))

(* CBN, values are closures, top-down, no eval under lam *)
let rec eval_L h env x = (match x with
  | `App(e1,e2) -> (
    let v1 = eval_L h env e1 in
    match v1 with
    | `Clos(env1,`Lam((x:string),e1)) -> (
      let env' = fupdate env1 (x,`Clos(env,e2)) in
      eval_L h env' e1)
    | _ -> `Err("eval_L 2",`App(v1,e2)))
  | `Lam(x,e) -> (`Clos(env,`Lam(x,e)))
  | `Var(x) -> (eval_L h env (env x))
  | `Clos(env,e) -> (eval_L h env e)  (* have to allow further processing *)
  | _ -> h env x)

let parse_and_eval txt = (
  p3_run_parser 
    (parse_L never >>>> (fun e -> eval_L (fun env x -> `Err("eval_L 3",x)) empty_env e))
    txt)

let _ = parse_and_eval "(\\ y (y y)) (\\ x x)"



(* example arith and bool and lambda calculator *)


(* termination argument: any parsed construct must be handled by one
   of the evaluators; eval_err prevents `Err from being passed around
   endlessly *)

let eval = 
  let rec l env x = eval_L a env x
  and a env x = eval_A (b env) x
  and b env x = eval_B (eval_err env) x
  and eval_err env x = (match x with
    | `Err x -> `Err x
    | _ -> (l env x))
  in
  l

let remove_err xs = unique (List.filter (fun x -> match x with `Err _ -> false | _ -> true) xs)

let parse_and_eval txt = (remove_err (
  p3_run_parser 
    ((parse_memo_U ()) >>>> eval empty_env)
    txt))

let txt = "if ((\\ x (x + 2)) 1) < 4 then ((\\ x x) true) else false"
let txt = "if (\\ x x + 2) 1 < 4 then (\\ x x) true else false"
let _ = parse_and_eval txt
(* [`Bool true] *)


let parse_and_eval txt = (
  p3_run_parser 
    ((parse_memo_U ()) >>>> eval empty_env)
    txt)

(* y combinator *)
let y = "(\\ f ((\\ x (f (x x))) (\\ x (f (x x)))))"
let [y_exp] = p3_run_parser (parse_memo_U ()) y

(* sigma; let rec sigma x = if x < 2 then 1 else x+(sigma (x-1)) *)
let sigma = "(\\ g (\\ x (if (x < 2) then 1 else (x+(g (x-1))))))"
(* following is a lambda calc version of the sigma function, applied to argument 5 *)
let txt = "("^y^" "^sigma^") 5"
let [r] = parse_and_eval txt
(* `Num 15 *)
let x = match r with `Num x -> x | _ -> failwith "sigma 5 didn't evaluate to a number"

let _ = print_endline ("sigma 5 = "^(string_of_int x))
