open Codelib;;
open Utilities.Pat.PatImp;;

(* Unsafe generator *)

let unsafe_gen_n_vars (n: int) : ('a, 'f, 'r) pat =
  let rec loop (n: int) : ('a, 'r) unsafe_pat =   
    if n = 0 
    then unsafe_loosen var
    else unsafe_loosen (var >:: (unsafe_tighten (loop (n - 1))))
in unsafe_tighten (loop (n - 1));;

(* Safe generator *)

let gen_n_vars (n: int) (i : ('a list -> 'r) code) (bs : ('r -> 'r) code): ('a list, 'r) case =
  let rec loop (n : int) : ('a, 'r) patwrap =
    if n = 0 
    then Pat (var, i)
    else let Pat (p, c) = loop (n - 1) in
         Pat (var >:: p, .<fun _ -> .~(modify_fun_body c bs)>.) (*.<fun x -> .~c>.)*)
  in match loop (n - 1) with
    | Pat (p, c) -> p => c

(* type (_,_) n =
    Z : (_ -> 'r,'r) n
  | S : ('r,_ -> 'f) n -> ('r, 'f) n

let rec prepend_int : type f r. (f,r) n -> r -> f = fun n f ->
  match n with
  | Z -> (fun n -> f)
  | S p -> prepend_int p (fun n -> f)
             
let zero = Z
let one = S Z
let two = S (S Z)
    
let rec to_int : type f r. (f,r) n -> int = function
  | Z -> 0
  | S p -> 1 + to_int p
  
let rec from_int : type f r. int -> (f,r) n = function
  | 0 -> Z
  | n -> S (from_int (n - 1))
    
let x = prepend_int two (fun x -> x)
let y = to_int one *)

(* let gen_n_vars_safe : int -> ('a, 'f, 'r) pat = fun n ->
  let rec loop : int -> ('a list, 'f, 'r) pat -> ('a, 'a list -> 'f, 'r) pat = fun n acc_pat ->
    if n = 0 
    then var >:: acc_pat
    else loop (n - 1) (var >:: acc_pat)
in loop (n - 1) var;;

let q : (int, int, int) pat = gen_n_vars_safe 1
let r : (int list, int -> int list, int) pat = gen_n_vars_safe 2 *)
let[@warning "-32-39-27"] unrolled_nmap_example = .<let rec nmap f = .~(function_ [
  gen_n_vars 3 .<fun xs -> 0>. (.<fun acc -> 1 + acc>.)
  (* empty       => .<[]>.;
  (unsafe_gen_n_vars 3) => .<fun x1 x2 xs -> let y1 = f x1 in let y2 = f x2 in y1 :: y2 :: nmap f xs>.;
  (var >:: var) => .<fun x xs -> let y = f x in y :: nmap f xs>. *)
]) in nmap>.;;

(* type 'a fn = Val of 'a | Fn of 'a fn *  *)

(* let x : ('a list, ('a -> 'a list -> 'a list), 'a list) pat = var >:: var
let y : ('a list, ('a -> 'a -> 'a list -> 'a list), 'a list) pat = var >:: (var >:: var) *)

let prepended = .<fun a -> .~(modify_fun_body .<fun x y -> x + y>. .<fun acc -> 1 + acc>.) a>.

let renamed = promote_code (Utilities.Pat.PTReplace.apply_fun (reduce_code .<fun x -> x>.) (reduce_code .<1>.))