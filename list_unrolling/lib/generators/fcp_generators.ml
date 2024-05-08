open Codelib;;
open Utilities.Pat.PatImp;;

(* Unsafe generator *)

let unsafe_gen_n_cons (n: int) : ('a, 'f, 'r) pat =
  let rec loop (n: int) : ('a, 'r) unsafe_pat =   
    if n = 0 
    then unsafe_loosen var
    else unsafe_loosen (var >:: (unsafe_tighten (loop (n - 1))))
in unsafe_tighten (loop (n - 1));;

(* Safe-wrapped unsafe generator *)

let wrapped_unsafe_gen_n_cons (n: int) (i : ('a list -> 'r) code) (bs : ('a -> 'r -> 'r) code): ('a list, 'r) case =
  let rec loop (n : int) : ('a, 'r) unsafe_patwrap =
    if n = 0 
    then Pat (var, i)
    else let Pat (p, c) = loop (n - 1) in
         Pat (var >:: p, .<fun x -> .~(modify_fun_body c bs .<x>.)>.)
  in match loop (n - 1) with
    | Pat (p, c) -> p => c;;

(* Safe generators *)

let gen_n_cons (n: int) (i : 'a list code -> 'r code) (bs : 'a code -> 'r code -> 'r code) : ('a list, 'r) case =
  let rec loop (n : int) : ('a, 'r) patwrap =
    if n = 0 
    then Pat (var, fun k -> .<fun xs -> .~(k (i .<xs>.))>.)
    else let Pat (p, k) = loop (n - 1) in
          Pat (var >:: p, fun c -> .<fun x -> .~(k (fun o -> c (bs .<x>. o)))>.)
  in match loop n with
    | Pat (p, c) -> p => (c Fun.id);;

let gen_exactly_n_cons (n: int) (i : 'r code) (bs : 'a code -> 'r code -> 'r code) : ('a list, 'r) case =
  let rec loop (n : int) : ('a, 'r) patwrap =
    if n = 0 
    then Pat (empty, fun k -> k i)
    else let Pat (p, k) = loop (n - 1) in
          Pat (var >:: p, fun c -> .<fun x -> .~(k (fun o -> c (bs .<x>. o)))>.)
  in match loop n with
    | Pat (p, c) -> p => (c Fun.id);;

(* Example generators *)

let gen_sum_n (n : int) = .<let sum = .~(function_ [
  (* gen_n_cons n (fun _ -> .<0>.) (fun x acc -> .<.~x + .~acc>.); *)
  gen_exactly_n_cons n .<0>. (fun x acc -> .<.~x + .~acc>.);
  __ => let error_msg = Format.sprintf "Sum function only accepts a list of length %d" n in
          .<raise @@ Invalid_argument error_msg>.
]) in sum>.

let gen_unrolled_nmap (n : int) = .<let rec nmap f = .~(function_ [
  empty       => .<[]>.;

  gen_n_cons n (fun xs -> .<nmap f .~xs>.) (fun x acc -> .<let y = f .~x in y :: .~acc>.);
  gen_n_cons 1 (fun xs -> .<nmap f .~xs>.) (fun x acc -> .<let y = f .~x in y :: .~acc>.);

  (* Alt: var >:: var => .<fun x xs -> let y = f x in y :: nmap f xs>. *)
]) in nmap>.

(* Safe get tail of list *)
let gen_list_example : (int list -> int list) code = .<let f = .~(function_ [
  empty       => .<[]>.;
  var >:: var => .<fun _ acc -> acc>.
]) in f>.;;
