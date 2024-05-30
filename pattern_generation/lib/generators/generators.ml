open Codelib;;

(* Generate the standard, non-tail recursive map definition without unrolling *)

let gen_nmap = .<let rec nmap f = function 
  | [] -> []
  | (x :: xs) -> let y = f x in y :: nmap f xs
in nmap>.;;

(* Alternative definition with match statement *)
let[@warning "-8"] gen_match_nmap = .<let rec nmap f l = .~(make_match .<l>. [
      .<fun [] -> []>. [@metaocaml.functionliteral];
      .<fun (x :: xs) -> let y = f x in y :: nmap f xs>. [@metaocaml.functionliteral]
  ]) in nmap>.;;

(* Generate non-tail recursive, n-unrolled map definitions *)

(* Ugly, slow workaround with pattern guards *)
open Option;;

let unrolled_workaround (n: int) =
  let rec bool_loop (n: int): (('a -> 'b) -> 'a list -> ('b list * 'a list) option) code =
    if n = 0 then .<fun _ rem -> some ([], rem)>. else .<fun f -> function 
      | x :: xs -> (match (.~(bool_loop (n - 1)) f xs) with
                    | None -> none
                    | Some (ys, rem) -> some (f x :: ys, rem)
                   )
      | _ -> none
      >.

    in .<let rec nmap f = function
    | [] -> []
    | p when is_some(.~(bool_loop n) f p) -> let ys, rem = get (.~(bool_loop n) f p) in ys @ nmap f rem
    | x :: xs -> let y = f x in y :: nmap f xs
  in nmap>.;;


(* Pattern match unrolling *)

let gen_unrolled_nmap (n: int) =
  let rec loop (n: int): ('c -> ('a -> 'b) -> 'a list -> 'b list) code = 
      if n = 0 
      then .<fun nm f xs -> nm f xs>. 
      else .<fun nm f -> function 
                          | x :: xs -> let y = f x in y :: .~(loop (n - 1)) nm f xs
                          | [] -> []
           >.  
  in .<let rec nmap f = function
    | [] -> []
    | xs -> .~(loop n) nmap f xs 
  in nmap>.;;

(* let nmap = Runnative.run (gen_unrolled_nmap 2) in List.iter (Printf.printf "%d ") (nmap succ [1; 2; 3; 4; 5;]); print_endline "";; *)
(* let () = print_code Format.std_formatter (gen_unrolled_nmap 2);; *)