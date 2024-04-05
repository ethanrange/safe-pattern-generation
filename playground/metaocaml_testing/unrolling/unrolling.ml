(* Target list *)
let big = List.init 100_000 Fun.id

(* Simple map defined with rev *)

let rec rev acc = function
  | [] -> acc
  | x :: xs -> rev (x :: acc) xs

let rec map f acc = function
  | [] -> rev [] acc
  | x :: xs ->
     map f (f x :: acc) xs

(* Unrolled DS and map *)

type 'a clist =
  Nil
| Cons of 'a * 'a clist
| Conses of 'a * 'a * 'a * 'a * 'a * 'a * 'a clist

(* Unrolled map, with unrolled data structure (8 word cons cells) *)
let rec crev (acc: 'a list): 'a clist -> 'a list = function
  | Nil -> acc
  | Cons (x, xs) -> crev (x :: acc) xs
  | Conses (x1,x2,x3,x4,x5,x6,xs) -> crev (x6::x5::x4::x3::x2::x1::acc) xs

let rec cmap (f: 'a -> 'b) (acc: 'b clist): ('a list -> 'b list) = function
  | [] -> crev [] acc
  | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: xs ->
    let y1 = f x1 in
    let y2 = f x2 in
    let y3 = f x3 in
    let y4 = f x4 in
    let y5 = f x5 in
    let y6 = f x6 in
    cmap f (Conses (y6, y5, y4, y3, y2, y1, acc)) xs
  | x :: xs ->
    cmap f (Cons (f x, acc)) xs

(* Attempts at recursive *)

(* let rec mkRecursive m = with_locus_rec @@ fun l ->
  let g = mkgenlet l (=) in
  let rec nmap m = .<fun n -> .~(g nmap m) 1>. in g nmap m

let () = print_code Format.std_formatter (mkRecursive 2);; *)

(* TMC map generation *)

(* (n: int): (('a -> 'b) -> 'a list -> 'b list) code *)

let gen_nmap (n: int) = .<let rec nmap f = function 
  | [] -> []
  | (x :: xs) -> let y = f x in y :: nmap f xs
in nmap>.;;

(* let () = print_code Format.std_formatter (gen_nmap 2);; *)
(* let nmap = Runnative.run (gen_nmap 2) in List.iter (Printf.printf "%d ") (nmap succ [1; 2; 3;]); print_endline "";; *)

(* let gen_nmap = .<let rec nmap f l = .~(make_match .<l>. [
      .<fun [] -> []>. [@metaocaml.functionliteral];
      .<fun (x :: xs) -> let y = f x in y :: nmap f xs>. [@metaocaml.functionliteral]
  ]) in nmap>.;;

(* let () = print_code Format.std_formatter (gen_nmap);; *)
let nmap_fun = Runnative.run (gen_nmap) in List.iter (Printf.printf "%d ") (nmap_fun succ [1; 2; 3;]); print_endline "";; *)


(* [@metaocaml.functionliteral] *)
  (* .~(make_match .<x>. @@ 
    [.<fun [] -> []>. [@metaocaml.functionliteral]] @
    [.<fun (x :: xs) -> let y = f x in [y]>. [@metaocaml.functionliteral]]
  )>.;; *)

(* .~(make_match .<x>. @@ [.<fun [] -> []>. [@metaocaml.functionliteral]]) *)
  (* [.<fun (x :: xs) -> let y = f x in y :: nmap f>. xs [@metaocaml.functionliteral]] *)

(* let () = print_code Format.std_formatter (gen_nmap);; *)

(* let nmap_fun = Runnative.run (gen_nmap) in List.iter (Printf.printf "%d ") (nmap_fun succ [1; 2; 3;]); print_endline "";; *)

(* Loop unrolled generation *)

(* : ('a list -> 'a list) pat_code :> ('a list -> 'a list) code *)
(* : ('a list -> 'a list) pat_code *)

open Option;;

let unrolled_workaround (n: int) =
  let rec bool_loop (n: int): (('a -> 'b) -> 'a list -> ('b list * 'a list) option) code =
    if n = 0 then .<fun f rem -> some ([], rem)>. else .<fun f -> function 
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

(* let bool_loop = Runnative.run (n_pattern_match 2) in List.iter (Printf.printf "%d ") (snd (get (bool_loop [1; 2; 3; 4;]))); print_endline "";; *)

let n_pattern_match (n: int) =
  let rec loop (n: int): ('c -> ('a -> 'b) -> 'a list -> 'b list) code = 
      if n = 0 then .<fun nm f xs -> nm f xs>. 
               else .<fun nm f -> function 
                | x :: xs -> let y = f x in y :: .~(loop (n - 1)) nm f xs
                | [] -> []
                >.

  (* in loop n;; *)
  
  in .<let rec nmap f x = .~(make_match .<x>. [
    .<fun [] -> []>. [@metaocaml.functionliteral];
    .<fun xs -> .~(loop n) nmap f xs>. [@metaocaml.functionliteral];
    ]) in nmap>.;;

let () = print_code Format.std_formatter (n_pattern_match 8);;
(* let nmap = Runnative.run (n_pattern_match 2) in List.iter (Printf.printf "%d ") (nmap succ [1; 2; 3; 4; 5;]); print_endline "";; *)

(* let () = print_code Format.std_formatter (n_pattern_match 2);; *)
(* let loop = Runnative.run (n_pattern_match 2) in List.iter (Printf.printf "%d ") (loop succ [1; 2; 3;]); print_endline "";; *)