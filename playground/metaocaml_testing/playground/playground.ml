open Square

let rec power (n: int) (x: int): int =
  if n = 0 then 1
  else if n mod 2 = 0 then square (power (n/2) x)
  else x * (power (n-1) x)

let power7 (x: int): int = power 7 x

let rec spower (n: int) (x: int code): int code =
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square .~(spower (n/2) x)>.
  else .<.~x * .~(spower (n-1) x)>.
(* val spower : int -> int code -> int code = <fun> *)

let spowern: int -> (int -> int) code = fun n -> .<fun x -> .~(spower n .<x>.)>.;;

(* let () = Printf.printf "Specialized power %d ^ %d is %d\n" x 7 (spower_fn x);; *)

let n, x = 7, 20 in
    let spower_fn = Runnative.run (spowern n) in
      Printf.printf "Specialized power %d ^ %d is %d\n"
        x n (spower_fn x);;
(* val spower7 : int -> int = <fun> *)

let () = print_code Format.std_formatter (spowern 100);;

(* let f = fun x_2 -> x_2 * (Square.square (x_2 * (Square.square (x_2 * 1)))) in
Printf.printf "Also %d ^ %d is %d\n" 5 7 (f 5) *)

(* let _ = print_endline "Hello world!" *)
