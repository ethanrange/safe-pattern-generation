open Codelib;;

let rec closed_pow (n : int) : (int -> int) code =
  if n = 0 then .<fun _ -> 1>.
  else if n mod 2 = 0 then let hp = closed_pow (n / 2) in .<fun x -> (.~hp x * .~hp x)>.
  else .<fun x -> x * .~(closed_pow (n - 1)) x>.

let rec open_pow (n : int) (x : int code) : int code =
  if n = 0 then .<1>.
  else if n = 1 then x
  else if n mod 2 = 0 then let hp = open_pow (n / 2) x in .<.~hp * .~hp>.
  else .<.~x * .~(open_pow (n - 1) x)>.

let cpower5_c : (int -> int) code = closed_pow 5;;
let cpower5 : int -> int = Runnative.run (closed_pow 5);;

let opower5_c = .<fun x -> .~(open_pow 5 .<x>.)>.;;
let opower5 = Runnative.run opower5_c;;

let () = Codelib.print_code Format.std_formatter cpower5_c; print_newline ()
let () = Codelib.print_code Format.std_formatter opower5_c; print_newline ()

let x = cpower5 2
let () = print_int x; print_newline ()

let y = opower5 2
let () = print_int y; print_newline ()

(* Closed code demo *)

let f : (int -> int) code = .<fun x -> x>.
let a : int = 2
let app : int code = .<.~f a>.

let () = Codelib.print_code Format.std_formatter app;;
