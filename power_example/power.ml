open Codelib;;

let rec closed_pow (n : int) : (int -> int) code =
  if n = 0 then .<fun _ -> 1>.
  else if n mod 2 = 0 then .<fun x -> let y = .~(closed_pow (n / 2)) x in y * y>.
  else .<fun x -> x * .~(closed_pow (n - 1)) x>.

let rec open_pow (n : int) (x : int code) : int code =
  if n = 0 then .<1>.
  else if n = 1 then x
  else if n = 2 then .<.~x * .~x>.
  else if n mod 2 = 0 then .<let y = .~(open_pow (n / 2) x) in y * y>.
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

let rec genlet_pow (n : int) (x : int code) : int code =
  if n = 0 then .<1>.
  else if n = 1 then x
  else if n mod 2 = 0 then let y = genlet ~name:"y" (genlet_pow (n / 2) x) in .<.~y * .~y>.
  else .<.~x * .~(genlet_pow (n - 1) x)>.

let glpower5_c = .<fun x -> .~(genlet_pow 5 .<x>.)>.;;
let glpower5 = Runnative.run glpower5_c;;

let () = Codelib.print_code Format.std_formatter glpower5_c; print_newline ()

let z = glpower5 2
let () = print_int z; print_newline ()
