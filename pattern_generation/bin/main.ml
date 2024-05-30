let () = print_endline "Hello, World!"

let test_list = [1; 2; 3; 4; 5]
let () = List.iter (Printf.printf "%d ") (List.map succ test_list); print_newline ();;

(* sum_n *)

(* let () = print_endline "\n=== sum (length 3) ===\n"

let sum_3 = Fcp_generators.gen_sum_n 3
let () = Codelib.print_code Format.std_formatter sum_3;;
let sum = Runnative.run sum_3 in print_int (sum [1; 2; 3;]); print_newline ();;

let () = print_endline "\n======================\n" *)

(* nmap *)

let () = print_endline "\n=== nmap (unrolled 2) ===\n"

let nmap_2 = Fcp_generators.gen_unrolled_nmap 2
let () = Codelib.print_code Format.std_formatter nmap_2;;
let nmap = Runnative.run nmap_2 in print_newline (); List.iter (Printf.printf "%d ") (nmap succ [1; 2; 3;]); print_newline ();;

let () = print_endline "\n=========================\n"

(* tail *)

(* let () = print_endline "\n=== rev (int list) ===\n"

let tail = Fcp_generators.gen_list_example
let () = Codelib.print_code Format.std_formatter tail;;
let tail_intl = Runnative.run tail in print_newline (); List.iter (Printf.printf "%d ") (tail_intl [1; 2; 3;]); print_newline ();;

let () = print_endline "\n======================\n" *)

(* let nmap = Runnative.run x in print_int (nmap succ [1; 2; 3;]); print_endline "";; *)

let () = List.iter (Printf.printf "%d ") (Map_impls.Standard.map succ [] test_list); print_newline ()