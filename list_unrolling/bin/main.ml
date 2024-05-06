let () = print_endline "Hello, World!"

let test_list = [1; 2; 3; 4; 5]
let () = List.iter (Printf.printf "%d ") (List.map succ test_list); print_newline ()

let x = Fcp_generators.unrolled_nmap_example
let () = Codelib.print_code Format.std_formatter x;;

(* let nmap = Runnative.run x in print_int (nmap succ [1; 2; 3;]); print_endline "";; *)

let () = List.iter (Printf.printf "%d ") (Map_impls.Standard.map succ [] test_list); print_newline ()