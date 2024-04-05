let () = print_endline "Hello, World!"

let test_list = [1; 2; 3; 4; 5]
let () = List.iter (Printf.printf "%d ") (List.map succ test_list); print_newline ()

(* let x = Generators.gen_nmap
let () = Codelib.print_code Format.std_formatter x *)

let () = List.iter (Printf.printf "%d ") (Map_impls.Standard.map succ [] test_list); print_newline ()