open Map_impls;;

(* Map as written in the stdlib, unrolled one level and using trmc *)
let[@tail_mod_cons] rec trmc_map f = function
  | [] -> []
  | [x] -> let y = f x in [y]
  | x1 :: x2 :: xs -> let y1 = f x1 in
                      let y2 = f x2 in
                      y1 :: y2 :: trmc_map f xs

let rec unrolled2_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 2
let rec unrolled4_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 4
let rec unrolled8_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 8
let rec unrolled16_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 16
let rec unrolled32_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 32
let rec unrolled64_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 64
let rec unrolled128_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 128
let rec unrolled256_gen = Runnative.run @@ Fcp_generators.gen_unrolled_nmap 256

let () = print_endline "Generated unrolled map functions!"

type ('a, 'b) map_type = string * (('a -> 'b) -> 'a list -> 'b list)

let test_list = List.init 100_000 Fun.id
let incr_list = List.init 100_000 (fun x -> x + 1)

let map_functions : (('a, 'b) map_type) list = [
  "Non TR map", Standard.nmap;

  "Stdlib map", List.map;
  "TRMC map", trmc_map;

  "2 unrolled HW", Hand_unrolled.map2_hw;
  "2 unrolled GEN", unrolled2_gen;

  "4 unrolled HW", Hand_unrolled.map4_hw;
  "4 unrolled GEN", unrolled4_gen;

  "8 unrolled HW", Hand_unrolled.map8_hw;
  "8 unrolled GEN", unrolled8_gen;

  "16 unrolled HW", Hand_unrolled.map16_hw;
  "16 unrolled GEN", unrolled16_gen;

  "32 unrolled HW", Hand_unrolled.map32_hw;
  "32 unrolled GEN", unrolled32_gen;

  "64 unrolled HW", Hand_unrolled.map64_hw;
  "64 unrolled GEN", unrolled64_gen;

  "128 unrolled HW", Hand_unrolled.map128_hw;
  "128 unrolled GEN", unrolled128_gen;

  "256 unrolled HW", Hand_unrolled.map256_hw;
  "256 unrolled GEN", unrolled256_gen;
]

(* Check the behaviour *)
let all_same : ((int, int) map_type) list -> bool = 
  List.fold_left (fun acc (_, f) -> acc && (f succ test_list = incr_list)) true

let () = assert (all_same map_functions)

let map_benches = List.map (fun (n, f) -> (n, fun _ -> Core.Staged.stage (fun () -> ignore (f succ test_list)))) map_functions

open Core_bench

let args = [0]

let () = Command_unix.run (Bench.make_command 
  (List.map (fun (n, b) -> Bench.Test.create_indexed ~name:n ~args b) map_benches)
)
