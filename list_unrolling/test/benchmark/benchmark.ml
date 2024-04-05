let () = print_string "This is the benchmark!"

(* 
let rec nmap_unrolled2_handwritten f = function
  | [] -> []
  | x1 :: x2 :: xs -> let y1 = f x1 and y2 = f x2 in y1 :: y2 :: nmap_unrolled2_handwritten f xs
  | x :: xs -> let y = f x in y :: nmap_unrolled2_handwritten f xs

let rec nmap_unrolled2_generated f_2 x_3 =
    match x_3 with
    | [] -> []
    | xs_4 ->
        ((fun nm_5 ->
            fun f_6 ->
              function
              | x_7::xs_8 ->
                  let y_9 = f_6 x_7 in y_9 ::
                    (((fun nm_10 ->
                         fun f_11 ->
                           function
                           | x_12::xs_13 ->
                               let y_14 = f_11 x_12 in y_14 ::
                                 (((fun nm_15 ->
                                      fun f_16 -> fun xs_17 -> nm_15 f_16 xs_17))
                                    nm_10 f_11 xs_13)
                           | [] -> [])) nm_5 f_6 xs_8)
              | [] -> [])) nmap_unrolled2_generated f_2 xs_4

let rec nmap_unrolled8_handwritten f = function
| [] -> []
| x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: xs -> 
     let y1 = f x1 in
     let y2 = f x2 in
     let y3 = f x3 in
     let y4 = f x4 in
     let y5 = f x5 in
     let y6 = f x6 in  
     y1 :: y2 :: y3 :: y4 :: y5 :: y6 :: nmap_unrolled8_handwritten f xs
| x :: xs -> let y = f x in y :: nmap_unrolled8_handwritten f xs

let rec nmap_unrolled8_generated f_2 x_3 =
  match x_3 with
  | [] -> []
  | xs_4 ->
      ((fun nm_5 ->
          fun f_6 ->
            function
            | x_7::xs_8 ->
                let y_9 = f_6 x_7 in y_9 ::
                  (((fun nm_10 ->
                       fun f_11 ->
                         function
                         | x_12::xs_13 ->
                             let y_14 = f_11 x_12 in y_14 ::
                               (((fun nm_15 ->
                                    fun f_16 ->
                                      function
                                      | x_17::xs_18 ->
                                          let y_19 = f_16 x_17 in y_19 ::
                                            (((fun nm_20 ->
                                                 fun f_21 ->
                                                   function
                                                   | x_22::xs_23 ->
                                                       let y_24 = f_21 x_22 in
                                                       y_24 ::
                                                         (((fun nm_25 ->
                                                              fun f_26 ->
                                                                function
                                                                | x_27::xs_28
                                                                    ->
                                                                    let y_29
                                                                    =
                                                                    f_26 x_27 in
                                                                    y_29 ::
                                                                    (((fun
                                                                    nm_30 ->
                                                                    fun f_31
                                                                    ->
                                                                    function
                                                                    | 
                                                                    x_32::xs_33
                                                                    ->
                                                                    let y_34
                                                                    =
                                                                    f_31 x_32 in
                                                                    y_34 ::
                                                                    (((fun
                                                                    nm_35 ->
                                                                    fun f_36
                                                                    ->
                                                                    function
                                                                    | 
                                                                    x_37::xs_38
                                                                    ->
                                                                    let y_39
                                                                    =
                                                                    f_36 x_37 in
                                                                    y_39 ::
                                                                    (((fun
                                                                    nm_40 ->
                                                                    fun f_41
                                                                    ->
                                                                    function
                                                                    | 
                                                                    x_42::xs_43
                                                                    ->
                                                                    let y_44
                                                                    =
                                                                    f_41 x_42 in
                                                                    y_44 ::
                                                                    (((fun
                                                                    nm_45 ->
                                                                    fun f_46
                                                                    ->
                                                                    fun xs_47
                                                                    ->
                                                                    nm_45
                                                                    f_46
                                                                    xs_47))
                                                                    nm_40
                                                                    f_41
                                                                    xs_43)
                                                                    | 
                                                                    [] -> []))
                                                                    nm_35
                                                                    f_36
                                                                    xs_38)
                                                                    | 
                                                                    [] -> []))
                                                                    nm_30
                                                                    f_31
                                                                    xs_33)
                                                                    | 
                                                                    [] -> []))
                                                                    nm_25
                                                                    f_26
                                                                    xs_28)
                                                                | [] -> []))
                                                            nm_20 f_21 xs_23)
                                                   | [] -> [])) nm_15 f_16
                                               xs_18)
                                      | [] -> [])) nm_10 f_11 xs_13)
                         | [] -> [])) nm_5 f_6 xs_8)
            | [] -> [])) nmap_unrolled8_generated f_2 xs_4

(* Map as written in the stdlib, unrolled one level and using tmrc *)
let[@tail_mod_cons] rec trmc_map f = function
    [] -> []
  | [a1] ->
      let r1 = f a1 in
      [r1]
  | a1::a2::l ->
      let r1 = f a1 in
      let r2 = f a2 in
      r1::r2::trmc_map f l

let _tmap : 'a 'b. ('a -> 'b) -> 'a list -> 'b list = trmc_map

let test_list = List.init 100_000 Fun.id 

(* Check the behaviour *)
let () = 
    let m = Standard_impl.map succ [] test_list
    and n = nmap succ test_list

    and nu2h = nmap_unrolled2_handwritten succ test_list
    and nu2g = nmap_unrolled2_generated succ test_list
    and nu8h = nmap_unrolled8_handwritten succ test_list
    and nu8g = nmap_unrolled8_generated succ test_list

    and u = umap succ [] test_list
    and c = cmap succ Nil test_list
    and c2 = cmap2 succ Nil test_list
    and t = trmc_map succ test_list in
    assert (m = u);
    assert (m = n);
    assert (m = c);
    assert (m = c2);
    assert (m = t);

    assert (m = nu2h);
    assert (m = nu2g);
    assert (m = nu8h);
    assert (m = nu8g)

let map_bench _ = Core.Staged.stage (fun () -> ignore (map succ [] test_list))
let nmap_bench _ = Core.Staged.stage (fun () -> ignore (nmap succ test_list))
let nmap_unrolled2_handwritten_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled2_handwritten succ test_list))

let nmap_unrolled2_generated_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled2_generated succ test_list))

let nmap_unrolled8_handwritten_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled8_handwritten succ test_list))

let nmap_unrolled8_generated_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled8_generated succ test_list))

let umap_bench _ = Core.Staged.stage (fun () -> ignore (umap succ [] test_list))
let cmap_bench _ = Core.Staged.stage (fun () -> ignore (cmap succ Nil test_list))
let cmap2_bench _ = Core.Staged.stage (fun () -> ignore (cmap2 succ Nil test_list))
let tmap_bench _ = Core.Staged.stage (fun () -> ignore (trmc_map succ test_list))

open Core
open Core_bench

let args = [0]
let () = Command_unix.run (Bench.make_command [
    Bench.Test.create_indexed ~name:"map" ~args map_bench;
    Bench.Test.create_indexed ~name:"nmap" ~args nmap_bench;

    Bench.Test.create_indexed ~name:"nmap [unrolled2] handwritten" ~args nmap_unrolled2_handwritten_bench;
    Bench.Test.create_indexed ~name:"nmap [unrolled2] generated" ~args nmap_unrolled2_generated_bench;

    Bench.Test.create_indexed ~name:"nmap [unrolled8] handwritten" ~args nmap_unrolled8_handwritten_bench;
    Bench.Test.create_indexed ~name:"nmap [unrolled8] generated" ~args nmap_unrolled8_generated_bench;

    Bench.Test.create_indexed ~name:"umap" ~args umap_bench;
    Bench.Test.create_indexed ~name:"cmap" ~args cmap_bench;
    Bench.Test.create_indexed ~name:"cmap2" ~args cmap2_bench;
    Bench.Test.create_indexed ~name:"tmap" ~args tmap_bench;
  ]) *)
