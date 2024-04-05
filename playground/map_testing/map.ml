(* Original code by Jeremy Yallop *)

(* 
  Install 5.1.0 with flambda enabled:
   
    opam switch create 5.1.0-flambda --packages=ocaml-option-flambda 

  Install core_bench

    opam install --yes core_bench

  Build the benchmark

    ocamlfind opt -thread -package core_unix.command_unix,core_bench -linkpkg map.ml

  Run the benchmark

    /a.out -quota 0.5

Example output:

   Estimated testing time 2s (4 benchmarks x 500ms). Change using '-quota'.
   ┌────────┬──────────┬──────────┬──────────┬──────────┬────────────┐
   │ Name   │ Time/Run │  mWd/Run │ mjWd/Run │ Prom/Run │ Percentage │
   ├────────┼──────────┼──────────┼──────────┼──────────┼────────────┤
   │ map:0  │   8.89ms │ 602.93kw │ 388.29kw │ 381.70kw │     96.51% │
   │ umap:0 │   9.21ms │ 600.52kw │ 405.51kw │ 396.78kw │    100.00% │
   │ cmap:0 │   5.71ms │ 432.96kw │ 290.03kw │ 283.02kw │     62.04% │
   │ tmap:0 │   6.23ms │ 304.20kw │ 305.35kw │ 304.19kw │     67.69% │
   └────────┴──────────┴──────────┴──────────┴──────────┴────────────┘

 *)

                                           
(* Standard map, no optimizations *)
let rec rev acc = function
  | [] -> acc
  | x :: xs -> rev (x :: acc) xs

let rec map f acc = function
  | [] -> rev [] acc
  | x :: xs ->
     map f (f x :: acc) xs

                                           
(* Non-tail-recursive map *)
let[@tail_mod_cons] rec nmap f = function
  | [] -> []
  | x :: xs -> let y = f x in y :: nmap f xs

let[@tail_mod_cons] rec nmap_unrolled2_handwritten f = function
  | [] -> []
  | x1 :: x2 :: xs -> let y1 = f x1 and y2 = f x2 in y1 :: y2 :: nmap_unrolled2_handwritten f xs
  | x :: xs -> let y = f x in y :: nmap_unrolled2_handwritten f xs

  let[@tail_mod_cons] rec nmap_unrolled2_generated f_2 x_3 =
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

let[@tail_mod_cons] rec nmap_unrolled8_handwritten f = function
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

(* Unrolled map *)
let rec urev acc = function
  | [] -> acc
  | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: xs -> urev (x6::x5::x4::x3::x2::x1::acc) xs
  | x :: xs -> urev (x :: acc) xs

let rec umap f acc = function
  | [] -> urev [] acc
  | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: xs ->
     let y1 = f x1 in
     let y2 = f x2 in
     let y3 = f x3 in
     let y4 = f x4 in
     let y5 = f x5 in
     let y6 = f x6 in
     umap f (y6 :: y5 :: y4 :: y3 :: y2 :: y1 :: acc) xs
  | x :: xs ->
     umap f (f x :: acc) xs

type 'a clist =
  Nil
| Cons of 'a * 'a clist
| Conses of 'a * 'a * 'a * 'a * 'a * 'a * 'a clist

(* Unrolled map, with unrolled data structure (8 word cons cells) *)
let rec crev acc = function
  | Nil -> acc
  | Cons (x, xs) -> crev (x :: acc) xs
  | Conses (x1,x2,x3,x4,x5,x6,xs) -> crev (x6::x5::x4::x3::x2::x1::acc) xs

let rec cmap f acc = function
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

type 'a clist2 =
  Nil
| Cons of 'a * 'a clist2
| Conses of 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a *'a * 'a clist2

(* Unrolled map, with unrolled data structure (16 word cons cells) *)
let rec crev2 acc = function
  | Nil -> acc
  | Cons (x, xs) -> crev2 (x :: acc) xs
  | Conses (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,xs) -> crev2 (x14::x13::x12::x11::x10::x9::x8::x7::x6::x5::x4::x3::x2::x1::acc) xs

let rec cmap2 f acc = function
  | [] -> crev2 [] acc
  | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: x7 :: x8 :: x9 :: x10 :: x11 :: x12 :: x13 :: x14 :: xs ->
     let y1 = f x1 in
     let y2 = f x2 in
     let y3 = f x3 in
     let y4 = f x4 in
     let y5 = f x5 in
     let y6 = f x6 in
     let y7 = f x7 in
     let y8 = f x8 in
     let y9 = f x9 in
     let y10 = f x10 in
     let y11 = f x11 in
     let y12 = f x12 in
     let y13 = f x13 in
     let y14 = f x14 in
     cmap2 f (Conses (y14, y13, y12, y11, y10, y9, y8, y7, y6, y5, y4, y3, y2, y1, acc)) xs
  | x :: xs ->
     cmap2 f (Cons (f x, acc)) xs



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

(* Check the types *)
let _map : 'a 'b. ('a -> 'b) -> 'b list -> 'a list -> 'b list = map
let _umap : 'a 'b. ('a -> 'b) -> 'b list -> 'a list -> 'b list = umap
let _cmap : 'a 'b. ('a -> 'b) -> 'b clist -> 'a list -> 'b list = cmap
let _cmap2 : 'a 'b. ('a -> 'b) -> 'b clist2 -> 'a list -> 'b list = cmap2
let _tmap : 'a 'b. ('a -> 'b) -> 'a list -> 'b list = trmc_map

let big = List.init 100_000 Fun.id 

(* Check the behaviour *)
let () = begin
    let m = map succ [] big
    and n = nmap succ big

    and nu2h = nmap_unrolled2_handwritten succ big
    and nu2g = nmap_unrolled2_generated succ big
    and nu8h = nmap_unrolled8_handwritten succ big
    and nu8g = nmap_unrolled8_generated succ big

    and u = umap succ [] big
    and c = cmap succ Nil big
    and c2 = cmap2 succ Nil big
    and t = trmc_map succ big in
    assert (m = u);
    assert (m = n);
    assert (m = c);
    assert (m = c2);
    assert (m = t);

    assert (m = nu2h);
    assert (m = nu2g);
    assert (m = nu8h);
    assert (m = nu8g)
  end


let map_bench _ = Core.Staged.stage (fun () -> ignore (map succ [] big))
let nmap_bench _ = Core.Staged.stage (fun () -> ignore (nmap succ big))
let nmap_unrolled2_handwritten_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled2_handwritten succ big))

let nmap_unrolled2_generated_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled2_generated succ big))

let nmap_unrolled8_handwritten_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled8_handwritten succ big))

let nmap_unrolled8_generated_bench _ = Core.Staged.stage (fun () -> ignore (nmap_unrolled8_generated succ big))

let umap_bench _ = Core.Staged.stage (fun () -> ignore (umap succ [] big))
let cmap_bench _ = Core.Staged.stage (fun () -> ignore (cmap succ Nil big))
let cmap2_bench _ = Core.Staged.stage (fun () -> ignore (cmap2 succ Nil big))
let tmap_bench _ = Core.Staged.stage (fun () -> ignore (trmc_map succ big))

open Core
open Core_bench

let args = [0]
let () =
  Command_unix.run (Bench.make_command [
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
  ])
