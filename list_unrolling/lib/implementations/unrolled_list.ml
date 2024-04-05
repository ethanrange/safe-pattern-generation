(* Unrolled map - chunks of 6 *)

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