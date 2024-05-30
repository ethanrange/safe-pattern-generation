(* 8 word cons cell DS *)

type 'a clist8 =
  Nil
| Cons of 'a * 'a clist8
| Conses of 'a * 'a * 'a * 'a * 'a * 'a * 'a clist8

(* Unrolled map, with unrolled data structure (8 word cons cells) *)
let rec crev8 (acc: 'a list): 'a clist8 -> 'a list = function
  | Nil -> acc
  | Cons (x, xs) -> crev8 (x :: acc) xs
  | Conses (x1,x2,x3,x4,x5,x6,xs) -> crev8 (x6::x5::x4::x3::x2::x1::acc) xs

let rec cmap8 (f: 'a -> 'b) (acc: 'b clist8): 'a list -> 'b list = function
  | [] -> crev8 [] acc
  | x1 :: x2 :: x3 :: x4 :: x5 :: x6 :: xs ->
     let y1 = f x1 in
     let y2 = f x2 in
     let y3 = f x3 in
     let y4 = f x4 in
     let y5 = f x5 in
     let y6 = f x6 in
     cmap8 f (Conses (y6, y5, y4, y3, y2, y1, acc)) xs
  | x :: xs ->
    cmap8 f (Cons (f x, acc)) xs

(* 16 word cons cell DS *)

type 'a clist16 =
  Nil
| Cons of 'a * 'a clist16
| Conses of 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a * 'a *'a * 'a clist16

(* Unrolled map, with unrolled data structure (16 word cons cells) *)
let rec crev16 (acc: 'a list): 'a clist16 -> 'a list = function
  | Nil -> acc
  | Cons (x, xs) -> crev16 (x :: acc) xs
  | Conses (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,xs) -> crev16 (x14::x13::x12::x11::x10::x9::x8::x7::x6::x5::x4::x3::x2::x1::acc) xs

let rec cmap16 (f: 'a -> 'b) (acc: 'b clist16): 'a list -> 'b list = function
  | [] -> crev16 [] acc
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
     cmap16 f (Conses (y14, y13, y12, y11, y10, y9, y8, y7, y6, y5, y4, y3, y2, y1, acc)) xs
  | x :: xs ->
    cmap16 f (Cons (f x, acc)) xs