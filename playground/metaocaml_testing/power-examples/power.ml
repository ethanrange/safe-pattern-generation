(* Introduction to MetaOCaml: the power example (A.P.Ershov, 1977) *)

let square x = x * x

(* The standard power function: x^n *)

let rec power : int -> int -> int = fun n x ->
  if n = 0 then 1
  else if n mod 2 = 0 then square (power (n/2) x)
  else x * (power (n-1) x)

(* Unit test *)
let 128 = power 7 2

(* Suppose our program has to compute x^7 many times. We may define *)
let power7 x = power 7 x


(* Specializing *)

(*{{{ Attempt 1 *)

(*
In MetaOCaml, we may also _specialize_ the power function to a
particular value |n|, obtaining the code which will later receive |x|
and compute |x^n|. We re-write |power n x| annotating expressions as
computed `now' (when |n| is known) or `later' (when |x| is given).

We start with the signature and work our way down.
Explain int code: value that will be known later (when specialized
code is run). Its type is known now (but its value is not).

Obviously as signature is changed, code must be changed.
We can't return just 1, since this has the type int, and we need
int code. So, the first feature of MetaOCaml: brackets.

Rename power -> spower
*)


let rec spower : int -> int code -> int code = fun n x ->
  if n = 0 then .<1>.
  else if n mod 2 = 0 then square (spower (n/2) x)
  else x * (spower (n-1) x)

(* What is the problem now? 
    Recall, the only way to produce a code value is to use brackets.
    So, we enclose the expression in brackets.
*)

(*}}}*)

(*{{{ Attempt 2 *)

let rec spower : int -> int code -> int code = fun n x ->
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square (spower (n/2) x)>.
  else x * (spower (n-1) x)

(* Now what? *)
(* We need splice -- the second feature of MetaOCaml *)

(*}}}*)

(*{{{ Attempt 3 *)

let rec spower : int -> int code -> int code = fun n x ->
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square .~(spower (n/2) x)>.
  else x * (spower (n-1) x)

(* It seems happy with square. What now *)

(*}}}*)

(*{{{ Almost finished *)

let rec spower : int -> int code -> int code = fun n x ->
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square .~(spower (n/2) x)>.
  else .<.~x * .~(spower (n-1) x)>.

(* But this is not the end! Specialized power should have a different
   signature: (int->int) code
   Recall: the only way to obtain code value is to use brackets.
*)

(*}}}*)

(*{{{ Specialized power generator *)

let spowern n = .<fun x -> .~(spower n .<x>.)>.

(*
insight from 2003, my first exposure to it
``<x> is a piece of code as a first-class value.
We can store it, hash it, pass to functions...''
*)

(*}}}*)

(*{{{ Generating code *)

(* Can we generate some code, to actually specialize? *)

let spower7c = spowern 7

(* We see two other features of MetaOCaml. First, the code
   can be printed. Even the code of functions can be printed. 
   Second, we see cross-stage persistent values, of CSP. 
   Are there other CSP? *)

(*}}}*)

(*{{{ Running the code *)

(* Let's accept for now that CSP just work. How to test it? *)
(* The third component: run 
   Run compiled the code and links it back to the interpreter
*)
open Runcode
let spower7 = run (spowern 7)

(* The type is as desired type. Let's see if it works
   spower7 2

*)
let 128 = spower7 2

(* QQQ What is the difference between spower7 and power7 ? *)

(*}}}*)

(* Example of run-time specialization: power_rts.ml *)


(*{{{ Generating libraries of optimized code *)

(*
Come back to CSP

One may think of CSP as references to `external libraries' -- only
in our case the program acts as a library for the code it generates.

Let's do that: make square.cmo and compile
*)

(* Load the new square *)
open Square
#load "square.cmo"

(* Re-evaluate spower *)

let rec spower : int -> int code -> int code = fun n x ->
  if n = 0 then .<1>.
  else if n mod 2 = 0 then .<square .~(spower (n/2) x)>.
  else .<.~x * .~(spower (n-1) x)>.

let spowern n = .<fun x -> .~(spower n .<x>.)>.

let spower7c = spowern 7

(* Code looks different *)

(* Save the generated code expression into a file. 
   If the first argument is (Some name), make a definition for the
   name.
*)
let write_code : string option -> string -> 'a code -> unit =
  fun pref file_name c ->
    let cde = close_code c in (* make sure the code is closed *)
    let cout = open_out file_name in
    let ppf = Format.formatter_of_out_channel cout in
    let _ = match pref with
    | Some x -> ignore (Format.fprintf ppf "let %s = " x)
    | None -> () in
    (* print code *)
    let () = format_code ppf cde in
    let () = Format.fprintf ppf "%!" in
    close_out cout

let () = write_code (Some "power7") "pw7.ml" spower7c
(* make pw7.cmo *)

(* The generated code can be saved into a file and compiled.
We can use the ordinary ocamlopt to compile the generated
code: it's regular OCaml!
We can build libraries of optimized code.

Thus, MetaOCaml can be used not only for run-time specialization,
but also for offline generation of specialized library code.
BLAS and Linpack are full of specialized functions.
MetaOCaml can be used for generating them. 
So, MetaOCaml can be used as ATLAS and, to extent, SPIRAL.
The further examples in the tutorial will stress this point.
*)

(*}}}*)

(* The distinction between MetaOCaml brackets and Lisp quotes *)

(* What function is being generated?
   What function would be generated in the Lisp version of the code?
*)
.<fun x -> .~(let body = .<x>. in .<fun x -> .~body>.) >.
;;