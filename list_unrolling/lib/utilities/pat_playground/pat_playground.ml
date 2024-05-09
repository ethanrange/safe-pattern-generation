open Codelib;;

module Examples(P : module type of Fcpatterns.Pat) = struct
  open P

  let[@warning "-32"] f : (int * int -> int) code = function_ [
      (var   ** int 4) => .< fun x -> x + 1 >.; 
      (int 3 ** __   ) => .< 1 >.             ;
      (var   ** var  ) => .< fun x -> fun y -> x + y >.
    ]
  let[@warning "-32"] pairs_example : (int  * int -> int) code = .<let f = .~(function_ [
    __    ** int 2   => .< 1 >. ;
    (int 3 ** __)    => .< 1 >. ;
    (var   ** var)   => .< fun a b -> a + b >.;
    ]) in f>.

  let[@warning "-32"] list_example : ('a list -> 'a list) code = .<let f = .~(function_ [
    empty       => .< [] >. ;
    var >:: var => .<fun _ xs -> xs>.
  ]) in f>.

  (* Due to OCaml's restrictions on the RHS of let recs, a let rec RHS cannot be a direct quote of code. In cases
     where arguments are taken in, such as the nmap_example below, this is not a problem, as the RHS is of the form
    
     fun x -> .< code >.
     
     However this is a problem in the length example. It can be overcome by either making the RHS of the form
     
     fun () -> .< code >. and passing a unit to every recrusive call, or
     lazy .< code >. and using Lazy.force before every recursive call.contents
     
     Alternatively, the match_ generator may be used to avoid this issue entirely, by explicitly stating the scrutinee
     
     See https://ocaml.org/manual/5.1/letrecvalues.html for discussion of these RHS restrictions *)
  let[@warning "-32"] length_example : ('a list -> int) code = .<let rec len _ = .~(function_ [
    empty       => .< 0 >. ;
    var >:: var => .<fun _ xs -> 1 + len () xs>.
  ]) in len ()>.

  let[@warning "-32"] match_length_example : ('a list -> int) code = .<let rec len l = .~(match_ .<l>. [
    empty       => .< 0 >. ;
    var >:: var => .<fun _ xs -> 1 + len xs>.
  ]) in len>.
  let[@warning "-32"] nmap_example = .<let rec nmap f = .~(function_ [
    empty       => .<[]>.;
    var >:: var => .<fun x xs -> let y = f x in y :: nmap f xs>.
  ]) in nmap>.;;
end

(* Testing *)

module PatImpExamples = Examples(Fcpatterns.Pat)

let () = print_code Format.std_formatter PatImpExamples.match_length_example;;

(* let matcher : 'a list -> 'a list = Runnative.run PatImpExamples.list_example in matcher [1; 2] *)
(* let nmap : ('a -> 'b) -> 'a list -> 'b list = Runnative.run PatImpExamples.nmap_example in List.iter print_int (nmap succ [1; 2]) *)
let len : 'a list -> int = Runnative.run PatImpExamples.match_length_example in print_int (len ["a"; "b"; "c"])
(* in List.iter print_int (matcher [1; 2]); print_endline "";; *)

(* 
  ////////
  Attempts at n argument function application generator 
  ////////
*)

(* 
let gen_n_apply (n : int) = 
  let rec loop (n: int) (f : 'f code) : 'b code = 
    if n = 0 
    then .<function
      | [x] -> .<.~f x>.
      | []   -> raise (Invalid_argument "[2] Too few args")
      | _   -> raise (Invalid_argument "[3] Too many args")>. 
    else .<function
            | x :: xs -> .<(.~(loop (n - 1) f) xs) x>.
            | _ -> raise (Invalid_argument "[1] Too few args")
         >.  
  in .<let apply f xs = .~(loop (n - 1) .< f >.) xs
  in apply>.;; *)

(* let () = Codelib.print_code Format.std_formatter (gen_n_apply 2);; *)
(* let f (x : int) = x ;; *)
(* let app2 = Runnative.run (gen_n_apply 3) in print_int (app2 (fun a b c -> a + b + c) [5; 6; 7; 8;]); print_endline "";; *)

(* 
  ////////
  Function application experiments 
  ////////
*)

(* module FunAppPlayground = struct  
  type (_,_) t = Val : ('r code, 'r code) t
               | Fn : ('a, 'r) t -> (int code -> 'a, 'r) t

  let next : unit -> int code =
    let r = ref [5; 7; 9] in
    fun () -> let[@warning "-8"] (v :: vs) = !r in r := vs; .<v>.

  let rec (@@@) : type f r. (f, r) t -> f -> r = fun t f -> match t with
    | Val -> f
    | Fn g -> g @@@ (f (next ()))

  let none = Val @@@ .< [] >.
  let one = (Fn Val) @@@ fun x -> .< [.~x] >.
  let two = Fn (Fn Val) @@@ fun x y -> .< [.~x; .~y] >.
end *)

(* let () = let open FunAppPlayground in print_code Format.std_formatter (Fn (Fn Val) @@@ fun x y -> .< [.~x; .~y] >.) *)