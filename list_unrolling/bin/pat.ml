open Codelib;;

module type pat = sig
  type (_, _, _) pat

  (* Wildcard pattern *)
  val __ : ('a, 'r, 'r) pat
  (* Integer constant pattern *)
  val int : int -> (int, 'r, 'r) pat

  (* Variable binding pattern *)
  val var : ('a, 'a -> 'r, 'r) pat
  
  (* Binary tuple pattern *)
  val ( ** ) : ('a, 'k, 'j) pat -> ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat

  (* Empty and cons list patterns *)
  val empty : ('a, 'r, 'r) pat
  val ( >:: ) : ('a, 'k, 'j) pat -> ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat

  (* Pattern - function case pairs *)
  type (_, _) case
  val (=>) :('a, 'f, 'r) pat -> 'f code -> ('a, 'r) case

  (* Function generator *)
  val function_ : ('a, 'b) case list -> ('a -> 'b) code
end

module Examples(P : pat) =
struct
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

  let[@warning "-32"] list_example = function_ [
    empty       => .< [] >. ;
    var >:: var => .<fun x xs -> xs>.
  ]

  let[@warning "-32"] length_example : ('a list -> int) code = .<let rec len _ = .~(function_ [
    empty       => .< 0 >. ;
    var >:: var => .<fun x xs -> 1 + len () xs>.
  ]) in len ()>.
  let nmap_example = .<let rec nmap f = .~(function_ [
    empty       => .<[]>.;
    var >:: var => .<fun x xs -> let y = f x in y :: nmap f xs>.
  ]) in nmap>.;;
end

module PatImp : pat = struct
  open Trx

  (* Pattern DSL *)
  type pat_tree =
    | Any
    | Var
    | EmptyList
    | Int of int
    | Pair of pat_tree * pat_tree
    | Cons of pat_tree * pat_tree

  type ('a, 'b, 'c) pat = pat_tree


  let __ : ('a, 'r, 'r) pat = Any
  let int : int -> (int, 'r, 'r) pat = fun n -> Int n
  let var : ('a, 'a -> 'r code, 'r) pat = Var
  
  let ( ** ) : ('a, 'k, 'j) pat -> ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat = fun l r -> Pair (l, r)

  let empty : ('a, 'r, 'r) pat = EmptyList
  let ( >:: ) : ('a, 'k, 'j) pat -> ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat = fun x xs -> Cons (x, xs)

  type ('a, 'b) case = Parsetree.case

  (* Helper functions *)

  (* code_repr internals from Trx *)
  type 'v heap = Nil | HNode of int * stackmark * 'v * 'v heap * 'v heap
  type cr = Code of flvars * Parsetree.expression
  and flvars = string Location.loc heap * vletbindings
  and vletbindings = (string * code_repr) list
  
  let reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in 
    let Code(_, pexp) = Obj.magic code_rep in pexp

  let closed_reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in
      let pexp : closed_code_repr :> Parsetree.expression = close_code_repr ~csp:CSP_error code_rep in pexp

  (* let rec decomp : Parsetree.expression -> Parsetree.expression = fun x -> match x.pexp_desc with
    | Parsetree.Pexp_fun(Nolabel, None, p, e) -> decomp e
    | _ -> x *)

  let lid_of_str : string -> Ast_helper.lid = fun s -> Location.mknoloc (Parse.longident (Lexing.from_string s))

  let rec name_tree : int -> ('a, 'b, 'c) pat -> int * Parsetree.pattern * string list = let open Ast_helper.Pat in 
    fun n -> function
      | Any         ->                                          (n    , any ()                           , []        )
      | Int c       ->                                          (n    , constant (Ast_helper.Const.int c), []        )
      | Var         -> let var_name = "r" ^ string_of_int n in  (n + 1, var (Location.mknoloc var_name)  , [var_name])
      
      | Pair(l, r)  -> let (n', lpat, lvs)  = name_tree n l in          
                       let (n'', rpat, rvs) = name_tree n' r in (n''  , tuple [lpat; rpat]               , lvs @ rvs )
      
      | EmptyList   ->                                          (n    , construct (lid_of_str "[]") None , []        )
      | Cons(x, xs) -> let (m, xp, xvs) = name_tree n x in 
                       let (r, xsp, xsvs) = name_tree m xs in
                       let p = Some([], tuple [xp; xsp]) in     (r    , construct (lid_of_str "(::)") p  , xvs @ xsvs)

  let mk_ident : string -> Asttypes.arg_label * Parsetree.expression = fun vn ->
    (Asttypes.Nolabel, Ast_helper.Exp.ident (lid_of_str vn))

  let (=>) (p : ('a, 'f, 'r code) pat) (f : 'f) : ('a, 'r) case = let (_, pat, vs) = name_tree 0 p in 
    {pc_lhs = pat; pc_guard = None; pc_rhs = Ast_helper.Exp.apply (reduce_code f) (List.map mk_ident vs)}

  let function_ (cases : ('a, 'b) case list) : ('a -> 'b) code = 
    let fun_exp : Parsetree.expression = Ast_helper.Exp.function_ cases in
    let fun_ccode : closed_code_repr = Obj.magic fun_exp in 
    Obj.magic (open_code fun_ccode)
end

(* Testing *)

module PatImpExamples = Examples(PatImp)

let () = Codelib.print_code Format.std_formatter PatImpExamples.length_example;;

(* let matcher : 'a list -> 'a list = Runnative.run PatImpExamples.list_example in matcher [1; 2] *)
(* let nmap : ('a -> 'b) -> 'a list -> 'b list = Runnative.run PatImpExamples.nmap_example in List.iter print_int (nmap succ [1; 2]) *)
let len : 'a list -> int = Runnative.run PatImpExamples.length_example in print_int (len [1;2;3])
(* in List.iter print_int (matcher [1; 2]); print_endline "";; *)

(* Attempts at n argument function application generator *)

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

module Test = struct  
  type (_,_) t = Val : ('r code, 'r code) t
               | Fn : ('a, 'r) t -> (int code -> 'a, 'r) t

  let next : unit -> int code =
    let r = ref [5; 7; 9] in
    fun () -> let[@warning "-8"] (v :: vs) = !r in r := vs; .<v>.

  let rec (@@@) : type f r. (f, r) t -> f -> r = fun t f -> match t with
    | Val -> f
    | Fn g -> g @@@ (f (next ()))

  (* let none = Val @@@ .< [] >.
  let one = (Fn Val) @@@ fun x -> .< [.~x] >.
  let two = Fn (Fn Val) @@@ fun x y -> .< [.~x; .~y] >. *)
end

(* let () = let open Test in print_code Format.std_formatter (Fn (Fn Val) @@@ fun x y -> .< [.~x; .~y] >.) *)