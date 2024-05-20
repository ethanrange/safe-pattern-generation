open Trx;;
open Common;;

(* Common import *)

type ('a, 'f, 'r) pat = ('a, 'f, 'r) Common.pat

let __ : ('a, 'r, 'r) pat = Any
let int : int -> (int, 'r, 'r) pat = fun n -> Int n
let var : ('a, 'a code -> 'r, 'r) pat = Var

let ( ** ) : ('a, 'k, 'j) pat -> ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat = fun l r -> Pair (l, r)

let empty : ('a, 'r, 'r) pat = EmptyList
let ( >:: ) : ('a, 'k, 'j) pat -> ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat = fun x xs -> Cons (x, xs)

type ('a, 'r) case = Parsetree.case

(* Helper functions *)

(* Generation of unique variable namings in a pattern_tree *)

let lid_of_str : string -> Ast_helper.lid = fun s -> Location.mknoloc (Parse.longident (Lexing.from_string s))

let mk_ident : string -> Parsetree.expression = fun vn -> Ast_helper.Exp.ident (lid_of_str vn);;
let mk_expr_ident (s : string) : 'a code = promote_code (mk_ident s)

(* Heterogenous sequences combinators *)
let nil : 'i -> 'i = fun k -> k
let one (v : 'v) : ('v -> 'k) -> 'k = fun k -> k v

let compose (p : 'i -> 'j) (q : 'j -> 'k) : 'i -> 'k = fun k -> q (p k)

let rec build_hetseq : type a f r . int -> (a, f, r) pat -> int * Parsetree.pattern * (f -> r) = fun n -> let open Ast_helper.Pat in function
  | Any         ->                                                     (n, any (), nil)
  | Int c       ->                                                     (n, constant (Ast_helper.Const.int c), nil)
  | Var         -> let var_name = "r" ^ string_of_int n in
                   let var_pat = var (Location.mknoloc var_name) in    (n + 1, var_pat, one (mk_expr_ident var_name))
  
  | Pair(l, r) -> let (n', lpat, lf) = build_hetseq n l in
                  let (n'', rpat, rf) = build_hetseq n' r in           (n'', tuple [lpat; rpat], compose lf rf)

  | EmptyList   -> let empty_pat = construct (lid_of_str "[]") None in (n, empty_pat, nil)
  | Cons(x, xs) -> let (n', xp, lf) = build_hetseq n x in
                   let (n'', xsp, rf) = build_hetseq n' xs in
                   let p = Some([], tuple [xp; xsp]) in
                   let cons_pat = construct (lid_of_str "(::)") p in   (n'', cons_pat, compose lf rf)

let (=>) (p : ('a, 'f, 'r code) pat) (f : 'f) : ('a, 'r) case = let (_, pat, body_gen) = build_hetseq 0 p in {
    pc_lhs   = pat; 
    pc_guard = None; 
    pc_rhs   = reduce_code (body_gen f)
  }

let function_ (cases : ('a, 'b) case list) : ('a -> 'b) code = promote_code (Ast_helper.Exp.function_ cases)

let match_ (scr : 'a code) (cases : ('a, 'b) case list) : 'b code = promote_code (Ast_helper.Exp.match_ (reduce_code scr) cases)

(* Safe first-class pattern generation *)

type ('a, 'r) patwrap = Pat : ('a list, 'f, 'r code) pat * (('r code -> 'r code) -> 'f) -> ('a, 'r) patwrap
