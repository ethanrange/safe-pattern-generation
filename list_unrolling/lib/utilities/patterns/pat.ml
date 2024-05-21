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

type ('a, 'r) case = Parsetree.pattern * 'r code

(* Helper functions *)

(* Generation of unique variable namings in a pattern_tree *)

let lid_of_str : string -> Ast_helper.lid = fun s -> Location.mknoloc (Parse.longident (Lexing.from_string s))

let mk_ident : string -> Parsetree.expression = fun vn -> Ast_helper.Exp.ident (lid_of_str vn);;
let mk_expr_ident (s : string) : 'a code = Obj.magic @@ cr_to_code_repr (Code (empty_vars, mk_ident s))

(* Heterogenous sequences combinators *)
let nil : 'i -> 'i = fun k -> k
let one (v : 'v) : ('v -> 'k) -> 'k = fun k -> k v

let compose (p : 'i -> 'j) (q : 'j -> 'k) : 'i -> 'k = fun k -> q (p k)

let patvar_count = ref 0

let rec build_case : type a f r . (a, f, r) pat -> Parsetree.pattern * (f -> r) = let open Ast_helper.Pat in function
  | Any         ->                                                     (any (), nil)
  | Int c       ->                                                     (constant (Ast_helper.Const.int c), nil)
  | Var         -> incr patvar_count; 
                   let var_name = "r" ^ string_of_int !patvar_count in
                   let var_pat = var (Location.mknoloc var_name) in    (var_pat, one (mk_expr_ident var_name))
  
  | Pair(l, r)  -> let (lpat, lf) = build_case l in
                   let (rpat, rf) = build_case r in                 (tuple [lpat; rpat], compose lf rf)

  | EmptyList   -> let empty_pat = construct (lid_of_str "[]") None in (empty_pat, nil)
  | Cons(x, xs) -> let (xp, lf) = build_case x in
                   let (xsp, rf) = build_case xs in
                   let p = Some([], tuple [xp; xsp]) in
                   let cons_pat = construct (lid_of_str "(::)") p in   (cons_pat, compose lf rf)

let (=>) (p : ('a, 'f, 'r code) pat) (f : 'f) : ('a, 'r) case = let (pat, body_gen) = build_case p in (pat, body_gen f)

let prod_safe_caselist (scr_cr_o : cr option) (raw_clist : (Parsetree.pattern * cr) list) : 
  (Parsetree.expression option * Parsetree.case list * flvars) = let open Parsetree in
  let (patterns, ucases) = List.split raw_clist in 
  let[@warning "-8"] (scr_e_o, exprs, vars) : (expression option * expression list * flvars)  = match scr_cr_o with
    | Some(scr_cr) -> let (scr_e :: exprs, vars) = validate_vars_list Location.none (scr_cr :: ucases) in
                      (Some(scr_e), exprs, vars)
    | None         -> let (exprs, vars) = validate_vars_list Location.none ucases in 
                      (None, exprs, vars)
  in 
  let prod_cases : (pattern * expression) -> case = fun (p, e) -> { pc_lhs = p; pc_guard = None; pc_rhs = e } in
  let caselist = List.map prod_cases (List.combine patterns exprs) in
  (scr_e_o, caselist, vars)


let[@warning "-8"] match_internal (scr_cr : cr) (cases_cr : (Parsetree.pattern * cr) list) : cr =
  let (Some(scr), caselist, vars) = prod_safe_caselist (Some(scr_cr)) cases_cr in
    Code(vars, Ast_helper.Exp.match_ ~loc:(scr.pexp_loc) scr caselist)

let match_ (scr_c : 'a code) (cases_c : ('a, 'b) case list) : 'b code =
  let scr = code_to_cr scr_c in
  let cases = List.map (fun (p, c) -> (p, code_to_cr c)) cases_c in
  Obj.magic (match_internal scr cases)

let[@warning "-8"] function_internal (cases_cr : (Parsetree.pattern * cr) list) : cr =
  let (None, caselist, vars) = prod_safe_caselist None cases_cr in
    Code(vars, Ast_helper.Exp.function_ caselist)

let function_ (cases_c : ('a, 'b) case list) : ('a -> 'b) code = 
  let cases = List.map (fun (p, c) -> (p, code_to_cr c)) cases_c in
  Obj.magic (function_internal cases)

(* Safe first-class pattern generation *)

type ('a, 'r) patwrap = Pat : ('a list, 'f, 'r code) pat * (('r code -> 'r code) -> 'f) -> ('a, 'r) patwrap
