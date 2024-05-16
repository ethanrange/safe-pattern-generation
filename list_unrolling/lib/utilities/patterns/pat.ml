open Trx;;

(* Common import *)

type ('a, 'f, 'r) pat = ('a, 'f, 'r) Common.pat

type ('a, 'b) case = Parsetree.case

(* Helper functions *)

(* (Unsafe) promotion and reduction of 'a code to the underlying Parsetree.expression *)

let reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in 
  let Common.Code(_, pexp) = Obj.magic code_rep in pexp

let promote_code : Parsetree.expression -> 'r code = fun e ->
  let cc_e : closed_code_repr = Obj.magic e in Obj.magic (open_code cc_e)

let[@warning "-32"] closed_reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in
    let pexp : closed_code_repr :> Parsetree.expression = close_code_repr ~csp:CSP_error code_rep in pexp

(* Generation of unique variable namings in a pattern_tree *)

let lid_of_str : string -> Ast_helper.lid = fun s -> Location.mknoloc (Parse.longident (Lexing.from_string s))

let rec name_tree : int -> ('a, 'f, 'r) pat -> int * Parsetree.pattern * string list = let open Ast_helper.Pat in 
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

let mk_ident : string -> Parsetree.expression = fun vn -> Ast_helper.Exp.ident (lid_of_str vn)

let (=>) (p : ('a, 'f, 'r) pat) (f : 'f code) : ('a, 'r) case = let (_, pat, vs) = name_tree 0 p in {
    pc_lhs   = pat; 
    pc_guard = None; 
    pc_rhs   = List.fold_left Fun_rebind.apply_fun (reduce_code f) (List.map mk_ident vs)
  }

let function_ (cases : ('a, 'b) case list) : ('a -> 'b) code = promote_code (Ast_helper.Exp.function_ cases)

let match_ (scr : 'a code) (cases : ('a, 'b) case list) : 'b code = promote_code (Ast_helper.Exp.match_ (reduce_code scr) cases)

(* Safe first-class pattern generation *)

type ('a, 'r) patwrap = Pat : ('a list, 'f, 'r) pat * (('r code -> 'r code) -> 'f code) -> ('a, 'r) patwrap
