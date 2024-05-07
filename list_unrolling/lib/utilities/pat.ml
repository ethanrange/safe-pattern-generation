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

  (* Match generator *)
  val match_ : ('a code) -> ('a, 'b) case list -> 'b code

  (* Unsafe first-class pattern generation *)

  type (_, _) unsafe_pat

  val unsafe_loosen : ('a, 'f, 'r) pat -> ('a, 'r) unsafe_pat
  val unsafe_tighten : ('a, 'r) unsafe_pat -> ('a, 'f, 'r) pat

  (* Safe first-class pattern generation *)

  type ('a, 'r) patwrap = Pat : ('a list, 'f, 'r) pat * 'f code -> ('a, 'r) patwrap

  (* PRECONDITION: 'f is some function of type 'a_1 -> ... -> 'a_n -> 'r *)
  val modify_fun_body : 'f code -> ('a -> 'r -> 'r) code -> 'a code -> 'f code

  (* Temporary exposures *)
  
  val promote_code : Parsetree.expression -> 'a code
  val reduce_code : 'a code -> Parsetree.expression

  val safe_extract_fun : ('a -> 'b) code -> 'b code
  val extract_fun : Parsetree.expression -> Parsetree.expression

  val prepend_code : 'a code -> ('a -> 't -> 'a) code -> (('t -> 'a) code)
end

module type ptreplace = sig
  open Parsetree
  val pt_replace_ident : string -> expression -> expression -> expression

  val apply_fun : expression -> expression -> expression
end

module PTReplace : ptreplace = struct
  open Parsetree
  open Longident
  open Ast_helper.Exp

  exception NotImplemented of string

  let pt_replace_ident (prev : string) (subst : expression): expression -> expression = fun e -> 
    let rec replace : expression -> expression = fun x -> match x.pexp_desc with
      | Pexp_ident {txt = Lident s; loc = _} when s = prev                                                -> subst
      | Pexp_ident _ | Pexp_constant _                                                                    -> x
      | Pexp_let (rf, vb, ex)                                                                             -> let_ 
                                      rf 
                                      (List.map (fun v -> { v with pvb_expr = replace v.pvb_expr}) vb) 
                                      (replace ex)
      | Pexp_function cl                                                                                  -> function_ 
                                      (List.map (fun c -> {
                                        c with pc_guard = Option.map replace c.pc_guard; 
                                               pc_rhs   =            replace c.pc_rhs        
                                      }) cl)      
      | Pexp_fun (al, oe, p, ex)                                                                          -> fun_
                                      al 
                                      (Option.map replace oe) 
                                      p 
                                      (replace ex) 
      | Pexp_apply (ex, ael)                                                                              -> apply 
                                      (replace ex) 
                                      (List.map (fun (al, ie) -> (al, replace ie)) ael)
      | Pexp_match (ex, cl)                                                                               -> match_ 
                                      (replace ex) 
                                      (List.map (fun c -> {
                                        c with pc_guard = Option.map replace c.pc_guard; 
                                               pc_rhs   =            replace c.pc_rhs
                                      }) cl)
      | Pexp_tuple el                                                                                     -> tuple @@
                                      List.map replace el
      | Pexp_construct (cn, eo)                                                                           -> construct
                                      cn 
                                      (Option.map replace eo)       
      | Pexp_ifthenelse (e1, e2, e3o)                                                                     -> ifthenelse
                                      (replace e1)
                                      (replace e2)
                                      (Option.map replace e3o)
      | Pexp_sequence (e1, e2)                                                                            -> sequence
                                      (replace e1)
                                      (replace e2)
      | _                                                                                                 -> raise @@
                                      NotImplemented "Parsetree renaming is not implemented for this expression type"
  in replace e

  let apply_fun (e : expression) (subst : expression) : expression = match e.pexp_desc with
    | Pexp_fun (_, _, pat, ex) -> begin 
        match pat.ppat_desc with
          | Ppat_var {txt = prev; loc = _} -> pt_replace_ident prev subst ex
          | Ppat_any                       -> ex
          | _ -> raise (NotImplemented "Only functions of the form 'fun <var> -> <exp> can be applied [Invalid pattern]") 
      end
    | _ -> raise (NotImplemented "Only functions of the form 'fun <var> -> <exp> can be applied [Not a function]") 
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
  let var : ('a, 'a -> 'r, 'r) pat = Var
  
  let ( ** ) : ('a, 'k, 'j) pat -> ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat = fun l r -> Pair (l, r)

  let empty : ('a, 'r, 'r) pat = EmptyList
  let ( >:: ) : ('a, 'k, 'j) pat -> ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat = fun x xs -> Cons (x, xs)

  type ('a, 'b) case = Parsetree.case

  (* Helper functions *)

  (* code_repr internals from Trx *)
  type[@warning "-37"] 'v heap = Nil | HNode of int * stackmark * 'v * 'v heap * 'v heap
  type[@warning "-37"] cr = Code of flvars * Parsetree.expression
  and flvars = string Location.loc heap * vletbindings
  and vletbindings = (string * code_repr) list
  
  (* (Unsafe) promotion and reduction of 'a code to the underlying Parsetree.expression *)

  let reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in 
    let Code(_, pexp) = Obj.magic code_rep in pexp

  let promote_code : Parsetree.expression -> 'r code = fun e ->
    let cc_e : closed_code_repr = Obj.magic e in Obj.magic (open_code cc_e)

  let[@warning "-32"] closed_reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in
      let pexp : closed_code_repr :> Parsetree.expression = close_code_repr ~csp:CSP_error code_rep in pexp

  (* Generation of unique variable namings in a pattern_tree *)

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

  let mk_ident : string -> Parsetree.expression = fun vn -> Ast_helper.Exp.ident (lid_of_str vn)

  let (=>) (p : ('a, 'f, 'r code) pat) (f : 'f) : ('a, 'r) case = let (_, pat, vs) = name_tree 0 p in {
      pc_lhs   = pat; 
      pc_guard = None; 
      pc_rhs   = List.fold_left PTReplace.apply_fun (reduce_code f) (List.map mk_ident vs)
    }

  let function_ (cases : ('a, 'b) case list) : ('a -> 'b) code = promote_code (Ast_helper.Exp.function_ cases)

  let match_ (scr : 'a code) (cases : ('a, 'b) case list) : 'b code = promote_code (Ast_helper.Exp.match_ (reduce_code scr) cases)

  (* Unsafe first-class pattern generation *)

  type ('a, 'r) unsafe_pat = pat_tree

  let unsafe_loosen (p : ('a, 'f, 'r) pat) : ('a, 'r) unsafe_pat = Obj.magic p
  let unsafe_tighten (p : ('a, 'r) unsafe_pat) : ('a, 'f, 'r) pat = Obj.magic p

  (* Safe first-class pattern generation *)

  type ('a, 'r) patwrap = Pat : ('a list, 'f, 'r) pat * 'f code -> ('a, 'r) patwrap

  let modify_fun_body : 'f code -> ('a -> 'r -> 'r) code -> 'a code -> 'f code = fun c pp a ->
    let rec dec_app : Parsetree.expression -> Parsetree.expression = fun x -> match x.pexp_desc with
      | Parsetree.Pexp_fun(Nolabel, None, p, e) -> Ast_helper.Exp.fun_ Nolabel None p (dec_app e)
      | _ -> PTReplace.apply_fun (PTReplace.apply_fun (reduce_code pp) (reduce_code a)) x
  in promote_code (dec_app (reduce_code c))

  (* Temporary functions *)

  let safe_extract_fun : ('a -> 'b) code -> 'b code = fun x -> match (reduce_code x).pexp_desc with
    | Parsetree.Pexp_fun(Nolabel, None, _, e) -> promote_code e
    | _ -> raise @@ Invalid_argument "Code not of form 'fun <var> -> <exp>'"

  let prepend_code : 'a code -> ('a -> 't -> 'a) code -> (('t -> 'a) code) = fun a b -> .<.~b .~a>.

  let rec extract_fun : Parsetree.expression -> Parsetree.expression = fun x -> match x.pexp_desc with
  | Parsetree.Pexp_fun(Nolabel, None, _, e) -> extract_fun e
  | _ -> x
end
