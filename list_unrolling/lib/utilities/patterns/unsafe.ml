type ('a, 'f, 'r) pat = ('a, 'f, 'r) Pat.pat

(* Unsafe first-class pattern generation *)

type ('a, 'r) unsafe_pat = ('a, unit, 'r) Pat.pat

let unsafe_loosen (p : ('a, 'f, 'r) pat) : ('a, 'r) unsafe_pat = Obj.magic p
let unsafe_tighten (p : ('a, 'r) unsafe_pat) : ('a, 'f, 'r) pat = Obj.magic p

(* Safe-wrapped unsafe first-class pattern generation

This unsafe approach is not possible without near-complete redesign after migration to the (=>) operator taking an 'f
instead of an 'f code, as the inner expression can no longer be unsafely accessed without applying values to the built
up function .

type ('a, 'r) unsafe_patwrap = UPat : ('a list, 'f, 'r) pat * 'f code -> ('a, 'r) unsafe_patwrap

let modify_fun_body : 'f code -> ('a -> 'r -> 'r) code -> 'a code -> 'f code = fun c pp a ->
  let rec dec_app : Parsetree.expression -> Parsetree.expression = fun x -> match x.pexp_desc with
    | Parsetree.Pexp_fun(Nolabel, None, p, e) -> Ast_helper.Exp.fun_ Nolabel None p (dec_app e)
    | _ -> apply_fun (apply_fun (reduce_code pp) (reduce_code a)) x
in promote_code (dec_app (reduce_code c)) 

*)
