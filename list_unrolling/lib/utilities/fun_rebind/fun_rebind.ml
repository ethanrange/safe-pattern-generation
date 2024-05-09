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