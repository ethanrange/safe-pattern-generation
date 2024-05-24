open Trx

(* Pattern DSL *)
type (_, _, _) pat =
    Any : ('a, 'r, 'r) pat
  | Int : int -> (int, 'r, 'r) pat
  | Var : ('a, 'a code -> 'r, 'r) pat
  | EmptyList : ('a list, 'r, 'r) pat
  | Pair : ('a, 'k, 'j) pat * ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat
  | Cons : ('a, 'k, 'j) pat * ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat

(* Helper functions *)

(* code_repr internals from Trx *)
type[@warning "-37"] 'v heap = Nil | HNode of int * stackmark * 'v * 'v heap * 'v heap
type[@warning "-37"] cr = Code of flvars * Parsetree.expression
and flvars = string Location.loc heap * vletbindings
and vletbindings = (string * code_repr) list

(* The two types can be coerced as they have identical representations - this need only be unsafe as this 
  representation is not exposed by MetaOCaml's library *)
let cr_to_code_repr (cr : cr) : code_repr = Obj.magic cr
let code_repr_to_cr (code_repr : code_repr) : cr = Obj.magic code_repr

let code_to_cr : 'a code -> cr = fun c -> let cr : 'a code :> code_repr = c in code_repr_to_cr cr

(* (Unsafe) promotion and reduction of 'a code to the underlying Parsetree.expression

These unsafe functions have been removed after implementing BER MetaOCaml's variable validation to prevent scope
extrusion, however are left commented to ensure commented previous approaches are still comprehensible.

let reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'a code :> code_repr = Obj.magic f in 
  let Code(_, pexp) = Obj.magic code_rep in pexp

let promote_code : Parsetree.expression -> 'r code = fun e ->
  let cc_e : closed_code_repr = Obj.magic e in Obj.magic (open_code cc_e)

let[@warning "-32"] closed_reduce_code : 'a code -> Parsetree.expression = fun f -> let code_rep : 'd code :> code_repr = Obj.magic f in
    let pexp : closed_code_repr :> Parsetree.expression = close_code_repr ~csp:CSP_error code_rep in pexp

*)

open Location;;

(* Variable internals from Trx - Source: https://github.com/metaocaml/ber-metaocaml *)

let empty_vars : flvars = (Nil, [])

let rec map_accum : ('acc -> 'a -> 'b * 'acc) -> 'acc -> 'a list -> 'b list * 'acc = fun f acc -> function
  | []   -> ([],acc)
  | h::t -> let (h,acc) = f acc h in
            let (t,acc) = map_accum f acc t in (h::t, acc)

let rec merge_heap : 'v heap -> 'v heap -> 'v heap = fun h1 h2 -> match (h1,h2) with
  | (Nil,h) | (h,Nil) -> h
  | (HNode (p1,k1,v1,l1,r1), HNode (p2,k2,v2,l2,r2)) -> (match p1 - p2 with
      | 0 -> 
          HNode (p1,k1,v1, merge_heap l1 l2, merge_heap r1 r2) (* same keys *)
      | n when n < 0 -> HNode (p2,k2,v2, merge_heap h1 l2, r2)
      | _ -> HNode (p1,k1,v1,l1,merge_heap h2 r1)
    )

let merge : flvars -> flvars -> flvars = fun (h1,vl1) (h2,vl2) -> (merge_heap h1 h2, vl1 @ vl2)

let scope_extrusion_error : detected:Location.t -> Parsetree.expression -> string loc -> 'a = let open Location in 
  fun ~detected ast var -> Format.kasprintf failwith 
      "Scope extrusion detected at %a for code built at %a for the identifier %s bound at %a\
      \nThe problematic code is shown below\n%a"
      print_loc detected print_loc ast.pexp_loc var.txt print_loc var.loc Pprintast.expression ast  

let validate_vars : Location.t -> cr -> cr = fun l -> function
  | Code ((Nil,_),_) as cde -> cde
  | Code ((h,_),ast) as cde -> begin
      let rec check = function
        | Nil -> ()
        | HNode (_,sm,var,h1,h2) ->
            if sm () then (check h1; check h2)
            else scope_extrusion_error ~detected:l ast var 
      in check h; cde
  end

let validate_vars_map : Location.t -> (Location.t -> 'a -> 'b * flvars) -> 'a list -> 'b list * flvars = 
  fun loc f xs -> map_accum (fun acc x -> let (y,vars) = f loc x in (y, merge vars acc)) empty_vars xs

let validate_vars_list : Location.t -> cr list -> Parsetree.expression list * flvars = fun l cs ->
  validate_vars_map l (fun l c -> let Code (vars,e) = validate_vars l c in (e,vars)) cs