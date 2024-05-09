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
