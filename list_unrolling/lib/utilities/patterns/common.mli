type pat_tree =
  | Any
  | Var
  | EmptyList
  | Int of int
  | Pair of pat_tree * pat_tree
  | Cons of pat_tree * pat_tree

type ('a, 'b, 'c) pat = pat_tree

type 'v heap = Nil | HNode of int * Trx.stackmark * 'v * 'v heap * 'v heap
type cr = Code of flvars * Parsetree.expression
and flvars = string Location.loc heap * vletbindings
and vletbindings = (string * Trx.code_repr) list

val reduce_code : 'a Trx.code -> Parsetree.expression

val promote_code : Parsetree.expression -> 'r Trx.code

val closed_reduce_code : 'a Trx.code -> Parsetree.expression
