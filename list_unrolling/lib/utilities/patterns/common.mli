type (_, _, _) pat =
    Any : ('a, 'r, 'r) pat
  | Int : int -> (int, 'r, 'r) pat
  | Var : ('a, 'a -> 'r, 'r) pat
  | EmptyList : ('a, 'r, 'r) pat
  | Pair : ('a, 'k, 'j) pat * ('b, 'j, 'r) pat -> ('a * 'b, 'k, 'r) pat
  | Cons : ('a, 'k, 'j) pat * ('a list, 'j, 'r) pat -> ('a list, 'k, 'r) pat

type 'v heap = Nil | HNode of int * Trx.stackmark * 'v * 'v heap * 'v heap
type cr = Code of flvars * Parsetree.expression
and flvars = string Location.loc heap * vletbindings
and vletbindings = (string * Trx.code_repr) list

val reduce_code : 'a Trx.code -> Parsetree.expression

val promote_code : Parsetree.expression -> 'r Trx.code

val closed_reduce_code : 'a Trx.code -> Parsetree.expression
