(* Unsafe first-class pattern generation *)

type (_, _) unsafe_pat

val unsafe_loosen : ('a, 'f, 'r) Pat.pat -> ('a, 'r) unsafe_pat
val unsafe_tighten : ('a, 'r) unsafe_pat -> ('a, 'f, 'r) Pat.pat

(* Safe-wrapped unsafe first-class pattern generation

This unsafe approach is not possible without near-complete redesign after migration to the (=>) operator taking an 'f
instead of an 'f code, as the inner expression can no longer be unsafely accessed without applying values to the built
up function .

type ('a, 'r) unsafe_patwrap = UPat : ('a list, 'f, 'r code) Pat.pat * 'f code -> ('a, 'r) unsafe_patwrap

(* PRECONDITION: 'f is some function of type 'a_1 -> ... -> 'a_n -> 'r *)
val modify_fun_body : 'f code -> ('a code -> ('r -> 'r) code) -> 'a code -> 'f code

*)
