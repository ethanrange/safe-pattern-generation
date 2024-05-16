open Codelib;;

(* Unsafe first-class pattern generation *)

type (_, _) unsafe_pat

val unsafe_loosen : ('a, 'f, 'r) Pat.pat -> ('a, 'r) unsafe_pat
val unsafe_tighten : ('a, 'r) unsafe_pat -> ('a, 'f, 'r) Pat.pat

(* Safe-wrapped unsafe first-class pattern generation *)

type ('a, 'r) unsafe_patwrap = Pat : ('a list, 'f, 'r) Pat.pat * 'f code -> ('a, 'r) unsafe_patwrap

(* PRECONDITION: 'f is some function of type 'a_1 -> ... -> 'a_n -> 'r *)
val modify_fun_body : 'f code -> ('a -> 'r -> 'r) code -> 'a code -> 'f code
