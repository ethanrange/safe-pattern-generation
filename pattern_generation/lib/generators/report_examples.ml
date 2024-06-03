open Codelib;;
open Fcpatterns.Pat;;

(* Code examples used for creation of dissertation *)

[@@@warning "-32-34-37"] (* Supress all unused warnings *)

(* Pattern generation nesting *)

let nested_example : (int -> int -> int) code =
  function_ [var => fun x -> (function_ [var => fun _ -> x])]

let nested_native : int -> int -> int = function
  | x -> (function | _ -> x);;

(* MetaOCaml Obj.magic creation *)

let magic_insertion : 'a -> 'a code = fun r0 -> .<r0>.;;

(* Scope extrusion attempt *)

let se_attempt : (int -> int code) code = let r = ref .<0>. in
  function_ [
    var => fun r0 -> .<.~(r := .<.~r0 + 1>.; .<r0>.)>.
  ];;

(* MetaOCaml sum_3 generation *)

let[@warning "-8"] gen_sum_3 = .<let sum_3 l = .~(make_match .<l>. [
     .<fun [x1; x2; x3] -> x1 + x2 + x3>. [@metaocaml.functionliteral];
     .<fun _ -> raise @@ Invalid_argument "Incorrect number of input">. 
                                          [@metaocaml.functionliteral];
]) in sum_3>.

(* Let polymorphism disadvantage example

let ptn : 'a . (int * int, int code -> int code -> 'a, 'a) pat = var ** var;; 
let first : (int * int -> int) code = function_ [ptn => fun _ _ -> .<1>.]
let second : (int * int -> char) code = function_ [ptn => fun _ _ -> .<'a'>.] 

*)

(* gen_pattern build up *)

type ('a, 'r) pewrap = Pat : ('a list, 'f, 'r code) pat * 'f -> ('a, 'r) pewrap

let compose f g = fun x -> g (f x)

let gen_pattern (n : int) (base : int code) (ind : int code -> int code -> int code): (int list, int) case =
  let rec loop (n : int) : (int, int) patwrap =
    if n = 0 
    then Pat (empty, fun k -> k base)
    else let Pat (p, k) = loop (n - 1)in Pat (var >:: p, fun c x -> k (compose (ind x) c))
  in match loop n with
    | Pat (p, k) -> p => (k Fun.id)

(* sum_n generator example *)

let gen_sum_n (n : int) = function_ [
  Fcp_generators.gen_exactly_n_cons n .<0>. (fun x acc -> .<.~x + .~acc>.);
  
  __ => let error_msg = Format.sprintf 
          "Sum function only accepts a list of length %d" n in
        .<raise @@ Invalid_argument error_msg>.
]

let sum_3_generated = .<function
  | r6::r7::r8::[] -> r6 + (r7 + (r8 + 0))
  | _ -> Stdlib.raise (Stdlib.Invalid_argument 
          "Sum function only accepts a list of length 3")>.

(* Nested generation example *)

let nested_example_2 : (int -> int -> int) code = function_ [
  var => fun x -> (function_ [var => fun _ -> x])
];;

(* Length generator example *)

let gen_len : ('a list -> int) code = 
  .<let rec len l = .~(match_ .<l>. [
      empty       => .< 0 >. ;
      var >:: var => fun _ xs -> .<1 + len .~xs>.
  ]) in len>.

(* Failing function length generation [Invalid RHS] *)

(* let gen_len : ('a list -> int) code = .<let rec len = .~(function_ [
      empty       => .< 0 >. ;
      var >:: var => fun _ xs -> .<1 + len .~xs>.
  ]) in len>. *)

(* Native, MetaOCaml and first-class pattern generation implementations of sum3 *)

let sum3_plain = function
  | [x1, x2, x3] -> x1 + x2 + x3
  | _ -> failwith "Input must be length 3"

let[@warning "-8"] sum3_metaocaml = .<let sum_3 l = .~(make_match .<l>. [
  .<fun [x1; x2; x3] -> x1 + x2 + x3>. [@metaocaml.functionliteral];
  .<fun _ -> failwith "Input must be length 3">. [@metaocaml.functionliteral];
  ]) in sum_3>.

let sum3_fcp = function_ [
  var >:: (var >:: (var >:: empty)) => (fun x1 x2 x3 -> .<.~x1 + .~x2 + .~x3>.);
  __              => .<failwith "Input must be length 3">.
]

let sum3_fcp_better = function_ [
  [var; var; var] => (fun x1 x2 x3 -> .<.~x1 + .~x2 + .~x3>.);
  __              => .<failwith "Input must be length 3">.
]

(* Error message evaluation generators *)

(* let incomp_ret = function_ [
  var => (fun _ -> .<1>.);
  var => (fun _ -> .<true>.)
] *)

(* let incomp_pat = function_ [
  int 2 => .<1>.;
  int 2 ** int 2 => .<1>.
] *)

(* let incomp_scr = match_ .<true>. [
  int 2 => .<1>.;
] *)

(* let incomp_scr_mo = make_match .<true>. [
  .<fun 2 -> 1>. [@metaocaml.functionliteral]
]  *)

(* let erroneous_use = match_ .<true>. [
  var => fun x -> .<.~x + 1>.
] *)

(* let too_many_binds = function_ [
  int 2 => fun x -> x
] *)

(* let too_few_binds = function_ [
  var => .<2>.
] *)

(* let forgotten_splice = function_ [
  [var; var] => fun x y -> .<x + y>.
] *)

(* let incompatible_combinators = function_ [
  gen_exactly_n_cons 2 .<true>. (fun x y -> .<.~x && .~y>.);
  gen_exactly_n_cons 3 .<0>. (fun x y -> .<.~x + .~y>.)
] *)

(* let incomp_basecase = function_ [
  gen_n_cons 2 (fun _ -> .<true>.) (fun x y -> .<.~x + .~y>.);
] *)

(* let incomp_inductive = function_ [
  gen_n_cons 2 (fun _ -> .<true>.) (fun x y -> .<.~y .~x>.);
] *)

(* let scope_capture : (int -> int) code = .<let x0 = 1 in .~(function_ [
  var => fun _ -> .<x0>.
])>. *)

(* Pattern scope extrusion attempt V2 *)
let scope_extrusion : (int -> int code) code = let r : int code ref = ref .<0>. in function_ [
  var => fun x -> .<r := x; .<1>. >.
]

(* Standard scope extrusion example *)
let r = ref .<0>.
let _ = fun r0 -> (r := .<r0>.)
let extrusion = .<fun _ -> .~(!r)>.
let _ = Runnative.run extrusion 0

let genlet_extrusion = function_ [
  var => fun x -> let y = genlet ~name:"y" .<.~x + 1>. in .<.~y>.
]
