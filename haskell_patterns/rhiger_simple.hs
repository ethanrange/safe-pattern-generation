{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use id" #-}

import Data.Coerce (coerce)

-- Continuation numerals

nil :: p -> p
nil = \k -> k

one :: t1 -> (t1 -> t2) -> t2
one v = \k -> k v

-- Heterogeneous sequences

(#) :: (t1 -> t2) -> (t2 -> t3) -> t1 -> t3
m # n = \k -> n (m k)

-- Pattern combinators

var :: t1 -> (t1 -> t2) -> t2
var = \v -> one v

cst :: Eq a => a -> a -> p -> p
cst v' = \v -> if v == v' then nil else error "match"

pair :: (a -> i -> j) -> (t -> j -> k) -> (a, t) -> i -> k
pair p q = \v -> p (fst v) # q (snd v)

-- Pattern demo

pattern :: (a, (Int, b)) -> (a -> b -> r) -> r
pattern = pair var (pair (cst 2) var)

x :: Int
x = pattern (1, (2, 3)) (\x y -> x + y) -- 4

mistake :: (Num a, Num b1, Num (Int, b2)) => ((a, b1) -> b2 -> r) -> r
mistake = pattern ((1, 2), 3)

-- errorMsg :: Int
-- errorMsg = mistake (\x y -> x + y)

-- Metaprogramming generation

newtype Code a = Code String

makeStr :: String -> Code a
makeStr = coerce

getStr :: Code a -> String
getStr = coerce

varfn :: (Code a -> j) -> j
varfn = one (makeStr "x")

pairfn :: (i -> j) -> (j -> k) -> i -> k
pairfn p q = \k -> q (p k)

-- DSL building attempts

-- data Tree = Var | Pair Tree Tree

-- -- build :: Tree -> f
-- build Var = varfn
-- build (Pair l r) = pairfn (build l) (build r)

-- patfn :: forall {k1} {k2} {a1 :: k1} {a2 :: k2} {k3}. (Code a1 -> Code a2 -> k3) -> k3
-- patfn = pairfn varfn varfn

-- gen = patfn (\x y -> makeStr (getStr x ++ getStr y))

-- OCaml Rhiger patterns [Draft]

-- (*
--   type pat = Pcst of int | Ppair of pat * pat | Pvar of string
--   type value = Vcst of int | Vpair of value * value
  
--   exception MatchError

--   let rec match_ : pat -> value -> (string * value) list = fun p v -> match p, v with
--     | Pvar x, v -> [(x, v)]
--     | Pcst i, Vcst j -> if i = j then [] else raise MatchError
--     | Ppair(p, q), Vpair(v, w) -> (match_ p v) @ (match_ q w)
--     | _ -> raise MatchError 
-- *)

-- exception MatchError
  
-- (*    Direct style

--       let nil = fun ac -> ac
--       let one v = fun ac -> (v, ac)
--       let ( ** ) m n = fun ac -> m (n ac)
-- *)

-- (* Continuation-passing style *)
-- let nil = fun k -> k
-- let one v = fun k -> k v
-- let ( ** ) n m = fun k -> n (m k)
    
-- let var = fun v -> one v
-- let cst v' = fun v -> if v = v' then nil else raise MatchError
-- let pair p q = fun v -> (p (fst v)) ** (q (snd v))
-- let match_ p v = p v
    
-- let pat = pair var (pair (cst 2) var)
-- let m = (1, (2, 3))
  
-- let c = pat m ;; 
-- let j = c (fun x y -> x + y)

-- let q = one 5 ** nil ** one true ** one "abc" ** nil 
