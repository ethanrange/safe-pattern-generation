{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Avoid lambda" #-}

import Prelude hiding (fail, succ)

-- Uncurrying combinators

zero :: p -> () -> p
zero f = \() -> f

succ :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t4, t2) -> t3
succ n f = \(x, xs) -> n (f x) xs

uncurry :: (t1 -> t2) -> t1 -> t2
uncurry n f = n f

-- Continuation numerals

nil :: (t1 -> t2) -> p -> t1 -> t2
nil = \ks kf ac -> ks ac

one :: a -> ((a, b) -> t) -> p -> b -> t
one v = \ks kf ac -> ks (v, ac)

-- Heterogeneous sequences

(#) :: (t1 -> t2 -> t3) -> (t3 -> t2 -> t4 -> t5) -> t1 -> t2 -> t4 -> t5
m # n = \ks kf ac -> n (m ks kf) kf ac

-- Failure combinators

fail :: p1 -> (() -> t) -> p2 -> t
fail = \ks kf ac -> kf ()

catch :: (t1 -> (() -> t2) -> t3 -> t4) -> (t1 -> t5 -> t3 -> t2) -> t1 -> t5 -> t3 -> t4
m `catch` n = \ks kf ac -> m ks (\() -> n ks kf ac) ac

-- Pattern combinators

var :: ((t1 -> t2 -> t3) -> (t4 -> t1) -> (t4, t2) -> t3,  a -> ((a, b) -> t) -> p -> b -> t)
var = (succ, \v -> one v)

cst :: Eq a1 => a1 -> (a2 -> a2, a1 -> (t1 -> t2) -> (() -> t2) -> t1 -> t2)
cst v' = (id, \v -> if v == v' then nil else fail)

pair :: (b -> c, a1 -> t1 -> t2 -> t3) -> (a2 -> b, t -> t3 -> t2 -> t4 -> t5) -> (a2 -> c, (a1, t) -> t1 -> t2 -> t4 -> t5)
pair p q = (currypq, \v -> matchp (fst v) # matchq (snd v))
  where
    (curryp, matchp) = p
    (curryq, matchq) = q
    currypq = curryp . curryq

(->>) :: ((p -> () -> p) -> t1 -> t2, t3 -> t2 -> t4 -> () -> t5) -> t1 -> t3 -> t4 -> t5
p ->> k = \v kf -> matchp v (curryp zero k) kf ()
    where (curryp , matchp ) = p
    
(||) :: (t1 -> (() -> t2) -> t3) -> (t1 -> t4 -> t2) -> t1 -> t4 -> t3
c1 || c2 = \v kf -> c1 v (\() -> c2 v kf )

match :: t1 -> (t1 -> (() -> a) -> t2) -> t2
match v cs = cs v (\() -> error "match")

-- Pattern demo

pattern :: ((t1 -> t2 -> t3) -> (t7 -> t8 -> t1) -> (t7, (t8, t2)) -> t3,  (a1, (Integer, t))  -> ((a1, (t, t9)) -> t5) -> (() -> t5) -> t9 -> t5)
pattern = pair var (pair (cst 2) var)

x :: Int
x = match (1, (2, 3)) $ pattern ->> (\x y -> x + y) -- 4

-- errorMsg :: Int
-- errorMsg = match ((1, 2), 3) $ pattern ->> (\x y -> x + y)
