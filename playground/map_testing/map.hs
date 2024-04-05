{-# LANGUAGE BangPatterns #-}

import System.CPUTime

main :: IO ()
main = do
    timeFn (dummyMap []) >>= print
    timeFn (nmap []) >>= print
    timeFn (umap []) >>= print
    timeFn (cmap Nil) >>= print
    timeFn (ntumap []) >>= print
    
timeFn f = do
    start <- getCPUTime 
    let !x1 = f (+1) [1..10000000]
    end <- getCPUTime
    return $ fromIntegral (end - start) / 10e9

dummyMap acc f [] = []
dummyMap acc f (x : xs) = f x : dummyMap [] f xs

rev acc [] = acc
rev acc (x : xs) = rev (x : acc) xs

nmap :: [b] -> (a -> b)-> [a] -> [b]
nmap acc f [] = rev [] acc
nmap acc f (x : xs) = nmap (f x : acc) f xs

urev acc [] = acc
urev acc (x1 : x2 : x3 : x4 : x5 : x6 : xs) = urev (x6 : x5 : x4 : x3 : x2 : x1 : acc) xs
urev acc (x : xs) = urev (x : acc) xs

umap :: [b] -> (a -> b) -> [a] -> [b]
umap acc f [] = urev [] acc
umap acc f (x1 : x2 : x3 : x4 : x5 : x6 : xs) = umap (f x6 : f x5 : f x4 : f x3 : f x2 : f x1 : acc) f xs
umap acc f (x : xs) = umap (f x : acc) f xs

ntumap :: [b] -> (a -> b) -> [a] -> [b]
ntumap _ f [] = []
ntumap _ f (x1 : x2 : x3 : x4 : x5 : x6 : xs) = f x6 : f x5 : f x4 : f x3 : f x2 : f x1 : f x1 : ntumap [] f xs
ntumap _ f (x : xs) = f x : ntumap [] f xs

data CList a = Nil | Cons !a !(CList a) | Conses !a !a !a !a !a !a !(CList a)

crev acc Nil = acc
crev acc (Cons x xs) = crev (x : acc) xs
crev acc (Conses x1 x2 x3 x4 x5 x6 xs) = crev (x6 : x5 : x4 : x3 : x2 : x1 : acc) xs

cmap :: CList b -> (a -> b) -> [a] -> [b]
cmap acc f [] = crev [] acc
cmap acc f (x1 : x2 : x3 : x4 : x5 : x6 : xs) = cmap (Conses (f x6) (f x5) (f x4) (f x3) (f x2) (f x1) acc) f xs
cmap acc f (x : xs) = cmap (Cons (f x) acc) f xs
