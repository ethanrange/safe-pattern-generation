{-# LANGUAGE TemplateHaskell #-}
import Control.Monad
import Language.Haskell.TH

-- Pre: n >= 1
sumN :: Int -> Q Dec
sumN n = funD name [cl1, cl2]
  where
    name = mkName $ "sum" ++ show n
    cl1  = do xxs@(x : xs) <- replicateM n (newName "x")
              let pattern  = listP (map varP xxs)
              let sum = foldl (\e n -> [| $e + $(varE n)|]) (varE x) xs
              clause [pattern] (normalB sum) []
    cl2  = clause [wildP] (normalB [|error "Incorrect number of inputs"|]) []

main :: IO ()
main = do
    dec <- runQ $ sumN 3
    putStrLn $ pprint dec

sum3 [x_0, x_1, x_2] = (x_0 + x_1) + x_2
sum3 _ = error "Incorrect number of inputs"
