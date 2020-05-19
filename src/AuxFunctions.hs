{-|
  Module      : AuxFunctions.hs
  Description : Auxiliary functions
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

module AuxFunctions where


import Data.List
import Data.Char
import qualified Data.Set.Monad as Set
import qualified Data.Map as Map


printList :: (Show a) => String -> [a] -> String
printList delim arr = intercalate delim (map (show) arr)


printListF :: (a -> String) -> String -> [a] -> String
printListF f delim arr = intercalate delim (map (f) arr)


printListCom :: (Show a) => [a] -> String
printListCom = printList ","


printSet :: (Ord a) => (Show a) => String -> Set.Set a -> String
printSet delim st = printList delim (Set.toList st)


printSetF :: (Ord a) => (a -> String) -> String -> Set.Set a -> String
printSetF f delim st = printListF f delim (Set.toList st)


printSetCom :: (Show a, Ord a) => Set.Set a -> String
printSetCom = printSet ","

printSetComF :: (Show a, Ord a) => (a -> String) -> Set.Set a -> String
printSetComF f = printSetF f ","


printMap :: (Show k, Show a) => Map.Map k a -> String
printMap mp = printListF (\(x,y) -> (show x)++"->"++(show y)) "," $ Map.toList mp


printMapF :: (Show k, Show a) => (k -> String) -> Map.Map k a -> String
printMapF f mp = printListF (\(x,y) -> (f x)++"->"++(show y)) "," $ Map.toList mp


mapSetFromList :: (Ord k, Ord v) => [(k,v)] -> Map.Map k (Set.Set v)
mapSetFromList = Map.fromListWith (Set.union) . map (\(x,y) -> (x, Set.singleton y))


cartProd s1 s2 = do
  x1 <- s1
  x2 <- s2
  return (x1, x2)


indexedSet :: (Ord a) => Set.Set a -> (Set.Set (Int, a))
indexedSet st = cartProd ind st where
  s = (Set.size st) - 1
  ind = Set.fromList [0..s]
