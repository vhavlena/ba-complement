
module AuxFunctions where


import Data.List
import Data.Char
import qualified Data.Set as Set
import qualified Data.Map as Map


printList :: (Show a) => String -> [a] -> String
printList delim arr = intercalate delim (map (show) arr)


printListCom :: (Show a) => [a] -> String
printListCom = printList ","


printSet :: (Show a) => String -> Set.Set a -> String
printSet delim st = printList delim (Set.toList st)


printSetCom :: (Show a) => Set.Set a -> String
printSetCom = printSet ","


mapSetFromList :: (Ord k, Ord v) => [(k,v)] -> Map.Map k (Set.Set v)
mapSetFromList = Map.fromListWith (Set.union) . map (\(x,y) -> (x, Set.singleton y))
