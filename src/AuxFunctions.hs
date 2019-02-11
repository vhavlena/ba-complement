
module AuxFunctions where


import Data.List
import Data.Char
import qualified Data.Set as Set


printList :: (Show a) => String -> [a] -> String
printList delim arr = intercalate delim (map (show) arr)


printListCom :: (Show a) => [a] -> String
printListCom = printList ","


printSet :: (Show a) => String -> Set.Set a -> String
printSet delim st = printList delim (Set.toList st)


printSetCom :: (Show a) => Set.Set a -> String
printSetCom = printSet ","
