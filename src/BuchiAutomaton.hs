
module BuchiAutomaton (
  BuchiAutomaton(..)
  , Transitions
  , Transition
  , succTrans
  , succSet
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AuxFunctions as Aux


type Transitions a b = Map.Map (a, b) (Set.Set a)
type Transition a b = ((a,b), (Set.Set a))

data BuchiAutomaton a b = BuchiAutomaton {
   states :: Set.Set a
   , initials :: Set.Set a
   , finals :: Set.Set a
   , transitions :: Transitions a b
}


succTrans :: (Ord a, Ord b) => a -> b -> Transitions a b -> [a]
succTrans q sym = Set.toList . Map.findWithDefault Set.empty (q,sym)


succSet :: (Ord a, Ord b) => Set.Set a -> b -> Transitions a b -> Set.Set a
succSet states sym tr = foldr (Set.union) Set.empty
  [Map.findWithDefault Set.empty (q,sym) tr | q <- Set.toList states]
