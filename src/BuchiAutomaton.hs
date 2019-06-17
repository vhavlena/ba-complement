{-|
  Module      : BuchiAutomaton.hs
  Description : Basic buchi automata definition
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

module BuchiAutomaton (
  BuchiAutomaton(..)
  , Transitions
  , Transition
  , TransitionArrow
  , succTrans
  , succTransList
  , succSet
  , alph
  , transToArrowList
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AuxFunctions as Aux


type Transitions a b = Map.Map (a, b) (Set.Set a)
type Transition a b = ((a,b), (Set.Set a))
type TransitionArrow a b = (a,b,a)

data BuchiAutomaton a b = BuchiAutomaton {
   states :: Set.Set a
   , initials :: Set.Set a
   , finals :: Set.Set a
   , transitions :: Transitions a b
}


alph :: (Ord b) => BuchiAutomaton a b -> Set.Set b
alph (BuchiAutomaton _ _ _ tr) = Set.fromList $ map (\((_,x),_) -> x) $ Map.toList tr


succTransList :: (Ord a, Ord b) => a -> b -> Transitions a b -> [a]
succTransList q sym = Set.toList . succTrans q sym


succTrans :: (Ord a, Ord b) => a -> b -> Transitions a b -> Set.Set a
succTrans q sym = Map.findWithDefault Set.empty (q,sym)


succSet :: (Ord a, Ord b) => Set.Set a -> b -> Transitions a b -> Set.Set a
succSet states sym tr = foldr (Set.union) Set.empty
  [Map.findWithDefault Set.empty (q,sym) tr | q <- Set.toList states]


transToArrowList :: Transition a b -> [TransitionArrow a b]
transToArrowList ((fr, sym), to) = map (\t -> (fr, sym, t)) $ Set.toList to
