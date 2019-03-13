
module BuchiAutomaton (
  BuchiAutomaton(..)
  , Transitions
  , Transition
  , succTrans
  , succSet
  , unionBA
  , constrFromOrig
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


unionBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b
  -> BuchiAutomaton a b
unionBA (BuchiAutomaton s1 i1 f1 tr1) (BuchiAutomaton s2 i2 f2 tr2) =
    BuchiAutomaton (Set.union s1 s2) (Set.union i1 i2)
      (Set.union f1 f2) (Map.union tr1 tr2)


constrFromOrigProc :: (Ord a, Ord b, Ord c) => [c]
  -> BuchiAutomaton a b
  -> BuchiAutomaton c b
  -> [b]
  -> (BuchiAutomaton a b -> c -> b -> Set.Set c)
  -> (c -> Bool)
  -> BuchiAutomaton c b
constrFromOrigProc [] _ ba _ _ _ = ba
constrFromOrigProc (x:xs) orig b1@(BuchiAutomaton st1 _ _ _) alp suc isFin =
  constrFromOrigProc x' orig (unionBA b1 b2) alp suc isFin where
    tr' = Map.fromList [((x,sym), suc orig x sym) | sym <- alp]
    st' = foldr (Set.union) Set.empty $ Map.elems tr'
    ini' = Set.empty
    fin' = Set.filter (isFin) st'
    b2 = BuchiAutomaton st' ini' fin' tr'
    x' = xs ++ (Set.toList $ Set.difference st' st1)


constrFromOrig :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b
  -> [b]
  -> (BuchiAutomaton a b -> c -> b -> Set.Set c)
  -> (Set.Set c)
  -> (c -> Bool)
  -> BuchiAutomaton c b
constrFromOrig orig alp suc ini isFin = constrFromOrigProc (Set.toList ini)
  orig (BuchiAutomaton ini ini (Set.filter (isFin) ini) Map.empty) alp suc isFin
