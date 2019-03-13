
module BuchiAutomatonConstr (
  constrFromOrig
) where


import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AuxFunctions as Aux
import BuchiAutomaton
import BuchiAutomataOper


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
