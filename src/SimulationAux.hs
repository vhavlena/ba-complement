
module SimulationAux where

import BuchiAutomaton
import qualified Data.Map as Map
import qualified Data.Set.Monad as Set


symDifference x y = Set.union (Set.difference x y) (Set.difference y x)

mapFromSet :: (Ord k, Ord v)  => Set.Set (k,v) -> Map.Map k v
mapFromSet = Map.fromList . Set.toList


preMap ba alph = mapFromSet $ Set.map (fnc ba) alph where
  fnc ba sym = (sym, preSym ba sym)


preSym :: (Ord a, Ord b) => BuchiAutomaton a b -> b -> Map.Map a (Set.Set a)
preSym (BuchiAutomaton _ _ _ tr) sym = Map.fromListWith (Set.union) $ map (\(x,_,y) -> (y, Set.singleton x)) $ filter (\(_,x,_) -> x == sym) $ transList tr


postMap ba alph = mapFromSet $ Set.map (fnc ba) alph where
  fnc ba sym = (sym, postSym ba sym)

postSym (BuchiAutomaton _ _ _ tr) sym = Map.fromListWith (Set.union) $ map (\(x,_,y) -> (x, Set.singleton y)) $ filter (\(_,x,_) -> x == sym) $ transList tr
