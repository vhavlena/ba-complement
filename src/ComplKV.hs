
module ComplKV (
  complKV
  , printStateKV
  , printRabitStateKV
) where


import BuchiAutomaton
import BuchiAutomataOper
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map

type RankFunc a = Map.Map a Int
type StateKV a = (Set.Set a, Set.Set a, RankFunc a)


printRabitStateKV :: StateKV String -> String
printRabitStateKV (s, o, f) = "({" ++ (Aux.printSetComF (id) s) ++ "},{"
  ++ (Aux.printSetComF (id) o) ++ "},{" ++ (Aux.printMapF (id) f) ++ "})"


printStateKV :: (Show a) => StateKV a -> String
printStateKV (s, o, f) = "({" ++ (Aux.printSetCom s) ++ "},{"
  ++ (Aux.printSetCom o) ++ "},{" ++ (Aux.printMap f) ++ "})"


oddRanks :: (Ord a) => RankFunc a -> Set.Set a
oddRanks = Set.fromList . Map.keys . Map.filter (odd)


allRanks :: (Ord a) => Set.Set a -> [a] -> Set.Set (RankFunc a)
allRanks fin states = generateFromConstr fin con where
  n = length states
  con = [(q, 2*n) | q <- states]


generateFromConstr :: (Ord a) => Set.Set a -> [(a, Int)] -> Set.Set (RankFunc a)
generateFromConstr fin = Set.fromList . map (Map.fromList) . sequence . map (smaller)
  where
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act sym  tr = generateFromConstr fin rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', f Map.! q) | q <- Set.toList act, q' <- succTransList q sym tr]


isFinKV :: StateKV a -> Bool
isFinKV (_, b, _) = Set.null b


iniKV :: (Ord a, Ord b) => BuchiAutomaton a b -> Set.Set (StateKV a)
iniKV (BuchiAutomaton st ini fin _) =
    Set.fromList [(ini, Set.empty, f) | f <- Set.toList $ allRanks fin (Set.toList st)]


succKV :: (Ord a, Ord b) => BuchiAutomaton a b -> StateKV a -> b
  -> Set.Set (StateKV a)
succKV (BuchiAutomaton _ _ fin tr) (sset, oset, f) sym = Set.fromList succs where
  funcs = Set.toList $ generateRanking fin f sset sym tr
  succs = [(succSet sset sym tr,
    if not $ Set.null oset then Set.difference (succSet oset sym tr) (oddRanks f')
    else Set.difference (succSet sset sym tr) (oddRanks f'),
    f') | f' <- funcs]


complKV :: (Ord a, Ord b) => BuchiAutomaton a b -> [b] -> BuchiAutomaton (StateKV a) b
complKV orig alp = constrFromOrig alp (succKV orig) (iniKV orig) (isFinKV)
