
module ComplKV (
  complKV
) where


import BuchiAutomaton
import qualified Data.Set as Set
import qualified Data.Map as Map

type RankFunc a = Map.Map a Int
type StateKV a = (Set.Set a, Set.Set a, RankFunc a)


oddRanks :: (Ord a) => RankFunc a -> Set.Set a
oddRanks = Set.fromList . Map.keys . Map.filter (odd)


generateFromConstr :: (Ord a) => [(a, Int)] -> Set.Set (RankFunc a)
generateFromConstr = Set.fromList . map (Map.fromList) . sequence . map (smaller)
  where
    smaller (x,y) = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => RankFunc a -> Set.Set a -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking f act sym  tr = generateFromConstr rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', f Map.! q) | q <- Set.toList act, q' <- succTrans q sym tr]


isFinKV :: StateKV a -> Bool
isFinKV (_, b, _) = Set.null b


iniKV :: (Ord a, Ord b) => BuchiAutomaton a b -> Set.Set (StateKV a)
iniKV (BuchiAutomaton st ini fin tr) = Set.empty

succKV :: (Ord a, Ord b) => BuchiAutomaton a b -> StateKV a -> b
  -> Set.Set (StateKV a)
succKV (BuchiAutomaton _ _ _ tr) (sset, oset, f) sym = Set.fromList succs where
  funcs = Set.toList $ generateRanking f sset sym tr
  succs = [(succSet sset sym tr, Set.difference (succSet oset sym tr)
    (oddRanks f'), f') | f' <- funcs]


complKV :: (Ord a, Ord b) => BuchiAutomaton a b -> [b] -> BuchiAutomaton (StateKV a) b
complKV orig alp = constrFromOrig orig alp (succKV) (iniKV orig) (isFinKV)
