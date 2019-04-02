
module ComplSchewe (
  complSchewe
) where


import BuchiAutomaton
import BuchiAutomataOper
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map

type RankFunc a = Map.Map a Int
type SufSchewe a = (Set.Set a, Set.Set a, RankFunc a, Int)
data StateSchewe a =
  Prefix (Set.Set a)
  | Suffix (SufSchewe a)
  deriving (Show, Ord)


-- printRabitStateKV :: StateKV String -> String
-- printRabitStateKV (s, o, f) = "({" ++ (Aux.printSetComF (id) s) ++ "},{"
--   ++ (Aux.printSetComF (id) o) ++ "},{" ++ (Aux.printMapF (id) f) ++ "})"
--
--
-- printStateKV :: (Show a) => StateKV a -> String
-- printStateKV (s, o, f) = "({" ++ (Aux.printSetCom s) ++ "},{"
--   ++ (Aux.printSetCom o) ++ "},{" ++ (Aux.printMap f) ++ "})"


oddRanks :: (Ord a) => RankFunc a -> Set.Set a
oddRanks = Set.fromList . Map.keys . Map.filter (odd)


allRanks :: (Ord a) => Set.Set a -> Set.Set a -> [a] -> Set.Set (RankFunc a)
allRanks fin sset states = generateFromConstr fin sset con where
  n = length states
  con = [(q, 2*n) | q <- states]


isRankSTight :: (Ord a) => Set.Set a -> RankFunc a -> Bool
isRankSTight st f = (odd mRank) && (Set.isSubsetOf odds ranks) where
  ranks = Set.fromList $ Map.elems f
  mRank = Set.findMax ranks
  odds = Set.fromList [x | x <- [1..mRank], odd x]



generateFromConstr :: (Ord a) => Set.Set a -> Set.Set a -> [(a, Int)] -> Set.Set (RankFunc a)
generateFromConstr fin sset = Set.filter (isRankSTight sset) . Set.fromList . map (Map.fromList) . sequence . map (smaller)
  where
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act sym  tr = generateFromConstr fin act rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', f Map.! q) | q <- Set.toList act, q' <- succTransList q sym tr]


isFinKV :: StateSchewe a -> Bool
isFinKV (Prefix b) = Set.null b
isFinKV (Suffix (_,b,_,_)) = Set.null b


iniKV :: (Ord a, Ord b) => BuchiAutomaton a b -> Set.Set (StateSchewe a)
iniKV (BuchiAutomaton st ini fin _) = Set.singleton $ Prefix ini


succSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> StateSchewe a -> b
  -> Set.Set (StateSchewe a)
succSchewe (BuchiAutomaton _ _ fin tr) (Prefix sset) sym = Set.union succs1 succs2 where
  funcs = Set.toList $ generateRanking fin f sset sym tr
  succs1 = Set.fromList [Suffix (succSet sset sym tr, Set.empty, f', 0) | f' <- funcs]
  succs2 = Set.singleton $ Prefix (succSet sset sym tr)
succSchewe (BuchiAutomaton _ _ fin tr) (sset, oset, f) sym = Set.fromList succs where
  funcs = Set.toList $ generateRanking fin f sset sym tr
  succs = [(succSet sset sym tr,
    if not $ Set.null oset then Set.difference (succSet oset sym tr) (oddRanks f')
    else Set.difference (succSet sset sym tr) (oddRanks f'),
    f') | f' <- funcs]


complKV :: (Ord a, Ord b) => BuchiAutomaton a b -> [b] -> BuchiAutomaton (StateKV a) b
complKV orig alp = constrFromOrig alp (succSchewe orig) (iniKV orig) (isFinKV)
