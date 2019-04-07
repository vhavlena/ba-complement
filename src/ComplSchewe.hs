{-|
Module      : ComplSchewe.hs
Description : Schewe rank-based complementation
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

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
  deriving (Show)


instance (Eq a) => Eq (StateSchewe a) where
  (Prefix x) == (Prefix y) = x == y
  (Suffix x) == (Suffix y) = x == y
  _ == _ = False

instance (Ord a) => Ord (StateSchewe a) where
  (Prefix x) <= (Prefix y) = x <= y
  (Suffix x) <= (Suffix y) = x <= y
  (Prefix _) <= _ = True
  _ <= _ = False

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


rankOf :: (Ord a) => RankFunc a -> Int
rankOf = maximum . Map.elems


allRanks :: (Ord a) => Set.Set a -> Int -> Set.Set a -> [a] -> Set.Set (RankFunc a)
allRanks fin n sset states = generateSRanksFromConstr fin sset con where
  con = [(q, 2*n) | q <- states]


isRankSTight :: (Ord a) => Set.Set a -> RankFunc a -> Bool
isRankSTight st f = (not $ Map.null f) && (odd mRank) && (Set.isSubsetOf odds ranks) where
  ranks = Set.fromList $ Map.elems f
  mRank = Set.findMax ranks
  odds = Set.fromList [x | x <- [1..mRank], odd x]


rankImage :: (Ord a) => Int -> RankFunc a -> Set.Set a
rankImage i = Map.keysSet . Map.filterWithKey (\_ v -> v == i)


generateSRanksFromConstr :: (Ord a) => Set.Set a -> Set.Set a -> [(a, Int)] -> Set.Set (RankFunc a)
generateSRanksFromConstr fin sset = Set.filter (isRankSTight sset) . Set.fromList . map (Map.fromList) . map (filter (\(_,y) -> y > 0)) . sequence . map (smaller)
  where
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act sym  tr = generateSRanksFromConstr fin act rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', Map.findWithDefault 0 q f) | q <- Set.toList act, q' <- succTransList q sym tr]


isFinSchewe :: StateSchewe a -> Bool
isFinSchewe (Prefix b) = Set.null b
isFinSchewe (Suffix (_,b,_,_)) = Set.null b


iniSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> Set.Set (StateSchewe a)
iniSchewe (BuchiAutomaton st ini fin _) = Set.singleton $ Prefix ini


succSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> StateSchewe a -> b
  -> Set.Set (StateSchewe a)
succSchewe (BuchiAutomaton states _ fin tr) (Prefix sset) sym = Set.union succs1 succs2 where
  funcs = Set.toList $ allRanks fin (Set.size states) sset (Set.toList states)
  succs1 = Set.fromList [Suffix (succSet sset sym tr, Set.empty, f', 0) | f' <- funcs]
  succs2 = Set.singleton $ Prefix (succSet sset sym tr)
succSchewe (BuchiAutomaton _ _ fin tr) (Suffix (sset, oset, f, i)) sym = Set.fromList succs where
  funcs = filter (\x -> (rankOf x) == (rankOf f)) $ Set.toList $ generateRanking fin f sset sym tr
  nsset = succSet sset sym tr
  succs = if not $ Set.null oset then [Suffix (nsset, Set.intersection (succSet oset sym tr) (rankImage i f'), f', i) | f' <- funcs]
          else [Suffix (nsset, (rankImage i f'), f', (i+2) `mod` ((rankOf f) + 1)) | f' <- funcs]


complSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> [b] -> BuchiAutomaton (StateSchewe a) b
complSchewe orig alp = constrFromOrig alp (succSchewe orig) (iniSchewe orig) (isFinSchewe)
