{-|
Module      : ComplSimSchewe.hs
Description : Schewe rank-based complementation improved via simulations
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

module ComplSimSchewe (
  complSimSchewe
) where


import Simulation
import BuchiAutomaton
import BuchiAutomataOper
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as Lst


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


-- extendRank :: (Ord a) => Set.Set a -> RankFunc a -> RankFunc a
-- extendRank sset f = Map.union f $ Map.fromList [(s,1) | s <- add] where
--   add = Set.toList $ Set.difference sset (Map.keysSet f)


rankOf :: (Ord a) => RankFunc a -> Int
rankOf = maximum . Map.elems


rankOddOf :: (Ord a) => RankFunc a -> Int
rankOddOf = maximum . filter (odd) . Map.elems


allRanks :: (Ord a) => Set.Set a -> Int -> Set.Set a -> [a] -> Set.Set (RankFunc a)
allRanks fin n sset states = generateSRanksFromConstr fin sset states con where
  con = [(q, 2*n) | q <- states]


isRankSTight :: (Ord a) => Set.Set a -> RankFunc a -> Bool
isRankSTight st f = (not $ Map.null f) && (odd mRank) && (Set.isSubsetOf odds ranks) where
  ranks = Set.fromList $ Map.elems f
  mRank = Set.findMax ranks
  odds = Set.fromList [x | x <- [1..mRank], odd x]


rankImage :: (Ord a) => Int -> RankFunc a -> Set.Set a
rankImage i = Map.keysSet . Map.filterWithKey (\_ v -> v == i)


saturateRank :: (Ord a) => DelaySim a -> Set.Set a -> Set.Set a -> RankFunc a -> RankFunc a
saturateRank sim satset sset f = if Set.null sset then f else  Map.union (Map.fromList [(s, val s) | s <- add]) f where
  add = Set.toList $ Set.difference satset sset
  gr s = Set.map (snd) $ Set.filter (\(x,y) -> x == s && Set.member y sset) sim
  val s = evenCeil $ Set.findMin $ Set.map (\x -> Map.findWithDefault 0 x f) (gr s)


isStateSimValid :: (Ord a) => DelaySim a -> StateSchewe a -> Bool
isStateSimValid _ (Prefix _) = True
isStateSimValid rel (Suffix (sset, oset, f, i)) = Set.foldr (&&) True $
  Set.map (\(x,y) -> (Map.findWithDefault 0 x f) <= (evenCeil (Map.findWithDefault 0 y f))) $
  Set.intersection rel (Set.cartesianProduct sset sset)


generateSRanksFromConstr :: (Ord a) => Set.Set a -> Set.Set a -> [a] -> [(a, Int)] -> Set.Set (RankFunc a)
generateSRanksFromConstr fin sset allst = Set.filter (isRankSTight sset) . Set.map (\x -> Map.union x baseline) . Set.fromList . map (Map.fromList) . sequence . map (smaller)
  where
    baseline = Map.fromList [(s,1) | s <- allst]
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> [a] -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act states sym  tr = generateSRanksFromConstr fin act states rest where
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
succSchewe (BuchiAutomaton st _ fin tr) (Suffix (sset, oset, f, i)) sym = Set.fromList succs where
  funcs = filter (\x -> (rankOf x) == (rankOf f)) $ Set.toList $ generateRanking fin f sset (Set.toList st) sym tr
  nsset = succSet sset sym tr
  succs = if not $ Set.null oset then [Suffix (nsset, Set.intersection (succSet oset sym tr) (rankImage i f'), f', i) | f' <- funcs]
          else [Suffix (nsset, (rankImage (indnew f') f'), f', indnew f') | f' <- funcs]
  indnew f' = (i+2) `mod` ((rankOf f') + 1)


complSimSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> [b] -> BuchiAutomaton (StateSchewe a) b
complSimSchewe orig alp = constrFromOrig alp (succSchewe orig) (iniSchewe orig) (isFinSchewe)
