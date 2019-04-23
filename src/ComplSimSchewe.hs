{-|
Module      : ComplSimSchewe.hs
Description : Schewe rank-based complementation improved via simulations
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

module ComplSimSchewe (
  complSimSchewe
  , SimAlg(..)
  , RemOption(..)
) where


import Simulation
import BuchiAutomaton
import BuchiAutomataOper
import ComplSchewe
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as Lst


data RemOption =
  OptDelayed
  | OptDirect
  | OptComb
  deriving (Eq)


data SimAlg =
  Saturation
  | Removing
  | Combination
  | None
  deriving (Eq)


rankOf :: (Ord a) => RankFunc a -> Int
rankOf = maximum . Map.elems


rankOddOf :: (Ord a) => RankFunc a -> Int
rankOddOf f = if null $ filter (odd) $ Map.elems f then error $ show $ Map.elems f else maximum $ filter (odd) $ Map.elems f


allRanks :: (Ord a) => Set.Set a -> Int -> Set.Set a -> [a] -> Set.Set (RankFunc a)
allRanks fin n sset states = generateSRanksFromConstr fin sset states con where
  con = [(q, 2*n) | q <- Set.toList $ sset]


isRankSTight :: (Ord a) => Set.Set a -> RankFunc a -> Bool
isRankSTight st f = (not $ Map.null f) && (odd mRank) && (Set.isSubsetOf odds ranks) where
  ranks = Set.fromList $ Map.elems f
  mRank = Set.findMax ranks
  odds = Set.fromList [x | x <- [1..mRank], odd x]


-- isRankEvenSTight :: (Ord a) => Set.Set a -> Simulation a -> RankFunc a -> Bool
-- isRankEvenSTight st sim f = (oneStep) && (not $ null (filter (odd) $ Map.elems f) ) && (Set.isSubsetOf odds ranks) where
--   ranks = Set.fromList $ Map.elems f
--   mRank = Set.findMax ranks
--   odds = Set.fromList [x | x <- [1..mRank], odd x]
--   mRankSts = Map.keysSet $ Map.filter (mRank==) f
--   oneStep = if even mRank then (Set.foldr (\(x,y) b -> b && ((f Map.! y) == mRank - 1) True $ Set.filter (\(x,y) -> (x /= y) && (Set.member x mRankSts) && (Set.member y st)) sim)
--                           else True


rankImage :: (Ord a) => Int -> RankFunc a -> Set.Set a
rankImage i = Map.keysSet . Map.filterWithKey (\_ v -> v == i)


-- saturateRank :: (Ord a) => Simulation a -> Set.Set a -> Set.Set a -> Set.Set a -> RankFunc a -> RankFunc a
-- saturateRank sim fin satset sset f = if Set.null sset then f else Map.union (Map.fromList [(s, val s) | s <- add]) f where
--   add = Set.toList $ Set.difference satset sset
--   gr s = Set.map (snd) $ Set.filter (\(x,y) -> x == s && Set.member y sset) sim
--   val s = evenFloorFin s fin $ Set.findMin $ Set.map (f Map.!) (gr s)


isStateSimValid :: (Ord a) => Simulation a -> StateSchewe a -> Bool
isStateSimValid _ (Prefix _) = True
isStateSimValid (Delayed rel) (Suffix (sset, oset, f, i)) = (isRankSTight sset f) && (Set.foldr (&&) True $
  Set.map (\(x,y) -> (f Map.! x) <= (evenCeil (f Map.! y))) $
  Set.intersection rel (Set.cartesianProduct sset sset))
isStateSimValid (Direct rel) (Suffix (sset, oset, f, i)) = (isRankSTight sset f) && (Set.foldr (&&) True $
  Set.map (\(x,y) -> (f Map.! x) <= (f Map.! y)) $
  Set.intersection rel (Set.cartesianProduct sset sset))


-- isStateRankValid :: (Ord a) => Simulation a -> StateSchewe a -> Bool
-- isStateRankValid _ (Prefix _) = True
-- isStateRankValid rel (Suffix (sset, oset, f, i)) = isRankSTight sset f


-- saturateSimState :: (Ord a) => Simulation a -> Set.Set a -> StateSchewe a -> StateSchewe a
-- saturateSimState sim _ (Prefix sset) = Prefix $ repeatUChange (simClosure sim) sset
-- saturateSimState sim fin (Suffix (sset, oset, f, i)) = Suffix (satset, oset, satf, i) where
--   satset = repeatUChange (simClosure sim) sset
--   satf = saturateRank sim fin satset sset f


saturateStates :: (Ord a) => Simulation a -> Set.Set a -> Set.Set a
saturateStates sim sset = repeatUChange (simClosure sim) sset


generateSRanksFromConstr :: (Ord a) => Set.Set a -> Set.Set a -> [a] -> [(a, Int)] -> Set.Set (RankFunc a)
generateSRanksFromConstr fin sset allst = Set.filter (isRankSTight sset) . Set.map (\x -> Map.union x baseline) . Set.fromList . map (Map.fromList) . sequence . map (smaller)
  where
    baseline = Map.fromList [(s,1) | s <- allst]
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> Set.Set a -> [a] -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act next states sym  tr = generateSRanksFromConstr fin next states rest where
  rest = Map.toList $ Map.fromListWith (min) $
    [(q', f Map.! q) | q <- Set.toList act, q' <- succTransList q sym tr] ++
    [(q', 2*(length states)) | q' <- Set.toList $ Set.difference next (succSet act sym tr)]


isFinSchewe :: StateSchewe a -> Bool
isFinSchewe (Prefix b) = Set.null b
isFinSchewe (Suffix (_,b,_,_)) = Set.null b


iniSchewe :: (Ord a, Ord b) => BuchiAutomaton a b -> Set.Set (StateSchewe a)
iniSchewe (BuchiAutomaton st ini fin _) = Set.singleton $ Prefix ini


succSchewe :: (Ord a, Ord b) => BuchiAutomaton a b
  -> (StateSchewe a -> Bool)
  -> (Set.Set a -> Set.Set a)
  -> StateSchewe a
  -> b
  -> Set.Set (StateSchewe a)
succSchewe (BuchiAutomaton states _ fin tr) flt sat (Prefix sset) sym = Set.union succs1 succs2 where
  funcs set = Set.toList $ allRanks fin (Set.size states) set (Set.toList states)
  nsset = sat $ succSet sset sym tr
  succs1 = Set.fromList [Suffix (nsset, Set.empty, f', 0) | f' <- funcs nsset]
  succs2 = Set.singleton $ Prefix nsset
succSchewe (BuchiAutomaton st _ fin tr) flt sat (Suffix (sset, oset, f, i)) sym = Set.fromList $ filter (flt) succs where
  funcs = filter (\x -> (rankOf x) == (rankOf f)) $ Set.toList $ generateRanking fin f sset nsset (Set.toList st) sym tr
  nsset = sat $ succSet sset sym tr
  succs = if not $ Set.null oset then [Suffix (nsset, Set.intersection (succSet oset sym tr) (rankImage i f'), f', i) | f' <- funcs]
          else [Suffix (nsset, (rankImage (indnew f') f'), f', indnew f') | f' <- funcs]
  indnew f' = (i+2) `mod` ((rankOf f') + 1)


complSimSchewe :: (Ord a, Ord b) => BuchiAutomaton a b
  -> [Simulation a]
  -> RemOption
  -> [b]
  -> SimAlg
  -> BuchiAutomaton (StateSchewe a) b
complSimSchewe orig sim ind alp alg = constrFromOrig alp (succSchewe orig (flt) (sat)) (iniSchewe orig) (isFinSchewe) where
  sat = if (alg == Saturation) || (alg == Combination) then saturateStates (sim !! 0) else id
  flt = case ind of
    OptComb -> (\x -> (isStateSimValid (sim !! 0) x) && (isStateSimValid (sim !! 1) x))
    OptDelayed ->(\x -> (isStateSimValid (sim !! 0) x))
    OptDirect -> (\x -> (isStateSimValid (sim !! 1) x))
