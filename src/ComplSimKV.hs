{-|
Module      : ComplSimKV.hs
Description : Improved KV complementation using simulations
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

module ComplSimKV (
  complSimKV
) where


import Simulation
import BuchiAutomaton
import BuchiAutomataOper
import RabitRelation
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map

type RankFunc a = Map.Map a Int
type StateKV a = (Set.Set a, Set.Set a, RankFunc a)

oddRanks :: (Ord a) => RankFunc a -> Set.Set a
oddRanks = Set.fromList . Map.keys . Map.filter (odd)


allRanks :: (Ord a) => Set.Set a -> Int -> [a] -> Set.Set (RankFunc a)
allRanks fin n states = Set.singleton $ Map.fromList [(q, 2*n) | q <- states]


generateFromConstr :: (Ord a) => Set.Set a -> [(a, Int)] -> Set.Set (RankFunc a)
generateFromConstr fin = Set.fromList . map (Map.fromList) . map (filter (\(_,y) -> y > 0)) . sequence . map (smaller)
  where
    smaller (x,y)
      | Set.member x fin = [(x,y') | y' <- [0..y], even y']
      | otherwise = [(x,y') | y' <- [0..y]]


generateRanking :: (Ord a, Ord b) => Set.Set a -> RankFunc a -> Set.Set a -> b
  -> Transitions a b -> Set.Set (RankFunc a)
generateRanking fin f act sym tr = generateFromConstr fin rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', Map.findWithDefault 0 q f) | q <- Set.toList act, q' <- succTransList q sym tr]


isFinSimKV :: StateKV a -> Bool
isFinSimKV (_, b, _) = Set.null b


iniSimKV :: (Ord a, Ord b) => BuchiAutomaton a b -> DelaySim a -> Set.Set (StateKV a)
iniSimKV (BuchiAutomaton st ini fin _) sim =
  Set.fromList $ map (saturateSimState sim) [(ini, Set.empty, f) | f <- Set.toList $ allRanks fin (Set.size st) (Set.toList ini)]


isStateSimValid :: (Ord a) => DelaySim a -> StateKV a -> Bool
isStateSimValid rel (sset, oset, f) = Set.foldr (&&) True $
  Set.map (\(x,y) -> (Map.findWithDefault 0 x f) <= (evenCeil (Map.findWithDefault 0 y f))) $
  Set.intersection rel (Set.cartesianProduct sset sset)


saturateRank :: (Ord a) => DelaySim a -> Set.Set a -> Set.Set a -> RankFunc a -> RankFunc a
saturateRank sim satset sset f = if Set.null sset then f else  Map.union f $ Map.fromList [(s, val s) | s <- add] where
  add = Set.toList $ Set.difference satset sset
  gr s = Set.map (snd) $ Set.filter (\(x,y) -> x == s && Set.member y sset) sim
  val s = evenCeil $ Set.findMin $ Set.map (\x -> Map.findWithDefault 0 x f) (gr s)


saturateSimState :: (Ord a) => DelaySim a -> StateKV a -> StateKV a
saturateSimState sim (sset, oset, f) = (satset, oset, satf) where
  satset = repeatUChange (simClosure sim) sset
  satf = saturateRank sim satset sset f


succSimKV :: (Ord a, Ord b) => BuchiAutomaton a b -> DelaySim a -> StateKV a -> b
  -> Set.Set (StateKV a)
succSimKV (BuchiAutomaton _ _ fin tr) sim (sset, oset, f) sym = Set.fromList $ map (saturateSimState sim) $ filter (isStateSimValid sim) succs where
  funcs = Set.toList $ generateRanking fin f sset sym tr
  succs = [(succSet sset sym tr,
    if not $ Set.null oset then Set.difference (succSet oset sym tr) (oddRanks f')
    else Set.difference (succSet sset sym tr) (oddRanks f'),
    f') | f' <- funcs]


complSimKV :: (Ord a, Ord b) => BuchiAutomaton a b -> DelaySim a -> [b] -> BuchiAutomaton (StateKV a) b
complSimKV orig rel alp = constrFromOrig alp (succSimKV orig rel) (iniSimKV orig rel) (isFinSimKV)
