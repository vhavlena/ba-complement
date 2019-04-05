{-|
Module      : ComplSimKV.hs
Description : Improved KV complementation using simulations
Author      : Vojtech Havlena, April 2019
License     : GPL-3
-}

module ComplSimKV (
  complSimKV
) where


import BuchiAutomaton
import BuchiAutomataOper
import RabitRelation
import qualified AuxFunctions as Aux
import qualified Data.Set as Set
import qualified Data.Map as Map

type RankFunc a = Map.Map a Int
type StateKV a = (Set.Set a, Set.Set a, RankFunc a)
type DelaySim a = Set.Set (a, a)


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
generateRanking fin f act sym tr = generateFromConstr fin rest where
  rest = Map.toList $ Map.fromListWith (min)
    [(q', f Map.! q) | q <- Set.toList act, q' <- succTransList q sym tr]


isFinSimKV :: StateKV a -> Bool
isFinSimKV (_, b, _) = Set.null b


iniSimKV :: (Ord a, Ord b) => BuchiAutomaton a b -> DelaySim a -> Set.Set (StateKV a)
iniSimKV (BuchiAutomaton st ini fin _) sim =
  Set.fromList $ map (saturateSimState sim) [(ini, Set.empty, f) | f <- Set.toList $ allRanks fin (Set.toList ini)]


evenCeil :: Int -> Int
evenCeil n = if odd n then n+1 else n


isStateSimValid :: (Ord a) => DelaySim a -> StateKV a -> Bool
isStateSimValid rel (sset, oset, f) = Set.foldr (&&) True $
  Set.map (\(x,y) -> (f Map.! x) <= (evenCeil (f Map.! y))) $
  Set.intersection rel (Set.cartesianProduct sset sset)


simClosure :: (Ord a) => DelaySim a -> Set.Set a -> Set.Set a
simClosure sim sset = Set.fromList $ lsim >>= \(x,y) -> if y `elem` sset then return x else [] where
  lsim = Set.toList sim


saturateRank :: (Ord a) => DelaySim a -> Set.Set a -> RankFunc a -> RankFunc a
saturateRank sim satset f = Map.union f $ Map.fromList [(s, val s) | s <- add] where
  sset = Map.keysSet f
  add = Set.toList $ Set.difference satset sset
  gr s = Set.map (snd) $ Set.filter (\(x,y) -> x == s && Set.member y sset) sim
  val s = evenCeil $ Set.findMin $ Set.map (f Map.!) (gr s)


repeatChange f v
  | (f v) == v = v
  | otherwise = repeatChange f (f v)


saturateSimState :: (Ord a) => DelaySim a -> StateKV a -> StateKV a
saturateSimState sim (sset, oset, f) = (satset, oset, satf) where
  satset = repeatChange (simClosure sim) sset
  satf = saturateRank sim satset f


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
