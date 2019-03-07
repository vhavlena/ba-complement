
module BuchiAutomataOper (
  transposeBA
  , restrictBA
  , trimBA
) where


import qualified Data.Map as Map
import qualified Data.Bimap as Bimp
import qualified Data.Set as Set
import Data.Graph
import Data.Tree
import BuchiAutomaton

type StateMap a = Bimp.Bimap Int a


createStateMap :: (Ord a) => BuchiAutomaton a b -> StateMap a
createStateMap (BuchiAutomaton st _ _ _) = Bimp.fromList $ zip [0..(length st)-1] (Set.toList st)


convBAtoGraph :: (Ord a) => StateMap a -> BuchiAutomaton a b -> Graph
convBAtoGraph mp (BuchiAutomaton st _ _ tr) = buildG bounds $ rename $ trToEdges tr  where
  vert = Bimp.keys mp
  bounds = (minimum vert, maximum vert)
  trToEdges = Set.toList . foldr (Set.union) Set.empty .
    map (\((x,y),s) -> Set.cartesianProduct (Set.singleton x) s) . Map.toList
  rename = map (\(x,y) -> (mp Bimp.!> x, mp Bimp.!> y))


finalSCC :: (Ord a) => BuchiAutomaton a b -> [Set.Set a]
finalSCC b@(BuchiAutomaton _ _ fin _) = filter (\x -> not $ Set.disjoint x fin) stscc where
  stmap = createStateMap b
  graph = convBAtoGraph stmap b
  sccs = scc graph
  stscc = map (Set.fromList . map (stmap Bimp.!) . flatten) sccs



reachableStates :: (Ord a) => BuchiAutomaton a b -> Set.Set a -> Set.Set a
reachableStates b start = convF $ reach $ Set.toList $ convB start where
  stmap = createStateMap b
  convF = Set.map (stmap Bimp.!)
  convB = Set.map (stmap Bimp.!>)
  graph = convBAtoGraph stmap b
  reach = foldr (Set.union) Set.empty . map (Set.fromList . flatten) . dfs graph


transposeBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b
transposeBA (BuchiAutomaton st ini fin tr) = BuchiAutomaton st ini fin tr' where
  trList = map (\((x,a),s) -> (Set.map (\(c,d) -> ((c,a),d)) $ revProd x s)) $ Map.toList tr
  tr' = Map.fromListWith (Set.union) $ Set.toList $ foldr (Set.union) Set.empty trList
  revProd x s = Set.cartesianProduct s (Set.singleton $ Set.singleton x)


restrictBA :: (Ord a) => BuchiAutomaton a b -> Set.Set a -> BuchiAutomaton a b
restrictBA (BuchiAutomaton st ini fin tr) s = BuchiAutomaton st' ini' fin' (projTr tr) where
  st' = Set.intersection st s
  ini' = Set.intersection ini s
  fin' = Set.intersection fin s
  projTr = Map.filterWithKey (\(x,a) y -> (Set.member x s) && (not $ Set.null y))
    . Map.map (Set.intersection s)


trimBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b
trimBA b@(BuchiAutomaton st ini fin tr) = restrictBA b rst where
  forwReach = reachableStates b ini
  backReach = reachableStates (transposeBA b) finSCC
  finSCC = foldr (Set.union) Set.empty $ finalSCC b
  rst = Set.intersection forwReach backReach
