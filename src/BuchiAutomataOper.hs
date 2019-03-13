
module BuchiAutomataOper (
  transposeBA
  , restrictBA
  , trimBA
  , renameBA
  , unionBA
  , disjointUnionBA
) where


import qualified Data.Map as Map
import qualified Data.Bimap as Bimp
import qualified Data.Set as Set
import Data.Graph
import Data.Tree
import BuchiAutomaton

type StateMap a = Bimp.Bimap Int a

--------------------------------------------------------------------------------------------------------------
-- Part with the trim, transpose, and restriction function
--------------------------------------------------------------------------------------------------------------

createStateMap :: (Ord a) => BuchiAutomaton a b -> StateMap a
createStateMap (BuchiAutomaton st _ _ _) = Bimp.fromList $ zip [0..(length st)-1] (Set.toList st)


createStateMapFrom :: (Ord a) => Int -> BuchiAutomaton a b -> StateMap a
createStateMapFrom x (BuchiAutomaton st _ _ _) = Bimp.fromList $ zip [x..(length st)-1+x] (Set.toList st)


convBAtoGraph :: (Ord a) => StateMap a -> BuchiAutomaton a b -> Graph
convBAtoGraph mp (BuchiAutomaton st _ _ tr) = buildG bounds $ rename $ trToEdges tr  where
  vert = Bimp.keys mp
  bounds = (minimum vert, maximum vert)
  trToEdges = Set.toList . foldr (Set.union) Set.empty .
    map (\((x,y),s) -> Set.cartesianProduct (Set.singleton x) s) . Map.toList
  rename = map (\(x,y) -> (mp Bimp.!> x, mp Bimp.!> y))


isTrivialSCC :: Graph -> Tree Vertex -> Bool
isTrivialSCC gr tree = ((length fl) == 1) && (elem (it, it) (edges gr)) where
  fl = flatten tree
  it = head fl


finalSCC :: (Ord a) => BuchiAutomaton a b -> [Set.Set a]
finalSCC b@(BuchiAutomaton _ _ fin _) = filter (\x -> not $ Set.disjoint x fin) stscc where
  stmap = createStateMap b
  graph = convBAtoGraph stmap b
  sccs = filter (not . isTrivialSCC graph) $ scc graph
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


--------------------------------------------------------------------------------------------------------------
-- Part with the rename, union function
--------------------------------------------------------------------------------------------------------------

renameBA :: (Ord a, Ord b) => Int -> BuchiAutomaton a b -> BuchiAutomaton Int b
renameBA x ba@(BuchiAutomaton st ini fin tr) = BuchiAutomaton (g st) (g ini) (g fin) tr' where
  stm = createStateMapFrom x ba
  rn x = stm Bimp.!> x
  g = Set.map (rn)
  tr' = Map.fromList $ map (\((x,y),z) -> ((rn x, y), g z)) $ Map.toList tr


unionBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b -> BuchiAutomaton a b
unionBA (BuchiAutomaton st1 ini1 fin1 tr1) (BuchiAutomaton st2 ini2 fin2 tr2) =
  BuchiAutomaton (Set.union st1 st2) (Set.union ini1 ini2) (Set.union fin1 fin2)
  (Map.unionWith (Set.union) tr1 tr2)


disjointUnionBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b -> BuchiAutomaton Int b
disjointUnionBA b1@(BuchiAutomaton st1 _ _ _) b2@(BuchiAutomaton st2 _ _ _) =
  unionBA rb1 rb2 where
    n = Set.size st1
    rb1 = renameBA 0 b1
    rb2 = renameBA n b2
