
module BuchiAutomataOper (
  transposeBA
  , constrFromOrig
  , restrictBA
  , trimBA
  , renameBA
  , unionBA
  , disjointUnionBA
  , StateProd
  , intersectionBA
  , isEmptyBA
) where


import qualified Data.Map as Map
import qualified Data.Bimap as Bimp
import qualified Data.Set as Set
import Data.Graph
import Data.Tree
import BuchiAutomaton

type StateMap a = Bimp.Bimap Int a
type StateProd a b = (a, b, Bool)

--------------------------------------------------------------------------------------------------------------
-- Part with the fixpoint contruction
--------------------------------------------------------------------------------------------------------------

constrFromOrigProc :: (Ord a, Ord b) => [a]
  -> BuchiAutomaton a b
  -> [b]
  -> (a -> b -> Set.Set a)
  -> (a -> Bool)
  -> BuchiAutomaton a b
constrFromOrigProc [] ba _ _ _ = ba
constrFromOrigProc (x:xs) b1@(BuchiAutomaton st1 _ _ _) alp suc isFin =
  constrFromOrigProc x' (unionBA b1 b2) alp suc isFin where
    tr' = Map.fromList [((x,sym), suc x sym) | sym <- alp]
    st' = foldr (Set.union) Set.empty $ Map.elems tr'
    ini' = Set.empty
    fin' = Set.filter (isFin) st'
    b2 = BuchiAutomaton st' ini' fin' tr'
    x' = xs ++ (Set.toList $ Set.difference st' st1)


constrFromOrig :: (Ord a, Ord b) => [b]
  -> (a -> b -> Set.Set a)
  -> (Set.Set a)
  -> (a -> Bool)
  -> BuchiAutomaton a b
constrFromOrig alp suc ini isFin = constrFromOrigProc (Set.toList ini)
  (BuchiAutomaton ini ini (Set.filter (isFin) ini) Map.empty) alp suc isFin


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
isTrivialSCC gr tree = ((length fl) <= 1) && (not $ elem (it, it) (edges gr)) where
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
-- Part with the rename, union, and emptiness checking function
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


isEmptyBA :: (Ord a, Ord b) => BuchiAutomaton a b -> Bool
isEmptyBA = Set.null . initials . trimBA


--------------------------------------------------------------------------------------------------------------
-- Part with the intersection function
--------------------------------------------------------------------------------------------------------------

succProd :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b -> BuchiAutomaton c b -> StateProd a c -> b
  -> Set.Set (StateProd a c)
succProd (BuchiAutomaton _ _ fin1 tr1) (BuchiAutomaton _ _ fin2 tr2) (q1, q2, n) sym
  | n == True = retSet i
  | otherwise = retSet i' where
    fsucc = succTrans q1 sym tr1
    ssucc = succTrans q2 sym tr2
    retSet i = Set.map (\(x,y) -> (x,y,i)) $ Set.cartesianProduct fsucc ssucc
    i = if Set.member q1 fin1 then False else True
    i' = if Set.member q2 fin2 then True else False


isFinProd :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b -> BuchiAutomaton c b -> StateProd a c -> Bool
isFinProd _ (BuchiAutomaton _ _ fin _) (_, q2, False) = Set.member q2 fin
isFinProd _ _ _ = False


iniProd :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b
  -> BuchiAutomaton c b
  -> Set.Set (StateProd a c)
iniProd (BuchiAutomaton _ ini1 _ _) (BuchiAutomaton _ ini2 _ _) =
    Set.map (\(x,y) -> (x,y,True)) $ Set.cartesianProduct ini1 ini2


intersectionBA :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b -> BuchiAutomaton c b -> BuchiAutomaton (StateProd a c) b
intersectionBA ba1 ba2 = constrFromOrig alp (succProd ba1 ba2) (iniProd ba1 ba2) (isFinProd ba1 ba2) where
  alp = Set.toList $ Set.union (alph ba1) (alph ba2)
