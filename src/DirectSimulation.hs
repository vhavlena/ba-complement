
module DirectSimulation where

import BuchiAutomaton
import SimulationAux
import qualified RabitAutomataParser as RA
import qualified Data.Set.Monad as Set
import qualified Data.Map as Mp
import qualified Debug.Trace as Dbg

type DetStateMap b = Mp.Map b (Set.Set b)
type LabelStateMap a b = Mp.Map a (DetStateMap b)

data SimRemMap b = SimRemMap {
  removeMap :: DetStateMap b
  , simMap :: DetStateMap b
} deriving (Show)


nullRemoveState :: (Ord b) =>
  SimRemMap b
  -> b
  ->  SimRemMap b
nullRemoveState (SimRemMap rm sm) v = SimRemMap (Mp.insert v Set.empty rm) sm


updateRemoveState :: (Ord b) =>
  SimRemMap b
  -> b
  -> Set.Set b
  -> SimRemMap b
updateRemoveState (SimRemMap rm sm) v val = SimRemMap (Mp.insert v val rm) sm


updateValState :: (Ord b) =>
  SimRemMap b
  -> b
  -> Set.Set b
  -> Set.Set b
  -> SimRemMap b
updateValState (SimRemMap rm sm) u rmval smval = SimRemMap (Mp.insert u rmval rm) (Mp.insert u smval sm)


removeUpd :: (Ord b) =>
  DetStateMap b
  -> DetStateMap b
  -> SimRemMap b
  -> b
  -> b
  -> SimRemMap b
removeUpd pre post st@(SimRemMap rm sm) u w =
  if Set.member w (sm Mp.! u) then updateValState st u smu rmu
    else st
    where
      smu = Set.difference (sm Mp.! u) (Set.singleton w)
      rmu = Set.union (rm Mp.! u) $ flat
      flat = do
        w' <- pre Mp.! w
        if disjoint (post Mp.! w') smu then
          Set.singleton w'
          else Set.empty


removeIter :: (Ord b) =>
  DetStateMap b
  -> DetStateMap b
  -> SimRemMap b
  -> b
  -> b
  -> SimRemMap b
removeIter pre post st@(SimRemMap rm sm) u v =
  Set.foldr (fnc pre post u) st (rm Mp.! v) where
    fnc pre post u w st' = removeUpd pre post st' u w


setRemoveLoop :: (Ord b, Show b) =>
  DetStateMap b
  -> DetStateMap b
  -> SimRemMap b
  -> b
  -> b
  -> Set.Set b
  -> SimRemMap b
setRemoveLoop pre post sstate u v remvall =
  if Set.isSubsetOf (remove' Mp.! v) remvall then rt
    else setRemoveLoop pre post rt' u v (Set.union remvall (remove' Mp.! v))
  where
    rt@(SimRemMap remove' _) = removeIter pre post sstate u v
    rt' = updateRemoveState rt v $ Set.difference (remove' Mp.! v) remvall


simIter :: (Ord b, Show b) =>
  DetStateMap b
  -> DetStateMap b
  -> SimRemMap b
  -> b
  -> SimRemMap b
simIter pre post simstate v =
  Set.foldr (fnc pre post) simstate (Mp.findWithDefault Set.empty v pre) where
    fnc pre post u st@(SimRemMap rm _) = setRemoveLoop pre post st u v (rm Mp.! v)


mainLoop :: (Ord b, Show b) =>
  DetStateMap b
  -> DetStateMap b
  -> SimRemMap b
  -> SimRemMap b
mainLoop pre post simstate =
  case getState simstate of
    Nothing -> simstate
    Just v -> mainLoop pre post $ nullRemoveState simstate' v where
      simstate' = simIter pre post simstate v


computeSimulationWrap :: (Ord a, Ord b, Show b, Show a) =>
  Set.Set a
  -> LabelStateMap a b
  -> LabelStateMap a b
  -> LabelStateMap a b
  -> DetStateMap b
  -> DetStateMap b
computeSimulationWrap alph premap postmap removemap sim =
  Set.foldr (fnc premap postmap removemap) sim alph where
    fnc premap postmap removemap sym sim = simMap $ mainLoop (premap Mp.! sym) (postmap Mp.! sym)
      $ SimRemMap (removemap Mp.! sym) sim


getState :: (Ord b) =>
  SimRemMap b
  -> Maybe b
getState simstate = if null flt then Nothing else Just $ flt !! 0 where
  flt = Mp.keys $ Mp.filter (not . Set.null) $ removeMap simstate


initSimuWrap :: (Ord a, Ord b) =>
  BuchiAutomaton b a
  -> DetStateMap b
  -> DetStateMap b
initSimuWrap ba@(BuchiAutomaton st _ fin _) post = mapFromSet $ do
  state <- st
  return (state, labelledStates ba state) where
    labelledStates (BuchiAutomaton sts _ f _) s =
      if val then Set.filter (\x -> not $ null $ Mp.findWithDefault Set.empty x post) sset else sset where
        val = not $ null $ Mp.findWithDefault Set.empty s post
        sset = if Set.member s f then f else Set.difference sts f


initSimu :: (Ord a, Ord b) =>
  BuchiAutomaton b a
  -> Set.Set a
  -> LabelStateMap a b
  -> DetStateMap b
initSimu ba@(BuchiAutomaton st _ fin _) alph postmap = Set.foldr (Mp.unionWith (Set.intersection)) Mp.empty $
  do
    a <- alph
    return $ initSimuWrap ba (postmap Mp.! a)


initRem :: (Ord a, Ord b) =>
  BuchiAutomaton b a
  -> Set.Set a
  -> LabelStateMap a b
  -> DetStateMap b
  -> LabelStateMap a b
initRem ba@(BuchiAutomaton st _ _ _) alph premap sim =
  mapFromSet $ Set.map (fnc sim) alph where
    fnc sim sym = (sym, initRemIter ba (premap Mp.! sym) sim)


initRemIter :: (Ord a, Ord b) =>
  BuchiAutomaton b a
  -> DetStateMap b
  -> DetStateMap b
  -> DetStateMap b
initRemIter ba@(BuchiAutomaton st _ _ _) pre sim = mapFromSet $ do
  state <- st
  return (state, Set.difference (preSet pre st) (preSet pre (sim Mp.! state))) where


initSimulation :: (Ord a, Ord b, Show a, Show b) =>
  BuchiAutomaton b a
  -> LabelStateMap a b
  -> LabelStateMap a b
  -> Set.Set a
  -> (LabelStateMap a b, DetStateMap b)
-- initSimulation ba@(BuchiAutomaton st _ _ tr) premap postmap alph | Dbg.trace ("init: " ++ show (initSimu ba alph postmap)) False = undefined
initSimulation ba@(BuchiAutomaton st _ _ tr) premap postmap alph = (remmap, sim) where
  sim = initSimu ba alph postmap
  remmap = initRem ba alph premap sim


computeSimulation :: (Ord a, Ord b, Show b, Show a) =>
  BuchiAutomaton b a
  -> Set.Set a
  -> DetStateMap b
computeSimulation ba alph = computeSimulationWrap alph premap postmap removemap sim where
  premap = preMap ba alph
  postmap = postMap ba alph
  (removemap, sim) = initSimulation ba premap postmap alph


preSet mapping set = do
  s <- set
  Mp.findWithDefault (Set.empty) s mapping


testAut1 filename = do
  aut <- RA.parseFile filename
  putStrLn $ show $ computeSimulation aut $ alph aut
