
module BuchiAutomaton (
  BuchiAutomaton(..)
  , Transitions
  , succTrans
  , succSet
  , unionBA
  , constrFromOrig
  , printDot
) where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AuxFunctions as Aux


type Transitions a b = Map.Map (a, b) (Set.Set a)
type Transition a b = ((a,b), (Set.Set a))

data BuchiAutomaton a b = BuchiAutomaton {
   states :: Set.Set a
   , initials :: Set.Set a
   , finals :: Set.Set a
   , transitions :: Transitions a b
}

instance (Show a, Show b) => Show (BuchiAutomaton a b) where
  show = printBA -- show $ length $ states b

printBA :: (Show a, Show b) => BuchiAutomaton a b -> String
printBA (BuchiAutomaton st ini fin trans) = "States: " ++ Aux.printSetCom st ++
  "\nInitials: " ++ Aux.printSetCom ini ++ "\nFinals: " ++ Aux.printSetCom fin ++
  "\nTransitions:\n" ++ (List.intercalate "\n" $ map (printTransition) $ Map.toList trans)


printTransition :: (Show a, Show b) => Transition a b -> String
printTransition ((from, sym), to) = "(" ++ (show from) ++ ", " ++ (show sym) ++
  ") -> {" ++ (Aux.printSetCom to) ++ "}"


printDot :: (Show a, Show b) => (a -> String) -> (b -> String) -> BuchiAutomaton a b -> String
printDot f g (BuchiAutomaton st ini fin trans) =
  "digraph finite_state_machine {\nrankdir=LR;\nsize=\"8,5\"" ++
  "node [shape = doublecircle]; " ++ Aux.printSetComF f fin ++ "\n" ++
  "node [shape = circle];\n" ++
  (List.intercalate "" $ map (printDotTransition f g) $ Map.toList trans) ++
  "}"


printDotTransition :: (Show a, Show b) => (a -> String) -> (b -> String) -> Transition a b -> String
printDotTransition f g ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = "\"" ++ (f from) ++ "\" -> \"" ++ (f to) ++
      "\" [ label = \"" ++ (g sym) ++ "\" ];"


succTrans :: (Ord a, Ord b) => a -> b -> Transitions a b -> [a]
succTrans q sym = Set.toList . Map.findWithDefault Set.empty (q,sym)


succSet :: (Ord a, Ord b) => Set.Set a -> b -> Transitions a b -> Set.Set a
succSet states sym tr = foldr (Set.union) Set.empty
  [Map.findWithDefault Set.empty (q,sym) tr | q <- Set.toList states]


unionBA :: (Ord a, Ord b) => BuchiAutomaton a b -> BuchiAutomaton a b
  -> BuchiAutomaton a b
unionBA (BuchiAutomaton s1 i1 f1 tr1) (BuchiAutomaton s2 i2 f2 tr2) =
    BuchiAutomaton (Set.union s1 s2) (Set.union i1 i2)
      (Set.union f1 f2) (Map.union tr1 tr2)


constrFromOrigProc :: (Ord a, Ord b, Ord c) => [c]
  -> BuchiAutomaton a b
  -> BuchiAutomaton c b
  -> [b]
  -> (BuchiAutomaton a b -> c -> b -> Set.Set c)
  -> (c -> Bool)
  -> BuchiAutomaton c b
constrFromOrigProc [] _ ba _ _ _ = ba
constrFromOrigProc (x:xs) orig b1@(BuchiAutomaton st1 _ _ _) alp suc isFin =
  constrFromOrigProc x' orig (unionBA b1 b2) alp suc isFin where
    tr' = Map.fromList [((x,sym), suc orig x sym) | sym <- alp]
    st' = foldr (Set.union) Set.empty $ Map.elems tr'
    ini' = Set.empty
    fin' = Set.filter (isFin) st'
    b2 = BuchiAutomaton st' ini' fin' tr'
    x' = xs ++ (Set.toList $ Set.difference st' st1)


constrFromOrig :: (Ord a, Ord b, Ord c) => BuchiAutomaton a b
  -> [b]
  -> (BuchiAutomaton a b -> c -> b -> Set.Set c)
  -> (Set.Set c)
  -> (c -> Bool)
  -> BuchiAutomaton c b
constrFromOrig orig alp suc ini isFin = constrFromOrigProc (Set.toList ini)
  orig (BuchiAutomaton ini ini (Set.filter (isFin) ini) Map.empty) alp suc isFin
