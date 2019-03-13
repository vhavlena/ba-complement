
module BuchiAutomataExp (
  printBA
  , printBADot
) where


import BuchiAutomaton
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified AuxFunctions as Aux


instance (Show a, Show b) => Show (BuchiAutomaton a b) where
  show = printBA -- show $ length $ states b


printBA :: (Show a, Show b) => BuchiAutomaton a b -> String
printBA (BuchiAutomaton st ini fin trans) = "States: " ++ Aux.printSetCom st ++
  "\nInitials: " ++ Aux.printSetCom ini ++ "\nFinals: " ++ Aux.printSetCom fin ++
  "\nTransitions:\n" ++ (List.intercalate "\n" $ map (printTransition) $ Map.toList trans)


printTransition :: (Show a, Show b) => Transition a b -> String
printTransition ((from, sym), to) = "(" ++ (show from) ++ ", " ++ (show sym) ++
  ") -> {" ++ (Aux.printSetCom to) ++ "}"


printBADot :: (Show a, Show b) => (a -> String) -> (b -> String) -> BuchiAutomaton a b -> String
printBADot f g (BuchiAutomaton st ini fin trans) =
  "digraph finite_state_machine {\nrankdir=LR;\n" ++
  "node [shape = point ]; " ++ Aux.printSetComF (qti . f) ini ++ "\n" ++
  "node [shape = doublecircle]; " ++ Aux.printSetComF (qt . f) fin ++ "\n" ++
  "node [shape = circle];\n" ++
  Aux.printSetF (qtitr . f) "" ini ++
  (List.intercalate "" $ map (printDotTransition f g) $ Map.toList trans) ++
  "}" where
    qt s = "\"" ++ s ++ "\""
    qti s = qt $ "q" ++ s
    qtitr s = qti s ++ " -> " ++ qt s ++ "\n"


printDotTransition :: (Show a, Show b) => (a -> String) -> (b -> String) -> Transition a b -> String
printDotTransition f g ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = "\"" ++ (f from) ++ "\" -> \"" ++ (f to) ++
      "\" [ label = \"" ++ (g sym) ++ "\" ];"
