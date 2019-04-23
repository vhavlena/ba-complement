{-|
  Module      : BuchiAutomataExp.hs
  Description : Buchi automata export
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

module BuchiAutomataExp (
  printBA
  , printBADot
  , printStateProd
  , printRabitStateProd
  , printBARabit
) where


import BuchiAutomaton
import BuchiAutomataOper
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


printStateProd :: (Show a, Show b) => StateProd a b -> String
printStateProd (q1,q2,True) = "(" ++ show q1 ++ "," ++ show q2 ++ ",1)"
printStateProd (q1,q2,False) = "(" ++ show q1 ++ "," ++ show q2 ++ ",0)"


printRabitStateProd :: StateProd String String -> String
printRabitStateProd (q1,q2,True) = "(" ++ q1 ++ "," ++ q2 ++ ",1)"
printRabitStateProd (q1,q2,False) = "(" ++ q1 ++ "," ++ q2 ++ ",0)"


printBARabit :: (Show a) => BuchiAutomaton a String -> String
printBARabit (BuchiAutomaton _ ini fin trans) = Aux.printSet "\n" ini ++ "\n" ++
  (List.intercalate "" $ map (printRabitTrans) $ Map.toList trans) ++
  Aux.printSet "\n" fin


printRabitTrans :: (Show a) => Transition a String -> String
printRabitTrans ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = sym ++ "," ++ (show from) ++ "->" ++ (show to)
