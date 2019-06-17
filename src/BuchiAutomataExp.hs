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
  , printBAGoal
) where


import BuchiAutomaton
import BuchiAutomataOper
import qualified Data.List as List
import qualified Data.List.Index as List.Ind
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


--------------------------------------------------------------------------------------------------------------
-- Graphviz export
--------------------------------------------------------------------------------------------------------------

printBADot :: (Show a, Show b) => (a -> String) -> (b -> String) -> BuchiAutomaton a b -> String
printBADot f g (BuchiAutomaton st ini fin trans) =
  "digraph finite_state_machine {\nrankdir=LR;\n" ++
  "node [shape = point ]; " ++ Aux.printSetComF (qti . f) ini ++ "\n" ++
  "node [shape = doublecircle]; " ++ Aux.printSetComF (qt . f) fin ++ "\n" ++
  "node [shape = circle];\n" ++
  Aux.printSetF (qtitr . f) "" ini ++
  (List.intercalate "" $ map (printTransitionDot f g) $ Map.toList trans) ++
  "}" where
    qt s = "\"" ++ s ++ "\""
    qti s = qt $ "q" ++ s
    qtitr s = qti s ++ " -> " ++ qt s ++ "\n"


printTransitionDot :: (Show a, Show b) => (a -> String) -> (b -> String) -> Transition a b -> String
printTransitionDot f g ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = "\"" ++ (f from) ++ "\" -> \"" ++ (f to) ++
      "\" [ label = \"" ++ (g sym) ++ "\" ];"


printStateProd :: (Show a, Show b) => StateProd a b -> String
printStateProd (q1,q2,True) = "(" ++ show q1 ++ "," ++ show q2 ++ ",1)"
printStateProd (q1,q2,False) = "(" ++ show q1 ++ "," ++ show q2 ++ ",0)"


--------------------------------------------------------------------------------------------------------------
-- Rabit export
--------------------------------------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------------------------------------
-- Goal export
--------------------------------------------------------------------------------------------------------------

printBAGoal :: (Show a, Show b, Ord b) => BuchiAutomaton a b -> String
printBAGoal b@(BuchiAutomaton st ini fin trans) =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
  "<Structure label-on=\"Transition\" type=\"FiniteStateAutomaton\">\n" ++
  "<Alphabet type=\"Propositional\">\n" ++
  (Aux.printSetF (printLetterGoal) "\n" (alph b)) ++
  "\n</Alphabet>\n<StateSet>\n" ++
  (Aux.printSetF (printStateGoal) "\n" st) ++
  "\n</StateSet>\n<InitialStateSet>\n" ++
  (Aux.printSetF (printStateRefGoal) "\n" ini) ++
  "\n</InitialStateSet>\n<TransitionSet complete=\"false\">\n" ++
  (Aux.printListF (printIndTransitionGoal) "\n" trList) ++
  "\n</TransitionSet>\n<Acc type=\"Buchi\">\n" ++
  (Aux.printSetF (printStateRefGoal) "\n" fin) ++
  "\n</Acc>\n<Properties/>\n</Structure>"
  where
    trList = List.Ind.indexed $ (Map.toList trans) >>= transToArrowList


printStateGoal :: (Show a) => a -> String
printStateGoal st = "<State sid=\"" ++ (show st) ++ "\" />"


printLetterGoal :: (Show b) => b -> String
printLetterGoal lt = "<Proposition>" ++ (show lt) ++ "</Proposition>"


printStateRefGoal :: (Show a) => a -> String
printStateRefGoal st = "<StateID>" ++ (show st) ++ "</StateID>"


printIndTransitionGoal :: (Show a, Show b) => (Int, TransitionArrow a b) -> String
printIndTransitionGoal (index, (from, sym, to)) =
  "<Transition tid=\"" ++ (show index) ++ "\">\n" ++
  "\t<From>" ++ (show from) ++ "</From>\n" ++
  "\t<To>" ++ (show to) ++ "</To>\n" ++
  "\t<Label>" ++ (show sym) ++ "</Label>\n" ++
  "</Transition>"
