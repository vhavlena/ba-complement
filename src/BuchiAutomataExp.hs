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
  , printBARabitF
  , printBAGoal
  , printBAGoalF
) where


import BuchiAutomaton
import BuchiAutomataOper
import qualified Data.List as List
import qualified Data.List.Index as List.Ind
import qualified Data.Map as Map
import qualified Data.Set.Monad as Set
import qualified AuxFunctions as Aux


instance (Ord a, Ord b, Show a, Show b) => Show (BuchiAutomaton a b) where
  show = printBA -- show $ length $ states b


printBA :: (Ord a, Ord b, Show a, Show b) => BuchiAutomaton a b -> String
printBA (BuchiAutomaton st ini fin trans) = "States: " ++ Aux.printSetCom st ++
  "\nInitials: " ++ Aux.printSetCom ini ++ "\nFinals: " ++ Aux.printSetCom fin ++
  "\nTransitions:\n" ++ (List.intercalate "\n" $ map (printTransition) $ Map.toList trans)


printTransition :: (Ord a, Ord b, Show a, Show b) => Transition a b -> String
printTransition ((from, sym), to) = "(" ++ (show from) ++ ", " ++ (show sym) ++
  ") -> {" ++ (Aux.printSetCom to) ++ "}"


--------------------------------------------------------------------------------------------------------------
-- Graphviz export
--------------------------------------------------------------------------------------------------------------

printBADot :: (Ord a, Ord b, Show a, Show b) => (a -> String) -> (b -> String) -> BuchiAutomaton a b -> String
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


printTransitionDot :: (Ord a, Ord b, Show a, Show b) => (a -> String) -> (b -> String) -> Transition a b -> String
printTransitionDot f g ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = "\"" ++ (f from) ++ "\" -> \"" ++ (f to) ++
      "\" [ label = \"" ++ (g sym) ++ "\" ];"


printStateProd :: (Ord a, Ord b, Show a, Show b) => StateProd a b -> String
printStateProd (q1,q2,True) = "(" ++ show q1 ++ "," ++ show q2 ++ ",1)"
printStateProd (q1,q2,False) = "(" ++ show q1 ++ "," ++ show q2 ++ ",0)"


--------------------------------------------------------------------------------------------------------------
-- Rabit export
--------------------------------------------------------------------------------------------------------------

printRabitStateProd :: StateProd String String -> String
printRabitStateProd (q1,q2,True) = "(" ++ q1 ++ "," ++ q2 ++ ",1)"
printRabitStateProd (q1,q2,False) = "(" ++ q1 ++ "," ++ q2 ++ ",0)"


printBARabit :: (Ord a, Show a) => BuchiAutomaton a String -> String
printBARabit (BuchiAutomaton _ ini fin trans) = Aux.printSet "\n" ini ++ "\n" ++
  (List.intercalate "" $ map (printRabitTrans) $ Map.toList trans) ++
  Aux.printSet "\n" fin


printBARabitF :: (Ord a, Show a) => (a -> String) -> BuchiAutomaton a String -> String
printBARabitF f (BuchiAutomaton _ ini fin trans) = Aux.printSetF f "\n" ini ++ "\n" ++
  (List.intercalate "" $ map (printRabitTransF f) $ Map.toList trans) ++
  Aux.printSetF f "\n" fin


printRabitTrans :: (Ord a, Show a) => Transition a String -> String
printRabitTrans = printRabitTransF (show)


printRabitTransF :: (Ord a) => (a -> String) -> Transition a String -> String
printRabitTransF f ((from, sym), to) =
  unlines $ map (printEdge from sym) $ Set.toList to where
    printEdge from sym to = sym ++ "," ++ (f from) ++ "->" ++ (f to)


--------------------------------------------------------------------------------------------------------------
-- Goal export
--------------------------------------------------------------------------------------------------------------

printBAGoalF :: (Ord a, Ord b) => (a -> String)
  -> (b -> String)
  -> BuchiAutomaton a b
  -> String
printBAGoalF f g b@(BuchiAutomaton st ini fin trans) =
  "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++
  "<Structure label-on=\"Transition\" type=\"FiniteStateAutomaton\">\n" ++
  "<Alphabet type=\"Classical\">\n" ++
  (Aux.printSetF (printLetterGoal g) "\n" (alph b)) ++
  "\n</Alphabet>\n<StateSet>\n" ++
  (Aux.printSetF (printStateGoal f) "\n" st) ++
  "\n</StateSet>\n<InitialStateSet>\n" ++
  (Aux.printSetF (printStateRefGoal f) "\n" ini) ++
  "\n</InitialStateSet>\n<TransitionSet complete=\"false\">\n" ++
  (Aux.printListF (printIndTransitionGoal f g) "\n" trList) ++
  "\n</TransitionSet>\n<Acc type=\"Buchi\">\n" ++
  (Aux.printSetF (printStateRefGoal f) "\n" fin) ++
  "\n</Acc>\n<Properties/>\n</Structure>"
  where
    trList = List.Ind.indexed $ (Map.toList trans) >>= transToArrowList


printBAGoal :: (Show a, Show b, Ord a, Ord b) => BuchiAutomaton a b -> String
printBAGoal = printBAGoalF (show) (show)


printStateGoal :: (a -> String) -> a -> String
printStateGoal f st = "<State sid=\"" ++ (f st) ++ "\" />"


printLetterGoal :: (b -> String) -> b -> String
printLetterGoal g lt = "<Symbol>" ++ (g lt) ++ "</Symbol>"


printStateRefGoal :: (a -> String) -> a -> String
printStateRefGoal f st = "<StateID>" ++ (f st) ++ "</StateID>"


printIndTransitionGoal :: (a -> String)
  -> (b -> String)
  -> (Int, TransitionArrow a b)
  -> String
printIndTransitionGoal f g (index, (from, sym, to)) =
  "<Transition tid=\"" ++ (show index) ++ "\">\n" ++
  "\t<From>" ++ (f from) ++ "</From>\n" ++
  "\t<To>" ++ (f to) ++ "</To>\n" ++
  "\t<Label>" ++ (g sym) ++ "</Label>\n" ++
  "</Transition>"
