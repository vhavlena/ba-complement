
module BuchiAutomaton (
  BuchiAutomaton(..)
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
  show = printBA


printBA :: (Show a, Show b) => BuchiAutomaton a b -> String
printBA (BuchiAutomaton st ini fin trans) = "States: " ++ Aux.printSetCom st ++
  "\nInitials: " ++ Aux.printSetCom ini ++ "\nFinals: " ++ Aux.printSetCom fin ++
  "\nTransitions:\n" ++ (List.intercalate "\n" $ map (printTransition) $ Map.toList trans)


printTransition :: (Show a, Show b) => Transition a b -> String
printTransition ((from, sym), to) = "(" ++ (show from) ++ ", " ++ (show sym) ++
  ") -> {" ++ (Aux.printSetCom to) ++ "}"
