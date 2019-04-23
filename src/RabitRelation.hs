{-|
  Module      : RabitRelation
  Description : Rabit simulation relation functions
  Author      : Vojtech Havlena, March 2019
  License     : GPL-3
-}

module RabitRelation (
  checkConsitency
  , getRabitRelation
  , RabitRelation
) where

import RabitRelationParser
import RabitAutomataParser
import qualified Data.Set as Set

type RabitRelation = Set.Set (RabitState, RabitState)


checkConsitency :: RabitRelationExt -> Bool
checkConsitency rel = checkConsitencyAux rel rel


checkConsitencyAux :: RabitRelationExt -> RabitRelationExt -> Bool
checkConsitencyAux [] _ = True
checkConsitencyAux ((st1,st2,aut1,aut2):xs) arr = ((st1,st2,aut1,aut1) `elem` arr) &&
  ((st1,st2,aut2,aut2) `elem` arr) && ((st1,st2,aut2,aut1) `elem` arr) &&
  checkConsitencyAux xs arr


getRabitRelation :: RabitRelationExt -> RabitRelation
getRabitRelation = Set.fromList . map (\(x,y,z,r) -> (x,y))
