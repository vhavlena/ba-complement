{-|
  Module      : Main module
  Description : Complementation of Buchi automata
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

import Data.Time
import System.Environment
import Data.Set as Set

import ComplKV
import BuchiAutomaton
import BuchiAutomataOper
import BuchiAutomataExp

import RabitRelation
import qualified RabitAutomataParser as RA
import qualified RabitRelationParser as RR


data ProgArgs =
  Compl FilePath
  | Help
  | Error


parseArgs :: [String] -> ProgArgs
parseArgs args
  | (length args) == 1 && (last args) == "--help" = Help
  | (length args) == 1 = Compl (head args)
  | otherwise = Error


main = do
  args <- getArgs
  start <- getCurrentTime
  case (parseArgs args) of
    Compl autname -> do
      aut <- RA.parseFile autname
      relExt <- RR.rabitActionRel autname
      let rel = if checkConsitency relExt
                then getRabitRelation relExt
                else error "Inconsistent simulation relation"
          compl = complKV aut $ Set.toList (alph aut)
      putStrLn $ show $ renameBA 0 compl
      stop <- getCurrentTime
      putStrLn $ "Time: " ++ show (diffUTCTime stop start)


--------------------------------------------------------------------------------------------------------------
-- Part with the testing functions
--------------------------------------------------------------------------------------------------------------

complBAKV filename = do
  aut <- RA.parseFile filename
  let com = aut --complKV aut ["a", "b", "c"]
  --putStrLn $ show $ renameBA 0 com
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (com) ["a", "b", "c"]
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (trimBA com) ["a", "b", "c"]


prodBATest f1 f2 = do
  aut1 <- RA.parseFile f1
  aut2 <- RA.parseFile f2
  let pr = intersectionBA aut1 aut2
  putStrLn $ printBADot (printRabitStateProd) (id) pr
  putStrLn $ show $ isEmptyBA pr


partComplCheck a1 a2 = isEmptyBA $ intersectionBA a1 a2
