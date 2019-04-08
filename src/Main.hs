{-|
  Module      : Main module
  Description : Complementation of Buchi automata
  Author      : Vojtech Havlena, February 2019
  License     : GPL-3
-}

import Data.Time
import System.Environment
import System.Directory
import Data.Set as Set

import ComplKV
import ComplSimKV
import ComplSchewe
import ComplSimSchewe
import BuchiAutomaton
import BuchiAutomataOper
import BuchiAutomataExp

import RabitRelation
import qualified RabitAutomataParser as RA
import qualified RabitRelationParser as RR


tmpFileSkeleton = "tempfa2d3e-ds1.ba"
defaultOutName = "out.ba"


data ProgArgs =
  Compl FilePath FilePath
  | Help
  | Error


parseArgs :: [String] -> ProgArgs
parseArgs args
  | (length args) == 1 && (last args) == "--help" = Help
  | (length args) == 1 = Compl (head args) defaultOutName
  | (length args) == 3 && (args !! 1) == "-o" = Compl (head args) (last args)
  | otherwise = Error


main = do
  args <- getArgs
  start <- getCurrentTime
  case (parseArgs args) of
    Compl autname outname -> do
      aut <- RA.parseFile autname
      relExt <- RR.rabitActionRel autname
      let rel = if checkConsitency relExt
                then getRabitRelation relExt
                else error "Inconsistent simulation relation"
      let compl = trimBA $ complSchewe (aut) $ Set.toList (alph aut)
          renOrig = renameBA 0 aut
          renCompl = renameBA 0 compl
      putStrLn $ show rel
      writeFile outname $ printBARabit $ compl
      putStrLn $ "States: " ++ (show $ Set.size $ states renCompl)
      res <- checkCorrectness renOrig renCompl
      putStrLn $ "Check: " ++ (show res)
      stop <- getCurrentTime
      putStrLn $ "Time: " ++ show (diffUTCTime stop start)



checkCorrectness :: (Ord a, Show a) => BuchiAutomaton a String -> BuchiAutomaton a String -> IO Bool
checkCorrectness aut1 aut2 = do
  let prod = isEmptyBA $ renameBA 0 $ intersectionBA aut1 aut2
      union = removeMultipleInitials 0 $ disjointUnionBA aut1 aut2
      alphabet = Set.union (alph aut1) (alph aut2)
      univ = universalBA alphabet
  writeFile tmpFileSkeleton $ printBARabit $ union
  writeFile (tmpFileSkeleton ++ "1") $ printBARabit $ univ
  incl1 <- RR.rabitActionIncl tmpFileSkeleton (tmpFileSkeleton ++ "1")
  incl2 <- RR.rabitActionIncl (tmpFileSkeleton ++ "1") tmpFileSkeleton
  removeFile tmpFileSkeleton
  removeFile (tmpFileSkeleton ++ "1")
  return $ incl1 && incl2 && prod


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
