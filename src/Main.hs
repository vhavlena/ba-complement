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

import Simulation
import ComplKV
import ComplSimKV
import ComplSchewe
import ComplSimSchewe
import BuchiAutomaton
import BuchiAutomataOper
import BuchiAutomataExp
import ComplConfig

import RabitRelation
import qualified RabitAutomataParser as RA
import qualified RabitRelationParser as RR


data Algorithm =
  Schewe
  | ScheweSim
  | ScheweSimSat
  | ScheweSimRem
  deriving (Eq)


data Quotient =
  QuotDirect
  | QuotDelayed
  | QuotNone
  deriving (Eq)


data ExportFormat =
  Goal
  | Ba
  | Graphviz
  deriving (Eq)


data ProgArgs =
  Compl Quotient Algorithm FilePath FilePath
  | Export Quotient ExportFormat FilePath FilePath
  | Help
  | Error


parseArgs :: [String] -> ProgArgs
parseArgs args
  | (length args) == 1 && (last args) == "--help" = Help
  | (length args) > 1 && (args !! 0) == "--schewe" = parseArgsAlg Schewe $ tail args
  | (length args) > 1 && (args !! 0) == "--schewesim" = parseArgsAlg ScheweSim $ tail args
  | (length args) > 1 && (args !! 0) == "--schewesimsat" = parseArgsAlg ScheweSimSat $ tail args
  | (length args) > 1 && (args !! 0) == "--schewesimrem" = parseArgsAlg ScheweSimRem $ tail args
  | (length args) > 1 && (args !! 0) == "--goal" = parseArgsExport Goal $ tail args
  | (length args) > 1 && (args !! 0) == "--ba" = parseArgsExport Ba $ tail args
  | otherwise = Error


algSimVar :: Algorithm -> SimAlg
algSimVar ScheweSim = Combination
algSimVar ScheweSimSat = Saturation
algSimVar ScheweSimRem = Removing
algSimVar _ = None


parseArgsAlg :: Algorithm -> [String] -> ProgArgs
parseArgsAlg alg args
  | (length args) >= 3 && (args !! 1) == "-o" = Compl quotient alg (head args) (args !! 2)
  | (length args) >= 1 = Compl quotient alg (head args) defaultOutName
  | otherwise = Error
  where
    quotient
      | (last args) == "-qdir" = QuotDirect
      | (last args) == "-qdel" = QuotDelayed
      | otherwise = QuotNone


parseArgsExport :: ExportFormat -> [String] -> ProgArgs
parseArgsExport format args
  | (length args) >= 3 && (args !! 1) == "-o" = Export quotient format (head args) (args !! 2)
  | (length args) >= 1 = Export quotient format (head args) defaultOutNameGoal
  | otherwise = Error
  where
    quotient
      | (last args) == "-qdir" = QuotDirect
      | (last args) == "-qdel" = QuotDelayed
      | otherwise = QuotNone


getConsRel :: RR.RabitRelationExt -> RabitRelation
getConsRel rel =
  if checkConsitency rel then getRabitRelation rel
  else error "Inconsistent simulation relation"


parseItems :: FilePath -> IO (RA.RabitBuchiAutomaton, Simulation String, Simulation String)
parseItems autname = do
  autParExp <- RA.parseFile autname
  relExt <- RR.rabitActionRel autname "-delsim"
  dirExt <- RR.rabitActionRel autname "-dirsim"
  let reldel = Delayed $ getConsRel relExt
      reldir = Direct $ getConsRel dirExt
  return (autParExp, reldel, reldir)


main = do
  args <- getArgs
  start <- getCurrentTime
  case (parseArgs args) of
    Compl quotient alg autname outname -> do
      (aut1, del1, dir1) <- parseItems autname
      let autExp
            | quotient == QuotDirect = renameBA 0 $ quotientSimBA aut1 dir1
            | quotient == QuotDelayed = renameBA 0 $ quotientSimBA aut1 del1
            | otherwise = renameBA 0 aut1
      writeFile tmpFileSimulation $ printBARabitF (RA.rabitState) $ autExp
      (aut2, reldel, reldir) <- parseItems tmpFileSimulation
      removeFile tmpFileSimulation

      let var = algSimVar alg
          rel = [rabitSimToInt reldel, rabitSimToInt reldir]
          aut = RA.rabitBAtoIntBA aut2
      let compl = if alg == Schewe then trimBA $ complSchewe (aut) $ Set.toList (alph aut)
                  else trimBA $ complSimSchewe (aut) rel remOpt (Set.toList $ alph aut) var
          renOrig = aut
          renCompl = renameBA 0 compl
      --putStrLn $ "Delayed simulation: " ++ (show $ 0)
      writeFile outname $ printBARabit $ renCompl
      putStrLn $ "States: " ++ (show $ Set.size $ states renCompl)

      if cfCheckCorrectness then do
        res <- checkCorrectness renOrig renCompl
        putStrLn $ "Check: " ++ (show res)
      else
        return ()

      stop <- getCurrentTime
      putStrLn $ "Time: " ++ show (diffUTCTime stop start)
    Export quotient format autname outname -> do
      (aut, del1, dir1) <- parseItems autname

      let baInt
            | quotient == QuotDirect = renameBA 0 $ quotientSimBA aut dir1
            | quotient == QuotDelayed = renameBA 0 $ quotientSimBA aut del1
            | otherwise = renameBA 0 aut

      let expf = if format == Goal then printBAGoalF (show) (id) else printBA
      writeFile outname $ expf baInt
    Error -> do
      putStrLn "Bad input parameters."



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

quotientTest autname = do
  aut <- RA.parseFile autname
  relExt <- RR.rabitActionRel autname "-delsim"
  dirExt <- RR.rabitActionRel autname "-dirsim"
  let reldel = Delayed $ getConsRel relExt
      reldir = Direct $ getConsRel dirExt
  putStrLn $ show reldel
  putStrLn $ show $ quotientSimBA aut reldel


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
