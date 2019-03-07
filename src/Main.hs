
import ComplKV
import BuchiAutomaton
import RabitAutomataParser
import BuchiAutomataOper


main = do
  putStrLn "Complementation"

complBAKV filename = do
  aut <- parseFile filename
  let com = aut --complKV aut ["a", "b", "c"]
  putStrLn $ printDot (printRabitStateKV) (id) $ complKV (com) ["a", "b", "c"]
  putStrLn $ printDot (printRabitStateKV) (id) $ complKV (trimBA $ com) ["a", "b", "c"]
