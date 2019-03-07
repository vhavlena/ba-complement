
import ComplKV
import BuchiAutomaton
import RabitAutomataParser
import BuchiAutomataOper


main = do
  putStrLn "Complementation"

complBAKV filename = do
  aut <- parseFile filename
  let com = complKV aut ["a", "b", "c"]
  putStrLn $ show $ com
  putStrLn $ show $ trimBA $ com
