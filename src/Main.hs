
import ComplKV
import BuchiAutomaton
import RabitAutomataParser
import BuchiAutomataOper
import BuchiAutomataExp


main = do
  putStrLn "Complementation"

complBAKV filename = do
  aut <- parseFile filename
  let com = aut --complKV aut ["a", "b", "c"]
  --putStrLn $ show com
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (com) ["a", "b", "c"]
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (trimBA com) ["a", "b", "c"]
