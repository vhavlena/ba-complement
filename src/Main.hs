
import ComplKV
import BuchiAutomaton
import RabitAutomataParser


main = do
  putStrLn "Complementation"

complBAKV filename = do
  aut <- parseFile filename
  putStrLn $ show $ complKV aut ["a", "b", "c"]
