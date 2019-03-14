
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
  --putStrLn $ show $ renameBA 0 com
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (com) ["a", "b", "c"]
  putStrLn $ printBADot (printRabitStateKV) (id) $ complKV (trimBA com) ["a", "b", "c"]


prodBATest f1 f2 = do
  aut1 <- parseFile f1
  aut2 <- parseFile f2
  let pr = intersectionBA aut1 aut2
  putStrLn $ printBADot (printRabitStateProd) (id) pr
  putStrLn $ show $ isEmptyBA pr


partComplCheck a1 a2 = isEmptyBA $ intersectionBA a1 a2
