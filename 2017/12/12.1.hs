import Data.List.Split;
import Data.Map;

countReachables v m vs =
  if elem v vs then 0
  else 1 + (fst (Prelude.foldr (\v' (a,vs') -> (a + countReachables v' m (v:vs'), (v:vs'))) (0,vs) (findWithDefault [] v m)))

main = do
  input <- getContents
  let ls = Prelude.map (\l -> splitOn " <-> " l) (lines input)
  --let vs = map (\l -> read (head l)) ls
  let es = Prelude.foldr (\l m -> insert (read (head l)) (Prelude.map read (splitOn ", " (head (tail l)))) m) empty ls :: Map Int [Int]
  let vs = keys es
  print (countReachables 0 es [])
