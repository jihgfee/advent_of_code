import Data.List.Split;
import Data.Map;

reachables v m vs =
  if elem v vs then vs
  else Prelude.foldr (\v' vs' -> reachables v' m vs') (v:vs) (findWithDefault [] v m)


countGroups [] vs es = 0
countGroups (v:vs) vs' es =
  if elem v vs' then countGroups vs vs' es
  else 1 + (countGroups vs (vs' ++ reachables v es []) es)
 

main = do
  input <- getContents
  let ls = Prelude.map (\l -> splitOn " <-> " l) (lines input)
  --let vs = map (\l -> read (head l)) ls
  let es = Prelude.foldr (\l m -> insert (read (head l)) (Prelude.map read (splitOn ", " (head (tail l)))) m) empty ls :: Map Int [Int]
  let vs = keys es
  print (countGroups vs [] es)
